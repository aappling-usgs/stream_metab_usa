
#' helper function for staging different data types from the config
#' 
#' @param target_name a var_src (e.g., baro_nldas or doobs_nwis)
#' @param ts.config a config list used to parameterize calls to stage_nwis_ts or stage_nldas_ts
#' @return files that were created
stage_ts <- function(ts.file){
  
  auth_internal()
  
  ts.table <- read.table(file=ts.file, sep='\t', header = TRUE, stringsAsFactors = FALSE)
  dir.name <- unique(dirname(ts.table$filepath))
  lapply(dir.name, function(x) if(!dir.exists(x)) dir.create(x))

  # // check local files
  ts.table$local <- file.exists(ts.table$filepath)
  # targets will tell us what the function to call is
  src <- tail(strsplit(ts.file,'[_.]')[[1]],2)[1]
  
  if (src == 'nldas'){
    ts.config <- yaml.load_file("configs/nldas_ts.yml")
    gconfig(sleep.time=60, retries=2)
    use.i <- !ts.table$local | ts.table$no.data
    files <- ts.table$filepath[use.i]
    message('data pulls for ',length(ts.table$filepath),' sites')
    message(length(ts.table$filepath[ts.table$local]),' already exist, ', length(files), ' will be new')
    details <- parse_ts_path(files, out=c('site_name','version','var',"dir_name"))
    times <- c(unique(ts.table$time.st[use.i]), unique(ts.table$time.en[use.i]))
    files = stage_nldas_ts(sites=details$site_name, var=unique(details$var), times = times, 
                           version=unique(details$version), folder=unique(details$dir_name), url = ts.config$nldas_url, verbose=TRUE)
    ts.table$local[use.i] = file.exists(files)
  } else if (src == 'nwis'){
    #chunk sites
    message('data pulls for ',length(ts.table$filepath),' sites')
    files <- ts.table$filepath[!ts.table$local | ts.table$no.data]
    message(length(ts.table$filepath[ts.table$local]),' already exist or have no data, ', length(files), ' will be new')
    for (file in files){

      file.i <- which(file == ts.table$filepath)
      
      details <- parse_ts_path(file)
      # // catch warning here
      local.file <- tryCatch(
        stage_nwis_ts(details$site_name, details$var, times=c(ts.table$time.st[file.i], ts.table$time.en[file.i]), 
                      version=details$version, folder=details$dir_name, verbose=TRUE),
        warning=function(w) {
          if (w$message == "error in data"){
            return(FALSE)
          } else {
            return(NULL)
          }
        })
      if (is.null(local.file)){
        # no data in this site
        ts.table$no.data[file.i] <- TRUE
      } else if (is.character(local.file) && file.exists(local.file)){
        ts.table$local[file.i] <- TRUE
      } else {
        message('seems to be an error for this site')
      }
    }
  }
  write_site_table(ts.table, ts.file)
}

#' helper function for posting files to sciencebase w/ post_ts
#' 
#' @param files files created by stage_ts (or stage_nwis_ts; stage_nldas_ts)
sb_post_ts <- function(ts.file){
  
  ts.config <- yaml.load_file("configs/nldas_ts.yml")
  auth_internal()
  
  remote.sites <- list_sites('doobs_nwis', with_ts_version='rds')
  ts.table <- read.table(file=ts.file, sep='\t', header = TRUE, stringsAsFactors = FALSE)
  details <- parse_ts_path(ts.table$filepath, out=c('site_name', 'var_src','version'))
  remote.sites <- list_sites(unique(details$var_src), with_ts_version=unique(details$version))
  remote <- ts.table$remote | details$site_name %in% remote.sites
  ts.table$remote <- remote
  
  files <- ts.table$filepath[ts.table$local & !ts.table$remote]
  
  for (file in files){
    locate_site(site, by = 'tag')
    if(is.na(locate_site(details$site, by = 'tag'))){
      post_site(site, on_exists = "skip", verbose=TRUE)
    }
    sb.id <- post_ts(file, on_exists=ts.config$on_exists, verbose=TRUE)
    if (is.character(sb.id) & nchar(sb.id) > 0){
      file.i <- which(file == ts.table$filepath)
      ts.table$remote[file.i] <- TRUE
      write_site_table(ts.table, ts.file)
    }
  }
  remote.sites <- parse_ts_path(ts.table$filepath[ts.table$remote], out='site_name', use_names = FALSE)
  return(remote.sites)
}