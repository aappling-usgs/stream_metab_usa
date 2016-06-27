#' helper function for staging different data types from the config
#' 
#' @param target_name a var_src (e.g., baro_nldas or doobs_nwis)
#' @param ts.config a config list used to parameterize calls to stage_nwis_ts or stage_nldas_ts
#' @return files that were created
stage_ts <- function(ts.file){
  
  auth_from_profile()
  
  ts.table <- read.table(file=ts.file, sep='\t', header = TRUE, stringsAsFactors = FALSE)
  dir.name <- unique(dirname(ts.table$filepath))
  lapply(dir.name, function(x) if(!dir.exists(x)) dir.create(x))

  # // check local files
  ts.table$local <- file.exists(ts.table$filepath)
  # targets will tell us what the function to call is
  src <- tail(strsplit(ts.file,'[_.]')[[1]],2)[1]
  
  if (src == 'nldas'){
    ts.config <- yaml.load_file("../1_timeseries/in/ts_config.yml")
    gconfig(sleep.time=60, retries=2)
    use.i <- !ts.table$local & !ts.table$no.data
    files <- ts.table$filepath[use.i]
    message('data pulls for ',length(ts.table$filepath),' sites')
    message(length(ts.table$filepath[ts.table$local]),' already exist, ', length(files), ' will be new')
    if (length(files) > 0){
      details <- parse_ts_path(files, out=c('site_name','version','var',"dir_name"))
      times <- c(unique(ts.table$time.st[use.i]), unique(ts.table$time.en[use.i]))
      processed.files = stage_nldas_ts(sites=details$site_name, var=unique(details$var), times = times, 
                                       version=unique(details$version), folder=unique(details$dir_name), url = ts.config$nldas_url, verbose=TRUE)
      # // if in files, but not processed.files, the site has no data
      ts.table$local[use.i] <- file.exists(files)
      ts.table$no.data[use.i] <- !files %in% processed.files
    } #// else do nothing
    
  } else if (src == 'gldas') {
    ts.config <- yaml.load_file("../1_timeseries/in/ts_config.yml")
    gconfig(sleep.time=60, retries=2)
    use.i <- !ts.table$local & !ts.table$no.data
    files <- ts.table$filepath[use.i]
    message('data pulls for ',length(ts.table$filepath),' sites')
    message(length(ts.table$filepath[ts.table$local]),' already exist, ', length(files), ' will be new')
    if (length(files) > 0){
      details <- parse_ts_path(files, out=c('site_name','version','var',"dir_name"))
      times <- c(unique(ts.table$time.st[use.i]), unique(ts.table$time.en[use.i]))
      processed.files = stage_nldas_ts(sites=details$site_name, var=unique(details$var), times = times, 
                                       version=unique(details$version), folder=unique(details$dir_name), url = ts.config$gldas_url, verbose=TRUE)
      # // if in files, but not processed.files, the site has no data
      ts.table$local[use.i] <- file.exists(files)
      ts.table$no.data[use.i] <- !files %in% processed.files
    } #// else do nothing
    
  } else if (src == 'nwis'){
    #chunk sites
    message('data pulls for ',length(ts.table$filepath),' sites')
    files <- ts.table$filepath[!ts.table$local & !ts.table$no.data]
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
  write_status_table(ts.table, ts.file)
}
