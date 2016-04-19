
#' helper function for staging different data types from the config
#' 
#' @param target_name a var_src (e.g., baro_nldas or doobs_nwis)
#' @param ts.config a config list used to parameterize calls to stage_nwis_ts or stage_nldas_ts
#' @return files that were created
stage_ts <- function(target_name, ts.config, sites){
  
  auth_internal()
  
  if (!dir.exists(ts.config$temp_dir))
    dir.create(ts.config$temp_dir)
  
  # targets will tell us what the function to call is
  ts_name <- make_ts_name(target_name)
  src <- parse_ts_name(ts_name, out = 'src')
  var <- parse_ts_name(ts_name, out = 'var')
  
  times <- ts.config$times
  version <- ts.config$version
  
  ts.name <- make_ts_name(target_name)
  
  if (src == 'nldas'){
    gconfig(sleep.time=60, retries=2)
    files = stage_nldas_ts(sites, var, times, version=version, folder=ts.config$temp_dir, url = ts.config$nldas_url, verbose=TRUE)
  } else if (src == 'nwis'){
    #chunk sites
    site.db <- mda.streams:::parse_site_name(sites, out='database')
    sites <- sites[site.db == 'nwis']
    new.sites <- !file.exists(make_ts_path(sites, ts.name, folder = ts.config$temp_dir, version = version))
    sites <- sites[new.sites]
    message('data pulls for ',length(sites),' sites')
    chunk.st <- seq(1, length(sites), by=ts.config$nwis.chunk)
    chunk.en <- c(tail(chunk.st, -1) -1, length(sites))
    files <- c()
    for (i in seq_along(chunk.st)){
      files = c(files, stage_nwis_ts(sites[chunk.st[i]:chunk.en[i]], var, times, version=version, folder=ts.config$temp_dir, verbose=TRUE))
    }
  }
  return(files)
}

#' helper function for posting files to sciencebase w/ post_ts
#' 
#' @param files files created by stage_ts (or stage_nwis_ts; stage_nldas_ts)
#' @param ts.config a config list used to parameterize the post_ts function
sb_post_ts <- function(files, ts.config){
  auth_internal()
  post_ts(files, on_exists=ts.config$on_exists, verbose=TRUE)
}