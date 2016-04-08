
#' helper function for staging different data types from the config
#' 
#' @param target_name a var_src (e.g., baro_nldas or doobs_nwis)
#' @param ts.config a config list used to parameterize calls to stage_nwis_ts or stage_nldas_ts
#' @return files that were created
stage_ts <- function(target_name, ts.config){
  
  auth_internal()
  
  if (!dir.exists(ts.config$temp_dir))
    dir.create(ts.config$temp_dir)
  gconfig(sleep.time=60, retries=2)
  # targets will tell us what the function to call is
  ts_name <- make_ts_name(target_name)
  src <- parse_ts_name(ts_name, out = 'src')
  var <- parse_ts_name(ts_name, out = 'var')
  
  times <- ts.config$times
  version <- ts.config$version
  
  sites <- list_sites()
  
  if (src == 'nldas'){
    files = stage_nldas_ts(sites, var, times, version=version, folder=ts.config$temp_dir, url = ts.config$nldas_url, verbose=TRUE)
  } else if (src == 'nwis'){
    stop('not implemented yet')
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