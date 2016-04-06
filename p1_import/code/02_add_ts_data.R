
stage_ts <- function(target_name, ts.config){
  gconfig(wps.url = "http://cida-test.er.usgs.gov/gdp/process/WebProcessingService", sleep.time=60, retries=2)
  # targets will tell us what the function to call is
  ts_name <- make_ts_name(target_name)
  src <- parse_ts_name(ts_name, out = 'src')
  var <- parse_ts_name(ts_name, out = 'var')
  
  times <- ts.config$times
  version <- ts.config$version
  
  sites <- list_sites()
  
  if (src == 'nldas'){
    return(stage_nldas_ts(sites, var, times, version, url = ts.config$nldas.url), verbose=TRUE)
  } else if (src == 'nwis'){
    stop('not implemented yet')
  }
}