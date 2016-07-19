#' helper function for staging different data types from the config
#' 
#' @param ts.file the status file for this timeseries variable & all appropriate
#'   sites
#' @param config a config list used to parameterize calls to stage_nwis_ts or 
#'   stage_nldas_ts
#' @return side effect: creates files and updates the ts.file. Returns TRUE if
#'   successful, stops on error otherwise
stage_ts <- function(ts.file, config=yaml.load_file("../1_timeseries/in/ts_config.yml")){
  
  # update the ts.file for remote and local status. done here in case the status
  # table was last updated on someone else's computer
  to.stage <- sb_check_ts_status(ts.file, phase='stage') # not used by 'stage': posted_after=config$posted_after
  if(nrow(to.stage) == 0) {
    return(TRUE) # if we're already done, return now
  }
  to.stage <- bind_cols(to.stage, parse_ts_path(to.stage$filepath, out=c('site_name','version','dir_name')))
  
  # read the full ts.table for reporting
  ts.table <- read_status_table(ts.file)
  message(
    'staging data for ', nrow(to.stage), ' new sites; ', 
    nrow(ts.table) - nrow(to.stage),' are already local/posted/unavailable; ', 
    nrow(ts.table), ' sites total')
  
  # make sure local staging dir exists
  dir.name <- unique(to.stage$dir_name)
  lapply(dir.name, function(x) if(!dir.exists(x)) dir.create(x))
  
  # determine which function to call and for which sites
  var <- tail(strsplit(ts.file,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(ts.file,'[_.]')[[1]],2)[1]
  
  if (nrow(to.stage) > 0) {
    if (src %in% c('nldas','gldas')) {
      # process all the sites all at once
      gconfig(sleep.time=60, retries=2)
      processed.files <- stage_ldas_ts(
        sites=to.stage$site_name, var=var, src=src, times=config$times, 
        version=config$version, folder=unique(to.stage$dir_name), 
        url=config[[paste0(src, '_url')]], verbose=TRUE)
      no_data <- to.stage$filepath[!(to.stage$filepath %in% processed.files)]
      
    } else if (src == 'nwis') {
      # dataRetrieval could handle sites in larger chunks, but doing them
      # one-by-one to isolate site-specific errors
      no_data <- c()
      for (i in 1:nrow(to.stage)) {
        if(parse_site_name(to.stage$site_name[i], out='database') != 'nwis') {
          no_data <- c(no_data, to.stage$filepath[i])
        } else {
          local.file <- withCallingHandlers({ 
            stage_nwis_ts(
              sites=to.stage$site_name[i], var=var, times=config$times, 
              version=config$version, folder=to.stage$dir_name[i], verbose=TRUE)
          }, warning=function(w) {
            if(grepl("NWIS error", w$message)) message(w$message)
          }, message=function(m) {
            if(grepl("(data are unavailable)|(no non-NA data)", m$message)) {
              no_data <<- c(no_data, to.stage$filepath[i])
            }            
          })
        }
        if((i %% 10) == 0) sb_check_ts_status(ts.file, phase='stage', no_data=no_data)
      }
    } else if (src == 'calc') {
      # staged <- stage_calc_ts(sites[i], var=parse_var_src(var_src, out='var'),
      # src=parse_var_src(var_src, out='src'), day_start=config$day_hours[1], day_end=config$day_hours[2])
    }
  }
  to.stage <- sb_check_ts_status(ts.file, phase='stage', no_data=no_data) # not used by 'stage': posted_after=config$posted_after
  
  if(nrow(to.stage) == 0) {
    return(TRUE)
  } else {
    stop("staging was incomplete; ", nrow(to.stage), " ts files still to stage")
  }
}

