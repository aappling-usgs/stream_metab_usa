#' helper function for staging different data types from the config
#' 
#' @param ts.file the status file for this timeseries variable & all appropriate
#'   sites
#' @param config a config list used to parameterize calls to stage_nwis_ts or 
#'   stage_nldas_ts
#' @return side effect: creates files and updates the ts.file. Returns TRUE if 
#'   successful, stops on error otherwise
#' @import mda.streams
#' @import dplyr
#' @seealso sb_check_ts_status read_status_table write_status_table
stage_ts <- function(ts.file, config=yaml.load_file("../1_timeseries/in/ts_config.yml"), retries=30){
  
  for(attempt in seq_len(retries)) {
    complete <- tryCatch({
      # update the ts.file for remote and local status. done here in case the status
      # table was last updated on someone else's computer
      to.stage <- sb_check_ts_status(ts.file, phase='stage') # not used by 'stage': posted_after=config$posted_after
      if(nrow(to.stage) == 0) {
        return(TRUE) # if we're already done, return now
      } else {
        message('#### STAGING TSES: ATTEMPT ', attempt, ' ####')
        
        to.stage <- bind_cols(to.stage, parse_ts_path(to.stage$filepath, out=c('site_name','version','dir_name')))
        
        # read the full ts.table for reporting (and, for calc_ts, documenting progress)
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
              version=config$version, folder=dir.name, 
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
                    version=config$version, folder=dir.name, verbose=TRUE)
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
          } else if (substr(src, 1, 4) == 'calc') {
            no_data <- c()
            for(i in 1:nrow(to.stage)) {
              tryCatch({
                # do the staging
                message('staging ', var, '_', src, ' for site ', to.stage[i,'site_name'])
                staged <- stage_calc_ts(
                  to.stage[i,'site_name'], var=var, src=src, folder=dir.name,
                  day_start=config$day_hours[1], day_end=config$day_hours[2], 
                  with_ts_version=config$version, with_ts_archived=FALSE, with_ts_uploaded_after=config$posted_after,
                  quietly=TRUE)
                if(is.null(staged)) stop('output of stage_calc_ts is NULL')
                
                # update the ts.status table and write to file
                srces <- select(attr(staged, 'choices'), -site_name, -file_path)
                status.row <- which(ts.table$filepath == attr(staged, 'choices')$file_path)
                if((length(status.row) != 1) || !all(names(srces) %in% names(ts.table))) stop('ts.status update error')
                ts.table[status.row, colnames(srces)] <- srces[1, colnames(srces)]
                ts.table[status.row, 'local'] <- TRUE
                write_status_table(ts.table, ts.file)
              }, error=function(e) {
                suppressWarnings(file.remove(to.stage$filepath[i]))
                if(grepl("(could not locate an appropriate ts)", e$message)) {
                  no_data <<- c(no_data, to.stage$filepath[i])
                } else {
                  stop("unexpected error: ", e$message)
                }
              })
            }
          }
        }
      }
    }, 
    error=function(e) {
      message('error in sb_post_ts: ', e)
      message('sleeping for 2 minutes before retry')
      Sys.sleep(60*2) # sleep a good long while to let SB recover
      FALSE
    })
    
    # decide whether to return or retry
    if(isTRUE(complete)) {
      break
    } else {
      # if we're not done, sleep a little while (on top of any error-related
      # sleeping above) and go to the next attempt
      Sys.sleep(5)
    }
  }
  
  num_incomplete <- if(isTRUE(complete)) 0 else { 
    # if !complete, we just don't know and need to check again
    tryCatch({
      nrow(sb_check_ts_status(ts.file, phase='stage', no_data=no_data))
    }, error=function(e) '??' )
  }
  if(is.numeric(num_incomplete) && num_incomplete==0) {
    return(TRUE)
  } else {
    stop("staging was incomplete; ", num_incomplete, " ts files still to stage")
  }
}

