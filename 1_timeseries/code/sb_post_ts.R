#' helper function for posting files to sciencebase w/ post_ts
#' 
#' @param files files created by stage_ts (or stage_nwis_ts; stage_nldas_ts)
#' 
#' @import mda.streams
#' @import dplyr
#' @seealso sb_check_ts_status read_status_table sb_archive_ts
sb_post_ts <- function(ts.file, config=yaml::yaml.load_file("../1_timeseries/in/ts_config.yml"), retries=30){
  # check on staging
  to.stage <- sb_check_ts_status(ts.file, phase='stage', posted_after = config$posted_after)
  if(nrow(to.stage) > 0) {
    warning("staging is still incomplete; ", nrow(to.stage), " ts files still to stage")
  }
  
  # make repeated, error-tolerant tries to repair & post tses
  for(attempt in seq_len(retries)) {
    complete <- tryCatch({
      # see what needs posting/repairing. return if we're done, otherwise
      # proceed to repair & post
      to.post.or.repair <- sb_check_ts_status(ts.file, phase='post', posted_after=config$posted_after)
      if(nrow(to.post.or.repair) == 0) {
        TRUE
      } else {
        message('#### POSTING TSES: ATTEMPT ', attempt, ' ####')
        
        to.post.or.repair <- bind_cols(
          to.post.or.repair, parse_ts_path(to.post.or.repair$filepath, out=c('var_src','site_name','version')))
        
        # read the full ts.table for reporting
        ts.table <- read_status_table(ts.file)
        message(
          nrow(ts.table) - nrow(to.post.or.repair),' are unavailable or already posted & tagged; ', 
          nrow(ts.table), ' sites total')
        
        # repair   
        to.repair <- filter(to.post.or.repair, posted & !tagged)
        if(nrow(to.repair) > 0) {
          message('repairing data for ', nrow(to.repair), ' timeseries')
          repair_ts(var_src=to.repair$var_src, site_name=to.repair$site_name)
        }
        
        # post
        to.post <- filter(to.post.or.repair, !posted)
        if(nrow(to.post) > 0) {
          message('posting data for ', nrow(to.post), ' timeseries')
          post_ts(to.post$filepath, on_exists=config$on_exists, archive_existing=config$archive_existing, verbose=TRUE)
        }
        
        # always return FALSE here so the to.post.or.repair check can happen
        FALSE
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
      length(sb_check_ts_status(ts.file, phase='post', posted_after=config$posted_after))
    }, error=function(e) '??' )
  }
  if(is.numeric(num_incomplete) && num_incomplete==0) {
    # get the final status
    status <- read_status_table(ts.file)
    
    # if we're done with posting, archive any files that still need it
    var_src <- unique(parse_ts_path(status$filepath)$var_src)
    sb_archive_ts(var_src, config)
    
    # return a vector of sites with this ts on SB
    posted <- status %>%
      filter(posted & tagged) %>%
      { .$filepath } %>%
      parse_ts_path(out='site_name', use_names=FALSE)
    return(posted)
  } else {
    # if we've tried and retried and still aren't done, give an error
    stop("posting was incomplete; ", num_incomplete, " ts files still to post")
  }
}
