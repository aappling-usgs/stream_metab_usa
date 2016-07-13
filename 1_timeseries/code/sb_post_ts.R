#' helper function for posting files to sciencebase w/ post_ts
#' 
#' @param files files created by stage_ts (or stage_nwis_ts; stage_nldas_ts)
#' 
#' @import mda.streams
#' @import dplyr
#' @seealso sb_check_ts_status read_status_table
sb_post_ts <- function(ts.file, config=yaml::yaml.load_file("../1_timeseries/in/ts_config.yml")){
  
  # check on staging
  to.stage <- sb_check_ts_status(ts.file, phase='stage')
  if(nrow(to.stage) > 0) {
    warning("staging is still incomplete; ", nrow(to.stage), " ts files still to stage")
  }
  
  # see what needs posting/repairing
  to.post.or.repair <- sb_check_ts_status(ts.file, phase='post', posted_after=config$posted_after)
  if(nrow(to.post.or.repair) == 0) {
    return(TRUE) # if we're already done, return now
  }
  to.post.or.repair <- bind_cols(
    to.post.or.repair, parse_ts_path(to.post.or.repair$filepath, out=c('var_src','site_name','version')))

  # read the full ts.table for reporting
  ts.table <- read_status_table(ts.file)
  message(
    nrow(ts.table) - nrow(to.post.or.repair),' are already posted & tagged; ', 
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
    message('posting data for ', nrow(to.repair), ' timeseries')
    post_ts(to.post$filepath, on_exists=config$on_exists, verbose=TRUE)
  }
  
  # re-check and either return a vector of sites w/ this ts on SB or give error
  incomplete <- sb_check_ts_status(outfile, phase='post', posted_after=config$posted_after)
  if(nrow(incomplete) == 0) {
    posted <- read_status_table(ts.file) %>%
      filter(posted & tagged) %>%
      { .$filepath } %>%
      parse_ts_path(out='site_name', use_names=FALSE)
    return(posted)
  } else {
    stop("posting was incomplete; ", nrow(incomplete), " ts files still to post")
  }
}
