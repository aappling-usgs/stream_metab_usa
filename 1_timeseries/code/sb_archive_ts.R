#' helper function for archiving files on sciencebase if they haven't already
#' been replaced by newly posted data
#' 
#' @import mda.streams
#' @import dplyr
sb_archive_ts <- function(var_src, config) {
  if(!config$archive_existing) {
    message('config$archive_existing is FALSE so not archiving unreplaced files')
  }
  
  login_sb()
   
  # see what's needed
  to.archive <- summarize_ts_files(var_src) %>%
    filter(!is_archive, upload_date < as.POSIXct(config$posted_after))
  
  if(nrow(to.archive) > 0) {
    # do the archiving
    message("archiving ", nrow(to.archive), " unreplaced files")
    mda.streams:::archive_ts(ts_id=to.archive$ts_item, filename=to.archive$file_name)
    
    # check whether we've succeeded
    to.archive <- summarize_ts_files(var_src) %>%
      filter(!is_archive, upload_date < as.POSIXct(config$posted_after))
  }
  
  # return or stop dependening on our success
  if(nrow(to.archive) == 0) TRUE else 
    stop("archiving was incomplete; ", nrow(to.archive), " ts files still to archive")
}
