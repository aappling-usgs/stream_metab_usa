#' Update the ts.file based on what's staged locally, and return ts file1names 
#' for any tses that aren't yet staged
#' 
#' @param ts.file the filename of the status table
#' @param phase should the table be updated for the staging phase or the posting
#'   phase?
#' @param no_data vector of filenames where we've confirmed that there are no
#'   available data. The no.data status of filenames not mentioned will be 
#'   untouched.
#' @seealso read_status_table write_status_table auth_from_profile
#' @import dplyr
sb_check_ts_status <- function(ts.file, phase=c('stage','post'), no_data, posted_after='2016-04-01') {
  phase <- match.arg(phase)
  posted_after <- as.POSIXct(posted_after)
  
  # read in the status table
  ts.table <- read_status_table(ts.file)
  details <- parse_ts_path(ts.table$filepath, out=c('site_name','version','var','src','var_src','dir_name'), use_names=FALSE)
  var <- unique(details$var)
  src <- unique(details$var_src)
  var_src <- unique(details$var_src)
  
  if(phase=='stage') {
    ts.table$local <- file.exists(ts.table$filepath)
    if(!missing(no_data) && length(no_data) > 0) {
      ts.table[ts.table$filepath %in% no_data, 'no.data'] <- TRUE
    }
    needed <- filter(ts.table, !local, !no.data, !posted, !tagged) 
    
  } else if(phase=='post') {
    # recalculate the 'remote' column
    auth_from_profile()
    sites_by_tbl <- details$site_name
    ts_query <- summarize_ts_files(var_src) %>%
      filter(version %in% details$version, !is_archive, upload_date > posted_after)
    sites_by_dir <- ts_query$site_name[ts_query$by_dir]
    sites_by_tag <- ts_query$site_name[ts_query$by_tag]
    ts.table$posted <- sites_by_tbl %in% sites_by_dir
    ts.table$tagged <- sites_by_tbl %in% sites_by_tag
    needed <- filter(ts.table, local & !posted | !tagged)
  }
  
  # write the revised site status table
  write_status_table(ts.table, ts.file)  
  
  # return the list of sites that still need to be posted/reposted
  needed
}

