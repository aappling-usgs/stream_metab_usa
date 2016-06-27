#' helper function for posting sites to sciencebase w/ post_site
#' 
#' @param sites a vector of site IDs
#' @param sites.config a read-in config file about acquiring site info
sb_post_sites <- function(sites, sites.config) {
  
  stop("this function isn't done yet")
  
  auth_from_profile()
  
  for (site in files){
    post_site(site, on_exists=sites.config$on_exists, verbose=TRUE)
    sb.id <- post_ts(file, on_exists=ts.config$on_exists, verbose=TRUE)
    if (is.character(sb.id) & nchar(sb.id) > 0){
      file.i <- which(file == ts.table$filepath)
      ts.table$remote[file.i] <- TRUE
      write_site_table(ts.table, ts.file)
    }
  }
  remote.sites <- parse_ts_path(ts.table$filepath[ts.table$remote], out='site_name', use_names = FALSE)
  return(remote.sites)
}
