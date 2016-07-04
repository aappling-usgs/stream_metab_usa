#' Update the site.file based on what's posted remotely, and return any
#' site_names for sites that aren't yet posted
#' 
#' @seealso read_status_table write_status_table
sb_check_site_status <- function(site.file) {
  # read in the site status table
  site.table <- read_status_table(site.file)
  
  # recalculate the 'remote' column
  sites_by_tbl <- site.table$site_name
  sites_by_dir <- list_sites()
  sites_by_tag <- locate_site(sites_by_tbl, by='tag')
  site.table$remote <- (sites_by_tbl %in% sites_by_dir) & !is.na(sites_by_tag)
  
  # write the revised site status table
  write_status_table(site.table, site.file)  
  
  # return the list of sites that still need to be posted/reposted
  site.table$site_name[!site.table$remote]
}
