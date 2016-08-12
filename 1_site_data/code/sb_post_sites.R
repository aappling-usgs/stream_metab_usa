#' helper function for posting sites to sciencebase w/ post_site
#' 
#' @param site.file a tsv containing status information on site availability and SB presence
sb_post_sites <- function(site.file, config) {
  # post to ScienceBase any sites that are not yet fully remotely posted
  login_sb()
  sites_to_post <- sb_check_site_status(site.file)
  post_site(sites_to_post, on_exists=config$on_exists)
  
  # stop with error if ANY sites haven't been properly posted & tagged
  incomplete_sites <- sb_check_site_status(site.file)
  if(length(incomplete_sites) > 0) stop('one or more sites were not successfully posted')

  # if we haven't stopped, return the list of sites that are complete (i.e., all of them)
  return(read_status_table(site.file)$site_name)
}
