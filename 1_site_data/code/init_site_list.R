#' create an initial nwis site list based on available data
#' 
#' @param config a list that configures the rules for sites
#' @return a character vector of site names as "nwis_{sitename}"
#' 
init_site_list <- function(config){
  
  # combine existing SB sites with all NWIS sites meeting our criteria
  project.sites <- mda.streams::list_sites()
  fresh.sites <- mda.streams::stage_nwis_sitelist(
    vars=config$has.vars, min.obs=config$min.count, site.types=config$site.types, HUCs=1:21, folder = NULL, verbose = TRUE)
  sitelist <- union(project.sites, fresh.sites)

  return(sitelist)
}
