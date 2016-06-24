#' create an initial nwis site list based on available data
#' 
#' @param config a list that configures the rules for sites
#' @return a character vector of site names as "nwis_{sitename}"
#' 
init_site_list <- function(config, outfile='out/site_list.txt'){
  
  project.sites <- mda.streams::list_sites()
  
  fresh.sites <- c()
  hucs <- sprintf('%02.0f',1:21) # all HUCs (PR is 21 BTW)
  for (huc in hucs){
    fresh.sites = c(fresh.sites, get_nwis_sites(huc, config$min.count, config$skip.types, config$has.param))
  }
  sitelist <- unique(c(project.sites, fresh.sites))
  writeLines(sitelist, outfile)
  return(sitelist)
}
