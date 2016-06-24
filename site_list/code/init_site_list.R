#' create an initial nwis site list based on available data
#' 
#' @param config a list that configures the rules for sites
#' @return a character vector of site names as "nwis_{sitename}"
#' 
init_site_list <- function(config){
  
  project.sites <- powstreams::list_sites()
  
  fresh.sites <- c()
  hucs <- sprintf('%02.0f',1:21) # all HUCs (PR is 21 BTW)
  for (huc in hucs){
    fresh.sites = c(fresh.sites, get_nwis_sites(huc, config$min.count, config$skip.types, config$has.param))
  }
  return(unique(c(project.sites, fresh.sites)))
}



#' get nwis sites for a given huc
#' 
#' @param huc a two-digit huc, as a character (e.g., "01")
#' @param min.count minimum number of sample days to include as a site
#' @param skype.types remove these site categories from the response (e.g., "ES" for estuary)
#' @param has.param site must have this parameter to be included (e.g., "00300")
#' @importFrom dataRetrieval readNWISdata
#' @return a vector of site names formated via \code{\link[mda.streams]{make_site_name}}
get_nwis_sites = function(huc, min.count, skip.types, has.param){
  sites <- filter(dataRetrieval::readNWISdata(service = "site", huc=huc, seriesCatalogOutput="true",outputDataTypeCd="iv",parameterCd=has.param), parm_cd==has.param) %>% 
    filter(count_nu > min.count, access_cd == "0") %>% 
    filter(!(site_tp_cd %in% skip.types)) %>% filter(nchar(site_no) >= 8) %>% .$site_no
  if (length(sites) > 0)
    mda.streams::make_site_name(sites, database = 'nwis')
  else c()
}