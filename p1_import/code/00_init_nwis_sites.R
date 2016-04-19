
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
#' @return a vector of site names formated via \code{\link[mda.streams]{make_site_name}}
get_nwis_sites = function(huc, min.count, skip.types, has.param){
  sites <- filter(readNWISdata(service = "site", huc=huc, seriesCatalogOutput="true",outputDataTypeCd="iv",parameterCd=has.param), parm_cd==has.param) %>% 
    filter(count_nu > min.count, access_cd == "0") %>% 
    filter(!(site_tp_cd %in% skip.types)) %>% filter(nchar(site_no) >= 8) %>% .$site_no
  if (length(sites) > 0)
    mda.streams::make_site_name(sites, database = 'nwis')
  else c()
}

#' create a table for ts data with booleans for status
#' 
#' useful for remote/local processes that have a somewhat high failure rate. 
#' This file should contain all information necessary for formulating NWIS data 
#' requests.
#' 
#' @param sites a vector of site names formated via \code{\link[mda.streams]{make_site_name}}
#' @param config a configuration list for time series data 
#' @param file.out the file to write the table to
create_nwis_ts_table <- function(sites, config, file.out){
  
  var <- tail(strsplit(file.out,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(file.out,'[_.]')[[1]],2)[1]
  site.db <- mda.streams:::parse_site_name(sites, out='database')
  sites <- sites[site.db == src]
  
  ts.name <- make_ts_name(var, src)
  filepaths <- make_ts_path(sites, ts.name, version = config$version, folder = config$temp_dir)
  false.vect <- rep(FALSE, length(filepaths))
  time.st <- rep(config$times[1], length(filepaths))
  time.en <- rep(config$times[2], length(filepaths))
  site.table <- data.frame(filepath=filepaths, local=false.vect, remote=false.vect, no.data=false.vect, 
                           time.st=time.st, time.en=time.en)
  write_site_table(site.table, file.out)
  return(file.out)
}

write_site_table <- function(table, filename){
  write.table(table, file=filename, sep='\t', row.names=FALSE)
}