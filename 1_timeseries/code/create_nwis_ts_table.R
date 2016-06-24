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
  site.db <- mda.streams:::parse_site_name(sites, out='database')
  src <- tail(strsplit(file.out,'[_.]')[[1]],2)[1]
  sites <- sites[site.db == src]
  create_ts_table(sites, config, file.out)
}
