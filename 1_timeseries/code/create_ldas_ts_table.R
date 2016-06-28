#' create a table for ts data with booleans for status
#' 
#' useful for remote/local processes that have a somewhat high failure rate. 
#' This file should contain all information necessary for formulating NWIS data 
#' requests.
#' 
#' @param sites a vector of site names formated via \code{\link[mda.streams]{make_site_name}}
#' @param config a configuration list for time series data 
#' @param file.out the file to write the table to
#' 
#' @seealso create_ts_table
create_ldas_ts_table <- function(sites, config, file.out){
  create_ts_table(sites, config, file.out)
}
