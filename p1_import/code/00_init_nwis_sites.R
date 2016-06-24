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

#' create a table for ts data with booleans for status
#' 
#' useful for remote/local processes that have a somewhat high failure rate. 
#' This file should contain all information necessary for formulating NWIS data 
#' requests.
#' 
#' @param sites a vector of site names formated via \code{\link[mda.streams]{make_site_name}}
#' @param config a configuration list for time series data 
#' @param file.out the file to write the table to
create_nldas_ts_table <- function(sites, config, file.out){
  create_ts_table(sites, config, file.out)
}

create_ts_table <- function(sites, config, file.out){
  var <- tail(strsplit(file.out,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(file.out,'[_.]')[[1]],2)[1]
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