#' figure out what files already exist on the server
#' 
#' @param ldas_config list config that specifies the data subset variables
ldas_server_files <- function(ldas_config){
  server.data <- xmlParse(ldas_config$catalog_url, useInternalNodes = TRUE)
  
  nsDefs <- xmlNamespaceDefinitions(server.data )
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  ncdf.datasets <- getNodeSet(server.data,'/xmlns:catalog/xmlns:dataset/xmlns:dataset[substring(@name, string-length(@name) - string-length(".nc") +1) = ".nc"]', ns)
  ncdf.files <- unlist(xmlApply(ncdf.datasets, function(x) xmlAttrs(x)[['name']]))
  ncdf.files <- ncdf.files[grepl(pattern = ldas_config$data_name, ncdf.files)] # match to dataset name
  if (is.null(ncdf.files))
    ncdf.files = c() # no files on the server
  return(ncdf.files)
}
