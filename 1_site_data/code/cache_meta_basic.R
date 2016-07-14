#' Stage the 'basic' metadata file
#' 
#' @param sites vector of site_names
#' @param config list. read-in metadata config file
#' 
#' @seealso cache_meta
cache_meta_basic <- function(sites, config) {
  cache_meta(type='basic', sites=sites, folder=config$temp_dir)
}
