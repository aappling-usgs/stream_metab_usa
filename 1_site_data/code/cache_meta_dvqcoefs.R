#' Stage the 'dvqcoefs' metadata file
#' 
#' @param sites vector of site_names
#' @param config list. read-in metadata config file
#' 
#' @seealso cache_meta
cache_meta_dvqcoefs <- function(config) {
  cache_meta(type='dvqcoefs', sites='ignored', folder=config$temp_dir)
}
