#' Stage the 'basic' metadata file
#' 
#' @param sites vector of site_names
#' @param config list. read-in metadata config file
#' 
#' @seealso cache_meta
cache_meta_basic <- function(
  sites=mda.streams::list_sites(), 
  config=yaml::yaml.load_file('../1_site_data/in/meta_config.yaml')) {
  
  cache_meta(type='basic', sites=sites, folder=config$temp_dir) # replaces cache file, not sb file  
}
