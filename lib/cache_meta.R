#' Create & stage a metadata file to the specified cache folder
#' 
#' @param type character type of the metadata file to create, e.g., 'basic'
#' @param sites vector of site_names
#' @param folder directory where metadata file should be cached
#' 
#' @import mda.streams
cache_meta <- function(type, sites, folder) {
  if(!dir.exists(folder)) dir.create(folder)
  # on_exists='replace' replaces cache file, not sb file  
  switch(
    type,
    basic=stage_meta_basic(sites=sites, folder=folder, on_exists='replace')
  )
}
