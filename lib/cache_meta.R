#' Create & stage a metadata file to the specified cache folder
#' 
#' @param type character type of the metadata file to create, e.g., 'basic'
#' @param sites vector of site_names
#' @param folder directory where metadata file should be cached
#' 
#' @import mda.streams
cache_meta <- function(type, config, ...) {
  folder <- config$temp_dir
  if(!dir.exists(folder)) dir.create(folder)
  # on_exists='replace' replaces cache file, not sb file  
  switch(
    type,
    basic    = stage_meta_basic(folder=folder, ...),
    dvqcoefs = stage_meta_dvqcoefs(folder=folder, ...),
    struct   = stage_meta_struct(folder=folder, ...)
  )
}
