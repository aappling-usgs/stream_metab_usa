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
    struct   = {
      # here's a shortcut if the mfootdaily time series estimates are stored in a
      # local cache - lets you not re-download all those tses from sciencebase
      args <- list(...)
      use_cached <- args[['use_cached']]
      if(isTRUE(use_cached)) {
        ts_folder <- "../2_metab_outputs/cache"
        mf_files <- dir(ts_folder, pattern='mfootdaily', full.names=TRUE)
        file.copy(mf_files, tempdir())
      }
      stage_meta_struct(folder=folder, struct_file=args$struct_file, verbose=args$struct_file)
    }
  )
}
