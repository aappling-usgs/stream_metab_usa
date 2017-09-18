#' write combine distances to NPDES/dams/canals with estimates of metabolic
#' footprint to create site flag metadata
make_meta_struct <- function(struct_file = "../1_spatial/in/CONF_struct_170407.csv", config, use_cached=TRUE, on_exists) {
  
  # here's a shortcut if the mfootdaily time series estimates are stored in a
  # local cache - lets you not re-download all those tses from sciencebase
  if(use_cached) {
    ts_folder <- "../2_metab_outputs/cache"
    mf_files <- dir(ts_folder, pattern='mfootdaily', full.names=TRUE)
    file.copy(mf_files, tempdir())
  }
  
  meta_file <- stage_meta_struct(struct_file = struct_file, folder = config$temp_dir, verbose = FALSE)
  
  sb_post_meta(meta_file, config=list(on_exists="replace"))
}
