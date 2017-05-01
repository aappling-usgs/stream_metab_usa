#' write metab ts files for the current model=resolution (could be several
#' resolutions per site)
make_model_tses <- function(model_out, outdir) {
  
  library(methods) # necessary when running via 'Rscript' call at command line
  library(streamMetabolizer)
  library(mda.streams)
  
  tsfiles <- stage_metab_ts(model_out, folder=outdir)
  res <- substring(get_info(model_out)$config$strategy, 7)
  sapply(tsfiles, function(tsfile) {
    newpath <- file.path(paste0(tools::file_path_sans_ext(tsfile), '_', res, '.', tools::file_ext(tsfile)))
    file.copy(tsfile, newpath)
    file.remove(tsfile)
  })
}
  