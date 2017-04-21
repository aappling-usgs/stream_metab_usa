#' Do the modeling stuff specific to one config row. This file is for the main
#' Bayesian run.
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
run_model <- function(config_row, verbose, outdir, model_name) {
  
  # run the model
  if(verbose) message('ready to run model')
  print(config_row)
  if(verbose) message('running model')
  model_out <- config_to_metab(config=config_row, rows=1, verbose=verbose)[[1]]
  saveRDS(model_out, file.path(outdir, sprintf("partial %s.Rds", model_name)))
  if(verbose) message('printing model')
  tryCatch({
    print(class(model_out))
    print(model_out)
  }, error=function(e) message(e$message))
  if(length(model_out) == 0 || is.character(model_out)) {
    message('modeling failed; returning')
    return(model_out)
  } else {
    message('modeling appears to have succeeded')
  }
  
  # [the model gets written by the calling function]
  return(model_out)
}
