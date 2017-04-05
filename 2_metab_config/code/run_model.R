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
  if(verbose) message('printing model')
  tryCatch({
    print(class(model_out))
    print(model_out)
  }, error=function(e) warning(e))
  if(length(model_out) == 0 || is.character(model_out)) {
    message('modeling failed; returning')
    return(model_out)
  } else {
    message('modeling appears to have succeeded')
  }
  
  # summarize the model & associated data
  if(verbose) message('summarizing model')
  tryCatch({
    fit <- get_params(model_out, uncertainty='ci')
    site <- get_info(model_out)$config$site
    row <- get_info(model_out)$config$config.row
    smry <- fit %>%
      select(site_name=site, config_row=row, everything())
    
    # write the summary
    smry.file <- file.path(outdir, sprintf("summary %s.tsv", model_name))
    write.table(smry, smry.file, sep='\t', row.names=FALSE)
  }, error=function(e) {
    warning("couldn't compute the summary; proceeding without")
  })
  
  # [the model gets written by the calling function]
  return(model_out)
}
