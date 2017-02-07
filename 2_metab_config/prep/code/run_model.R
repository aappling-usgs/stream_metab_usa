#' For prep MLE run, run the model and compile daily K and Q
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
run_model <- function(config_row, verbose, outdir) {
  library(streamMetabolizer)
  library(mda.streams)
  library(dplyr)
  library(unitted)
  
  # run the model
  if(verbose) message('running model')
  model_out <- config_to_metab(config=config_row, rows=1, verbose=verbose)[[1]]
  
  # summarize the model & associated data
  if(verbose) message('summarizing model')
  fit <- get_fit(model_out)
  site <- get_info(model_out)$config$site
  Q <- get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), site)
  smry <- fit %>%
    mutate(site_name=site) %>%
    left_join(v(Q), by=c(date='sitedate')) %>%
    select(site_name, K600.daily, K600.daily.sd, discharge.daily=dischdaily, everything())
  
  # write the summary
  write.table(smry, file.path(outdir, sprintf('summary_%03.0f.tsv', as.numeric(config_row$config.row))), sep='\t', row.names=FALSE)
  
  # [the model gets written by the calling function]
  return(model_out)
}
