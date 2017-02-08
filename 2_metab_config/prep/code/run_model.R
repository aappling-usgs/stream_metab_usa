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
  print(config_row)
  print(str(config_row))
  message(config_row[[1,'model_args']])
  print(streamMetabolizer::specs)
  tryCatch(streamMetabolizer::specs('m_np_oi_tr_plrckm.nlm'), error=function(e) message("couldn't run streamMetabolizer::specs('m_np_oi_tr_plrckm.nlm')"))
  print(specs)
  tryCatch(specs('m_np_oi_tr_plrckm.nlm'), error=function(e) {warning(e); message("couldn't run specs('m_np_oi_tr_plrckm.nlm')")})
  tryCatch(list(specs=specs('m_np_oi_tr_plrckm.nlm')), error=function(e) {warning(e); message("couldn't run list(specs=specs())")})
  tryCatch(message(eval(parse(text=config_row[[1,'model_args']]))), error=function(e) {warning(e); message("couldn't eval(parse(text=cma))")})
  message(is.list(eval(parse(text=config_row[[1,'model_args']]))))
  model_out <- config_to_metab(config=config_row, rows=1, verbose=verbose)[[1]]
  
  # summarize the model & associated data
  if(verbose) message('summarizing model')
  fit <- get_fit(model_out)
  site <- get_info(model_out)$config$site
  Q <- get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), site)
  date_stats <- mm_model_by_ply(mm_model_by_ply_prototype, data=get_data(model_out), day_start=4, day_end=28)
  smry <- fit %>%
    mutate(site_name=site) %>%
    left_join(v(Q), by=c(date='sitedate')) %>%
    left_join(v(date_stats), by='date') %>%
    select(site_name, date, K600.daily, K600.daily.sd, discharge.daily=dischdaily, data_ply_nrow, ply_validity, timestep_days, everything())
  
  # write the summary
  write.table(smry, file.path(outdir, sprintf('summary_%03.0f.tsv', as.numeric(config_row$config.row))), sep='\t', row.names=FALSE)
  
  # [the model gets written by the calling function]
  return(model_out)
}
