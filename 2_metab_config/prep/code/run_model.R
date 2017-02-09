#' For prep MLE run, run the model and compile daily K and Q
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
run_model <- function(config_row, verbose, outdir, model_name) {
  
  login_sb()
  
  # run the model
  if(verbose) message('running model')
  print(config_row)
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
  smry.file <- file.path(outdir, sprintf("summary %s.tsv", model_name))
  write.table(smry, smry.file, sep='\t', row.names=FALSE)

  # [the model gets written by the calling function]
  return(model_out)
}
