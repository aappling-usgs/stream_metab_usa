#' For prep MLE run, run the model and compile daily K and Q
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
run_model <- function(config_row, verbose, outdir) {
  
  # run the model
  if(verbose) message('running model')
  print(1)
  print(config_row)
  print(2)
  print(config_row[[1,'model_args']])
  print(2.5)
  print(mm_parse_name('m_np_oi_tr_plrckm.nlm'))
  print(3)
  sp <- streamMetabolizer::specs('m_np_oi_tr_plrckm.nlm')
  print(3.5)
  print(sp)
  print(4)
  print(specs('m_np_oi_tr_plrckm.nlm'))
  print(5)
  print(list(specs=specs('m_np_oi_tr_plrckm.nlm')))
  print(6)
  print(eval(parse(text=config_row[[1,'model_args']])))
  print(7)
  model_out <- config_to_metab(config=config_row, rows=1, verbose=verbose)[[1]]
  print(8)

  # summarize the model & associated data
  if(verbose) message('summarizing model')
  print(9)
  fit <- get_fit(model_out)
  print(10)
  site <- get_info(model_out)$config$site
  print(11)
  Q <- get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), site)
  print(12)
  date_stats <- mm_model_by_ply(mm_model_by_ply_prototype, data=get_data(model_out), day_start=4, day_end=28)
  print(13)
  smry <- fit %>%
    mutate(site_name=site) %>%
    left_join(v(Q), by=c(date='sitedate')) %>%
    left_join(v(date_stats), by='date') %>%
    select(site_name, date, K600.daily, K600.daily.sd, discharge.daily=dischdaily, data_ply_nrow, ply_validity, timestep_days, everything())
  print(14)

  # write the summary
  write.table(smry, file.path(outdir, sprintf('summary_%03.0f.tsv', as.numeric(config_row$config.row))), sep='\t', row.names=FALSE)
  print(15)

  # [the model gets written by the calling function]
  return(model_out)
}
