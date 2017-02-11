#' Do the modeling stuff specific to one config row. For the prep MLE run, run 
#' the model and compile daily K and Q into a summary data.frame
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
run_model <- function(config_row, verbose, outdir, model_name) {
  
  # sleep somewhere between 0 and 20 minutes to stagger the load on ScienceBase
  Sys.sleep(60*runif(1,0,20))
  
  # run the model
  if(verbose) message('ready to run model')
  print(config_row)
  if(verbose) message('preparing model')
  model_prep <- config_to_metab(config=config_row, rows=1, verbose=verbose, prep_only=TRUE)[[1]]
  tryCatch({
    print(lapply(model_prep, head))
    print(lapply(model_prep, dim))
  }, error=function(e) warning(e))
  if(verbose) message('running model')
  model_out <- config_to_metab(config=config_row, rows=1, verbose=verbose)[[1]]
  if(verbose) message('printing model')
  tryCatch({
    print(class(model_out))
    print(model_out)
  }, error=function(e) warning(e))
  if(verbose) message('determining whether modeling worked')
  if(length(model_out) == 0 || is.character(model_out)) {
    msg <- attr(model_out, 'errors')
    if(length(msg) == 0) msg <- model_out
    stop(msg)
  }
  
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
