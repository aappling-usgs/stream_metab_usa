#' make a tsv file of predictions for this model
make_model_preds <- function(model_out, outdir) {
  
  library(methods) # necessary when running via 'Rscript' call at command line
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  library(streamMetabolizer)
  library(mda.streams)
  
  # Get model info
  config_row <- get_info(model_out)$config %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site))
  
  # create one big table of fitted daily values (valid days only)
  fit <- get_fit(model_out)
  preds <- fit$daily %>%
    filter(valid_day) %>%
    select(
      date,
      GPP=GPP_daily_50pct, GPP.lower=GPP_daily_2.5pct, GPP.upper=GPP_daily_97.5pct, GPP.Rhat=GPP_daily_Rhat,
      ER=ER_daily_50pct, ER.lower=ER_daily_2.5pct, ER.upper=ER_daily_97.5pct, ER.Rhat=ER_daily_Rhat,
      K600=K600_daily_50pct, K600.lower=K600_daily_2.5pct, K600.upper=K600_daily_97.5pct, K600.Rhat=K600_daily_Rhat)
  predfile <- file.path(outdir, sprintf("%s %s.%s", config_row$model_name, "preds", "tsv"))
  write.table(preds, predfile, row.names=FALSE, sep='\t')
}
