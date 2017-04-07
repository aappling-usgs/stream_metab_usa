#' Create a config file to use as guidance for metabolism runs
#' 
#' @import mda.streams
#' @import streamMetabolizer
#' @import dplyr
#' @import tidyr
#' 
#' @param config a config for the config
create_run3_config <- function(
  run3_yaml=read_run3_yaml(),
  prev.config.file='../2_metab_models/run2/out/config.tsv',
  prev.stats.file='../2_metab_models/run2/out/model_stats.csv',
  outfile='../2_metab_models/run3/out/config.tsv') {
  
  if(check_frozen(outfile)) return(NULL)
  
  prev.config <- read_config(prev.config.file)
  prev.config$model_name <- make_metab_model_name(make_metab_run_title(format(as.Date(prev.config$date), '%y%m%d'), prev.config$tag, prev.config$strategy), prev.config$config.row, prev.config$site)
  prev.stats <- read.table(prev.stats.file, header=TRUE, sep=',', stringsAsFactors=FALSE)
  
  cfg.calcs <- prev.config %>%
    select(model_args, model_name) %>%
    full_join(select(prev.stats, model_name, err_obs_iid_sigma_Rhat.max, err_proc_iid_sigma_Rhat.max, K600_daily_sigma_Rhat.max), by='model_name') %>%
    arrange(model_name) %>%
    mutate(
      max_key_Rhat=pmax(err_obs_iid_sigma_Rhat.max, err_proc_iid_sigma_Rhat.max, K600_daily_sigma_Rhat.max),
      is_converged=max_key_Rhat < 1.2,
      rerun_priority=ifelse(!is_converged & !is.na(is_converged), 1, ifelse(is.na(is_converged), 2, 3)),
      burnin_steps=ifelse(!is_converged, 2000, 1000),
      saved_steps=ifelse(!is_converged, 2000, 500),
      model_args=mapply(function(ma, bs) {gsub('burnin_steps=1000', sprintf('burnin_steps=%0.0f', bs), ma)}, model_args, burnin_steps),
      model_args=mapply(function(ma, ss) {gsub('saved_steps=500', sprintf('saved_steps=%0.0f', ss), ma)}, model_args, saved_steps))

  cfg <- prev.config %>%
    select(-model_args) %>%
    full_join(cfg.calcs, by='model_name') %>%
    arrange(rerun_priority) %>%
    mutate(
      date="2017-04-06 15:04:23",
      tag=run3_yaml[['tag']],
      config.row=1:n()) %>%
    select_(.dots=setdiff(names(prev.config), 'model_name'))
    
  cfg.file <- write_config(cfg, outfile)
  
}