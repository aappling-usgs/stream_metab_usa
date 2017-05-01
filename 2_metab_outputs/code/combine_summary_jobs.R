combine_summary_jobs <- function(config.file='../2_metab_outputs/out/config.tsv',
                                 outfile='../2_metab_outputs/out/model_stats.tsv') {
  
  if(check_frozen(outfile)) return(NULL)
  
  library(dplyr)
  
  # create one big stats summary table for all models
  stats_tsvs <- dir('../2_metab_outputs/out/summaries', pattern=' stats.tsv', full.names=TRUE)
  all_stats <- bind_rows(lapply(stats_tsvs, function(sc) {
    read.table(sc, header=TRUE, stringsAsFactors=FALSE, sep='\t')
  }))
  
  # use the config file for this run to ensure we're not missing any sites, even
  # if some stats .tsv files are missing
  config <- read_config(config.file)
  config$model_name <- make_metab_model_name(
    make_metab_run_title(format(as.Date(config$date), '%y%m%d'), config$tag, config$strategy), 
    config$config.row, config$site)
  all_stats <- all_stats %>%
    full_join(select(config, model_name), by='model_name')
  
  # add additional model metadata
  all_stats <- all_stats %>%
    bind_cols(parse_metab_model_name(all_stats$model_name, use_names=FALSE)) %>%
    mutate(resolution=substring(strategy, 7))
  
  # compute our chosen metric of convergence
  all_stats <- all_stats %>%
    mutate(max_key_Rhat=pmax(err_obs_iid_sigma_Rhat.max, err_proc_iid_sigma_Rhat.max, K600_daily_sigma_Rhat.max),
           is_converged=max_key_Rhat < 1.2,
           needs_rerun=!is_converged & saved_steps == 500)
  
  write.table(all_stats, outfile, row.names=FALSE, sep='\t')
}
