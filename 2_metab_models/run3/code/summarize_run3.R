# Do several kinds of summaries all at once, in the hope they'll all work:
# diagnostic plots, estBest tses, table of predictions for data pub, 
summarize_run3_models <- function(config.file='../2_metab_models/run3/out/config.tsv', outfile='../2_metab_models/run3/out/allstats.tsv') {
  
  if(check_frozen(outfile)) return(NULL)
  
  library(dplyr)
  library(tidyr)
  library(mda.streams)
  source('../2_metab_models/run3/code/make_model_summary.R')
  source('../2_metab_models/run3/code/make_model_tses.R')
  source('../2_metab_models/run3/code/make_model_preds.R')
  source('../2_metab_models/run3/code/make_model_fit.R')
  
  sumdir <- file.path(dirname(config.file), 'summaries')
  tsdir <- file.path(dirname(config.file), 'tses')
  predsdir <- file.path(dirname(config.file), 'preds')
  fitdir <- file.path(dirname(config.file), 'fits')
  lapply(c(sumdir, tsdir, predsdir, fitdir), function(xdir) if(!dir.exists(xdir)) dir.create(xdir))
  
  config <- read_config(config.file) %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site))
  file_tally <- tally_summary_files(config, sumdir)
  need_rows <- file_tally %>% filter(is.na(all_files)) %>% .$row
  
  # create the individual files for each model (config.row)
  login_sb()
  for(cr in need_rows) {
    message("summarizing config row ", cr)
    tryCatch({
      mm <- get_metab_model(config$model_name[cr], version='original', update_sb=FALSE)
      make_model_summary(mm, outdir=sumdir)
      make_model_tses(mm, outdir=tsdir)
      make_model_preds(mm, outdir=predsdir)
      make_model_fit(mm, outdir=fitdir)
    }, error=function(e) {
      message('  error: ', e$message)
      cat(sprintf("Error in config.row %d:", cr),
          paste0('  ', capture.output(traceback(e))),
          '',
          sep='\n',
          file=file.path(dirname(config.file), 'summary_errors.txt'), 
          append=TRUE)
    })
  }
  
  need_rows <- which_summaries_needed(config, sumdir)
  if(length(need_rows) > 0) stop(paste("not done with summaries yet; missing", length(need_rows)))
  
  # create one big stats summary table for all models
  stats_csvs <- dir('../2_metab_models/run3/out/summaries', pattern=' stats.csv', full.names=TRUE)
  all_stats <- bind_rows(lapply(stats_csvs, function(sc) {
    read.csv(sc, header=TRUE, stringsAsFactors=FALSE)
  }))
  write.csv(all_stats, outfile, row.names=FALSE)
}  

tally_summary_files <- function(config, sumdir) {
  # figure out what's already in the summaries directory
  targets <- list(
    summaries = c('messages.txt', 'plot_daily.png', 'plot_daily_new.png', 'plot_daily_newscale.png', 'plot_dists.png', 'plot_KQ.png', 'plot_Rhats.png', 'stats.csv'),
    tses = c('ts_gpp_estBest.rds', 'ts_er_estBest.rds', 'ts_K600_estBest.rds'),
      preds =
  )
  done_files <- dir(sumdir)
  suffix_pos <- regexpr('bayes_[0-9]*min ', done_files)
  done_files_sum <- 
    data_frame(
      row = parse_metab_model_name(done_files, out='row', use_names=FALSE),
      filetype = mapply(function(str, pos, len) { substring(str, pos+len) }, done_files, suffix_pos, attr(suffix_pos, 'match.length'), USE.NAMES = FALSE),
      present = TRUE) %>%
    group_by(row) %>%
    summarize(all_files=all(target_files %in% filetype)) %>%
    full_join(select(mutate(config, row=as.numeric(config.row)), row), by='row') %>%
    arrange(row)
  done_files_sum
}

summarize_run3_stats <- function(model.stats.file='../2_metab_models/run3/out/model_stats.csv', 
                                 outdir='../2_metab_models/run3/out/summaries/') {
  
  all_stats <- read.csv(model.stats.file, header=TRUE, stringsAsFactors=FALSE)
  
  # plot distributions of various metrics of whole-model convergence
  rhat_stats_wide <- select(all_stats, model_name, starts_with('all_params_Rhat')) %>% 
    setNames(., gsub('all_params_Rhat.', '', names(.))) %>%
    select(-n, -n_over_1.05, -n_over_1.1, -npar) %>%
    arrange(mean) %>%
    mutate(id=1:n())
  plot_rhatqs <- rhat_stats_wide %>% arrange(mean) %>% mutate(id=1:n()) %>%
    gather(metric, value, -model_name, -id, -min, -max) %>%
    filter(metric %in% c('mean', 'median', 'meanq95', 'q95')) %>%
    ggplot(aes(x=id)) +  
    geom_ribbon(aes(ymin=min, ymax=max), alpha=0.2) +
    geom_point(aes(y=value, color=metric)) +
    theme_bw() + theme(legend.position=c(0.2,0.8)) +
    scale_y_log10() + 
    ylab('mean/median/etc. of Rhats') + xlab('sites ordered by mean Rhat')
  ggsave(file.path(outdir, 'plot_rhatqs.png'), plot_rhatqs, width=7, height=7)
  plot_rhatovers <- rhat_stats_wide %>% arrange(meanpct_over_1.05) %>% mutate(id=1:n()) %>%
    gather(metric, value, -model_name, -id) %>%
    filter(metric %in% c('meanpct_over_1.05', 'meanpct_over_1.1', 'pct_over_1.05', 'pct_over_1.1')) %>%
    ggplot(aes(x=id)) +
    geom_point(aes(y=value, color=metric)) + 
    theme_bw() + theme(legend.position=c(0.2,0.8)) +
    ylab('Percent of Rhats exceeding a value') + xlab('sites ordered by meanpct_over_1.05')
  ggsave(file.path(outdir, 'plot_rhatovers.png'), plot_rhatovers, width=7, height=7)
  
  # plot K600_daily_sigma_sigma vs K600_daily_sigma
  plot_K600_daily_sigmas <- ggplot(all_stats, aes(x=K600_daily_sigma_sigma, y=K600_daily_sigma.median.p50)) + 
    geom_abline() + geom_point() + 
    theme_bw() + xlab('K600_daily_sigma_sigma (the prior)') + ylab('K600_daily_sigma_50pct (the posterior)')
  ggsave(file.path(outdir, 'plot_K600_daily_sigmas.png'), plot_K600_daily_sigmas, width=7, height=7)
  
  plot_K600_sig_vs_median <- cowplot::plot_grid(
    ggplot(all_stats, aes(x=K600_new.median, y=K600_daily_sigma_sigma)) + geom_point(color='red') + theme_bw(),
    ggplot(all_stats, aes(x=K600_new.median, y=K600_daily_sigma.median.p50)) + geom_point(color='black') + theme_bw()
  )
  ggsave(file.path(outdir, 'plot_K600_sig_vs_median.png'), plot_K600_sig_vs_median, width=7, height=4)
}

start_run3_expert_file <- function(config.file='../2_metab_models/run3/out/config.tsv',
                                   model.stats.file='../2_metab_models/run3/out/model_stats.csv', 
                                   outfile='../2_metab_models/run3/out/expert_file.csv') {
  # read the config file for this run
  config <- read_config(config.file)
  config$model_name <- make_metab_model_name(make_metab_run_title(format(as.Date(config$date), '%y%m%d'), config$tag, config$strategy), config$config.row, config$site)
  
  all_stats <- read.csv(model.stats.file, header=TRUE, stringsAsFactors=FALSE)
  
  # starter file for bob and maite's expert assessment
  bm_model_rows <- all_stats %>%
    full_join(select(config, model_name), by='model_name') %>%
    arrange(model_name) %>%
    mutate(max_key_Rhat=pmax(err_obs_iid_sigma_Rhat.max, err_proc_iid_sigma_Rhat.max, K600_daily_sigma_Rhat.max),
           is_converged=max_key_Rhat < 1.2)
  bm_model_rows <- bm_model_rows %>%
    bind_cols(parse_metab_model_name(bm_model_rows$model_name, use_names=FALSE)) %>%
    mutate(resolution=substring(strategy, 7)) %>%
    select(model_name, site, resolution, max_key_Rhat, is_converged)
  bm_sum_rows <- bm_model_rows %>%
    group_by(site) %>%
    summarize(model_name='Overall', num_models = length(resolution)) %>%
    ungroup()
  bob_maite <- full_join(bm_model_rows, select(filter(bm_sum_rows, num_models > 1), -num_models), by=c('site','model_name')) %>%
    arrange(site, model_name) %>%
    select(site, everything()) %>%
    mutate(confidence = NA)
  write.csv(bob_maite, outfile, row.names=FALSE)
}
