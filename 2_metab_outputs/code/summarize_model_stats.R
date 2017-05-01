
summarize_model_stats <- function(model.stats.file='../2_metab_outputs/out/model_stats.tsv',
                                  outdir='../2_metab_outputs/out/model_stats/') {
  
  all_stats <- read.table(model.stats.file, header=TRUE, stringsAsFactors=FALSE, sep='\t')
  if(!dir.exists(outdir)) dir.create(outdir)
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # plot convergence, runtime, resolution, number of iterations, and number of
  # input days. resolution has a strong effect on runtime.
  plot_convergence <- ggplot(all_stats, aes(x=num_days, y=runtime/(60*60*24), color=resolution, alpha=is_converged)) + 
    geom_point() + facet_grid(saved_steps ~ .)
  ggsave(file.path(outdir, 'plot_convergence.png'), plot_convergence, width=7, height=7)
  
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

