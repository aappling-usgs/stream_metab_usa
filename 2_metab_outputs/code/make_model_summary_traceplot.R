library(mda.streams)
library(streamMetabolizer)
library(ggplot2)
library(rstan)
login_sb()
mms <- list_metab_models('1.0.20')

for(i in 1:length(mms)) {
  model_name <- mms[i]
  # sb_item <- locate_metab_model(model_name)
  # out <- sbtools::item_file_download(sb_item, dest_dir=tempdir())
  model_name_orig <- gsub('1.0.20', '1.0.2', model_name, fixed=TRUE)
  out <- grep(model_name_orig, dir('../temp', full.names=TRUE), value=TRUE)
  mm <- get(load(out))
  # rstan::traceplot(get_mcmc(mm), c('err_obs_iid_sigma','err_proc_iid_sigma','K600_daily_sigma'), nrow=3) + ggtitle(model_name_orig)
  # ggsave(sprintf('../2_metab_outputs/out/summaries/%s traceplot_saved.png', model_name_orig), height=7, width=7)
  # rstan::traceplot(get_mcmc(mm), c('err_obs_iid_sigma','err_proc_iid_sigma','K600_daily_sigma'), nrow=3, inc_warmup=TRUE) + ggtitle(model_name_orig)
  # ggsave(sprintf('../2_metab_outputs/out/summaries/%s traceplot_all.png', model_name_orig), height=7, width=7)
  rstan:::pairs.stanfit(get_mcmc(mm), pars=c('err_obs_iid_sigma','err_proc_iid_sigma','K600_daily_sigma'))
  cat(sprintf('%s pairplot_saved.png', model_name_orig))
  ggsave(sprintf('../2_metab_outputs/out/summaries/%s pairplot_saved.png', model_name_orig), height=7, width=7)
}
