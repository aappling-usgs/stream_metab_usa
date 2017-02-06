# install packages from the local repo
install.packages(
  c('streamMetabolizer', 'dplyr', 'tidyr', 'ggplot2', 'unitted', 'devtools'),
  repo="file:bundle",type="source", dependencies=c("Depends","Imports"), lib='rLibs')
devtools::session_info()

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1])+1 # 0-indexed by HTCondor; convert to 1-indexed

# run model
run_sdh_test <- function(job, outdir) {
  
  # load the basics
  library(streamMetabolizer)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  inputs <- readRDS('inputs.Rds')
  config <- readRDS('config.Rds')
  
  # convert job index into data and model indices
  i <- ceiling(job/nrow(config))
  m <- (job-1) %% nrow(config) + 1
  
  # get input data
  dname <- names(inputs)[i]
  dat <- inputs[[dname]]$dat
  dlab <- sprintf('d%s_%s', i, dname)
  
  message(sprintf('input %d: %s \tmodel %d: %s \ttime: %s', i, dname, m, config$model[m], Sys.time()))
  message('  ', paste0(sapply(names(config), function(nm) paste0(nm, '=', config[m,nm])), collapse=', '))
  
  # temporary for testing
  # dat <- dat[1:150,]
  # message('temporary!!')
  
  # get reasonable inputs for discharge nodes and 
  qbins <- calc_bins(log(dat$discharge), 'interval', n=5)$bounds
  kmle <- median(na.omit(get_params(metab(specs('mle'), select(dat, -discharge)))$K600.daily))
  
  # run models
  sdlab <- sprintf('d%s_m%d_%s_%1.2f_%1.1f', i, m, substr(config$model[m], 3, 5), config$sd_val[m], config$err_proc_iid_sigma_scale[m])
  rdsfile <- sprintf('outputs_%s.Rds', sdlab)
  
  # fit the model using all the default specs
  sp <- specs(config$model[m])
  sp[[config$sd_var[m]]] <- kmle*config$sd_val[m]
  sp <- revise(
    sp, 
    K600_lnQ_nodes_centers=qbins,
    params_out=setdiff(params_out, c('err_obs_iid', 'err_proc_iid', 'GPP', 'ER')))
  mm <- metab(sp, dat)
  
  # save the outputs
  disch <- get_data_daily(mm) %>% select(date, discharge.daily)
  param_preds <- get_params(mm)
  metab_preds <- predict_metab(mm)
  DO_preds <- predict_DO(mm)
  
  # plot DO preds
  g <- plot_DO_preds(DO_preds) + ggtitle(sdlab)
  ggsave(filename=sprintf(file.path(outdir, 'DO_%s.png'), sdlab), plot=g, width=6, height=6, units='in')
  
  # plot params & daily discharge
  disch <- get_data_daily(mm) %>% select(date, discharge.daily)
  param_preds <- get_params(mm)
  g <- full_join(param_preds, disch, by='date') %>% 
    gather(variable, value, GPP.daily, ER.daily, K600.daily, discharge.daily) %>%
    mutate(variable=ordered(variable, levels=c('GPP.daily','ER.daily','K600.daily','discharge.daily')),
           rightsign=(variable=='ER.daily' & value < 0) | value > 0) %>%
    ggplot(aes(x=date, y=value, color=rightsign)) + geom_point() + 
    facet_grid(variable ~ ., scales='free_y') + ggtitle(sdlab) + theme_bw()
  ggsave(filename=sprintf(file.path(outdir, 'params_%s.png'), sdlab), plot=g, width=6, height=6, units='in')
  
  # plot distribs
  disttitle <- paste0(sdlab, ': ', mm@specs$model_name)
  if(mm_parse_name(mm@specs$model_name, expand=TRUE)$pool_K600_sd == 'fitted') {
    g <- plot_distribs(mm, 'K600_daily_sigma', style='ggplot2') + ggtitle('K600_daily_sigma', subtitle=disttitle)
    ggsave(filename=sprintf(file.path(outdir, 'distrib_Kds_%s.png'), sdlab), plot=g, width=6, height=3, units='in')
  }
  if(mm_parse_name(mm@specs$model_name)$err_obs_iid) {
    g <- plot_distribs(mm, 'err_obs_iid_sigma', style='ggplot2') + ggtitle('err_obs_iid_sigma', subtitle=disttitle)
    ggsave(filename=sprintf(file.path(outdir, 'distrib_oi_%s.png'), sdlab), plot=g, width=6, height=3, units='in')
  }
  if(mm_parse_name(mm@specs$model_name)$err_proc_iid) {
    g <- plot_distribs(mm, 'err_proc_iid_sigma', style='ggplot2') + ggtitle('err_proc_iid_sigma', subtitle=disttitle)
    ggsave(filename=sprintf(file.path(outdir, 'distrib_pi_%s.png'), sdlab), plot=g, width=6, height=3, units='in')
  }
  
  message(paste0('  compile_time: ', mm@compile_time[['elapsed']], ', fitting_time: ', mm@fitting_time[['elapsed']]))
  
  # save everything except the really big stuff
  mm@mcmc <- mm@mcmc_data <- NULL
  saveRDS(list(m=m, dname=dname, i=i, param_preds=param_preds, metab_preds=metab_preds, DO_preds=DO_preds, fit_model=mm), 
          file=file.path(outdir, rdsfile))
  
  message(sprintf('  done with input %d: %s \tmodel %d: %s \ttime: %s', i, dname, m, config$model[m], Sys.time()))
}
outdir <- 'results' # sprintf('results/job_%d', job)
if(!dir.exists(outdir)) dir.create(outdir)
for(job in 1:32) {
  run_sdh_test(job, outdir)
}

# package output
writeLines('this is where the data go', 'trends.zip')