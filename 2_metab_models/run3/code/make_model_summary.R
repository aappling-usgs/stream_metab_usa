#' make a bunch of model summary files: diagnostic plots, statistics,
#' comparisons to old estBest estimates, etc.
make_model_summary <- function(model_out, outdir) {
  
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
    
  # Prepare to save things
  if(!dir.exists(outdir)) dir.create(outdir)
  make_filename <- function(file_id, ext) {
    file.path(outdir, sprintf("%s %s.%s", config_row$model_name, file_id, ext))
  }
  
  #### Read in Files ####
  
  # Get old estimates and/or discharge
  old_estBest <- tryCatch(
    get_ts(c('sitedate_calcLon','gpp_estBest','er_estBest','K600_estBest','dischdaily_calcDMean'), config_row$site) %>%
      unitted::v() %>% gather(param, value, gpp, er, K600, dischdaily),
    error=function(e) get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), config_row$site) %>%
      unitted::v() %>% gather(param, value, dischdaily)
  )
  old_daily <- old_estBest %>%
    filter(!is.na(value)) %>%
    mutate(
      date=as.Date(sitedate), 
      param=unname(c(gpp='GPP',er='ER',K600='K600',dischdaily='Q')[param]),
      age=ifelse(param=='Q','new','old')
    ) %>%
    select(date, param, age, value)
  write.csv(old_daily, make_filename('old_daily','csv'), row.names=FALSE)
  
  # Read the daily params if available
  pars <- get_params(model_out, uncertainty='ci')
  pars$site <- config_row$site
  pars$row <- config_row$config.row
  smry <- pars %>%
    select(site_name=site, config_row=row, everything())
  new_daily <- smry %>%
    gather(param, value, GPP.daily, ER.daily, K600.daily) %>%
    filter(!is.na(value)) %>%
    mutate(
      date=as.Date(date), 
      param=unname(c('GPP.daily'='GPP','ER.daily'='ER','K600.daily'='K600')[param]),
      age='new') %>%
    select(date, param, age, value)
  
  # Combine old and new daily estimates, including a df that ignores outlier old
  # values relative to the new ones (for plotting)
  daily <- bind_rows(old_daily, new_daily)
  daily_newscale <- daily %>%
    group_by(param) %>%
    mutate(min=min(value[age == 'new']), max=max(value[age == 'new'])) %>%
    ungroup() %>%
    filter(param=='Q' | (min <= value & value <= max))
  
  # Read the fit file (all params, with Rhats) if available
  fit <- get_fit(model_out)

  #### Plots and Stats ####
  
  # Plot daily values as time series
  fun_plot_daily <- function(daily) {
    daily %>%
      mutate(
        age=ordered(age, c('old','new')),
        param = ordered(param, c('GPP','ER','K600','Q','Warning','Error')),
        param_age = paste0(param, '_', age)) %>%
      ggplot(aes(x=date, y=value, color=param_age, shape=age)) + geom_point() +
      facet_grid(param ~ ., scales='free_y') +
      scale_shape_manual(guide='none', values=c(old=4, new=1)) +
      scale_color_manual(values=c(GPP_old='seagreen1', ER_old='tomato1', K600_old='turquoise1', Q_old='royalblue4',
                                  GPP_new='seagreen4', ER_new='tomato4', K600_new='turquoise4', Q_new='royalblue4')) +
      theme_bw() + ggtitle(config_row$model_name)
  }
  plot_daily <- fun_plot_daily(daily)
  ggsave(filename=make_filename('plot_daily_newold', 'png'), plot=plot_daily, width=7, height=7)
  plot_daily_newscale <- fun_plot_daily(daily_newscale)
  ggsave(filename=make_filename('plot_daily_newscale', 'png'), plot=plot_daily_newscale, width=7, height=7)
  plot_daily_new <- fun_plot_daily(new_daily)
  ggsave(filename=make_filename('plot_daily', 'png'), plot=plot_daily_new, width=7, height=7)
  
  # Calculate correlations among daily values
  daily_mat <- daily %>%
    mutate(par_age = paste(param, age, sep='_')) %>%
    select(-param, -age) %>%
    spread(par_age, value)
  if('GPP_old' %in% daily_mat) {
    correlations <- data_frame(
      cor.GPP_newold = cor(daily_mat$GPP_new, daily_mat$GPP_old, use='complete.obs', method='kendall'),
      cor.ER_newold = cor(daily_mat$ER_new, daily_mat$ER_old, use='complete.obs', method='kendall'),
      cor.K600_newold = cor(daily_mat$K600_new, daily_mat$K600_old, use='complete.obs', method='kendall'),
      cor.K600ER_old = cor(daily_mat$K600_old, daily_mat$ER_old, use='complete.obs', method='kendall'),
      cor.K600ER_new = cor(daily_mat$K600_new, daily_mat$ER_new, use='complete.obs', method='kendall'),
      cor.K600GPP_old = cor(daily_mat$K600_old, daily_mat$GPP_old, use='complete.obs', method='kendall'),
      cor.K600GPP_new = cor(daily_mat$K600_new, daily_mat$GPP_new, use='complete.obs', method='kendall'),
      cor.GPPER_old = cor(daily_mat$GPP_old, daily_mat$ER_old, use='complete.obs', method='kendall'),
      cor.GPPER_new = cor(daily_mat$GPP_new, daily_mat$ER_new, use='complete.obs', method='kendall')
    ) %>% as.data.frame(stringsAsFactors=FALSE)
  } else {
    correlations <- data_frame(
      cor.K600ER_new = cor(daily_mat$K600_new, daily_mat$ER_new, use='complete.obs', method='kendall'),
      cor.K600GPP_new = cor(daily_mat$K600_new, daily_mat$GPP_new, use='complete.obs', method='kendall'),
      cor.GPPER_new = cor(daily_mat$GPP_new, daily_mat$ER_new, use='complete.obs', method='kendall')
    ) %>% as.data.frame(stringsAsFactors=FALSE)
  }
  
  # Calculate ranges of values
  functions <- list(
    min = function(x) min(x, na.rm=TRUE),
    max = function(x) max(x, na.rm=TRUE),
    median = function(x) median(x, na.rm=TRUE),
    mean = function(x) mean(x, na.rm=TRUE),
    n = function(x) length(na.omit(x)),
    pct_pos = function(x) 100 * length(na.omit(x[x>0])) / length(na.omit(x))
  )
  ranges <- bind_rows(lapply(functions[1:6], function(fun) summarize_all(select(daily_mat, -date, -Q_new), fun))) %>%
    mutate(stat=names(functions)) %>%
    select(stat, everything()) %>%
    gather(param, value, -stat) %>%
    mutate(param_stat=paste0(param, '.', stat)) %>%
    select(-stat, -param) %>%
    spread(param_stat, value)
  
  # Summarize warnings and errors
  messages <- capture.output({
    cat('## Global Warnings ##\n')
    cat(fit$warnings, sep='\n')
    cat('\n## Global Errors ##\n')
    cat(fit$errors, sep='\n')
    cat('\n## Daily Warnings ##\n')
    cat(streamMetabolizer:::summarize_stopwarn_msgs(fit$daily$warnings), sep='\n')
    cat('\n## Daily Errors ##\n')
    cat(streamMetabolizer:::summarize_stopwarn_msgs(fit$daily$errors), sep='\n')
  })
  writeLines(messages, con=make_filename('messages','txt'))
  
  # Summarize Rhats
  data_fits <- fit[!names(fit) %in% c('warnings','errors')]
  Rhatvecs <- do.call(c, unname(lapply(data_fits, function(df) {
    select(df, ends_with('Rhat')) %>%
      as.list()
  })))
  Rhats <- bind_rows(lapply(names(Rhatvecs), function(parname) {
    data_frame(param=parname, Rhat=Rhatvecs[[parname]])
  })) %>%
    group_by(param) %>% 
    mutate(n=n(), count=ifelse(n() == 1, 'one', 'many')) %>% 
    ungroup()
  Rhats$param <- ordered(Rhats$param, distinct(select(Rhats, param, n)) %>% arrange(n) %>% .$param)
  Rhatsum <- Rhats %>%
    filter(!is.na(Rhat)) %>%
    group_by(param) %>%
    summarize(
      min=min(Rhat),
      max=max(Rhat),
      mean=mean(Rhat),
      median=median(Rhat),
      q95=quantile(Rhat, probs=0.95),
      n=n[1],
      n_over_1.05=length(which(Rhat > 1.05)),
      n_over_1.1=length(which(Rhat > 1.1)),
      pct_over_1.05=100*n_over_1.05/n,
      pct_over_1.1=100*n_over_1.1/n
    ) %>%
    mutate(param=as.character(param))
  Rhatsum <- bind_rows(
    Rhatsum, 
    summarize(
      Rhatsum, 
      param='all_params_Rhat',
      min=min(min),
      max=max(max),
      mean=mean(mean),
      median=median(median),
      meanq95=mean(q95), # calculate this before modifying q95
      q95=quantile(Rhats$Rhat[!is.na(Rhats$Rhat)], probs=0.95),
      meanpct_over_1.05 = mean(pct_over_1.05), # calculate this before modifying pct_over_1.05 below
      meanpct_over_1.1 = mean(pct_over_1.1),
      npar = n(), # the number of parameter types/names (9), not the number of values (way more),
      n=sum(n),
      n_over_1.05=sum(n_over_1.05),
      n_over_1.1=sum(n_over_1.1),
      pct_over_1.05=100*n_over_1.05/n,
      pct_over_1.1=100*n_over_1.1/n
    ))
  Rhatsum <- Rhatsum %>%
    gather(stat, value, -param) %>%
    mutate(param_stat=paste0(param, '.', stat)) %>%
    select(-stat, -param) %>%
    spread(param_stat, value)
  Rhatsum <- Rhatsum %>%
    select_(.dots=names(which(!sapply(Rhatsum, is.na))))
  
  # Plot Rhats
  plot_Rhats <- ggplot(Rhats, aes(x=param, y=Rhat)) + 
    geom_violin(fill='skyblue', color=NA, scale='width') + 
    geom_hline(yintercept=1, color='black') +
    geom_hline(yintercept=1.05, color='red') +
    geom_point(aes(size=count), alpha=0.3, position=position_jitter(width=0.2)) +
    scale_size_discrete(guide='none') + xlab('') +
    theme_bw() + coord_flip() + ggtitle(config_row$model_name)
  ggsave(filename=make_filename('plot_Rhats', 'png'), plot=plot_Rhats, width=7, height=7)
  
  # Summarize priors
  specs <- get_specs(model_out)
  pars <- c('GPP_daily','ER_daily','lnK600_lnQ_nodes','err_obs_iid_sigma','err_proc_iid_sigma','K600_daily_sigma')
  posteriors <- bind_rows(lapply(
    pars,
    function(param) {
      fitname <- names(which(sapply(fit, function(f) length(grep(param, names(f))) > 0)))
      fitdm <- fit[[fitname]] %>%
        select_(paste0(param, '_50pct'), paste0(param, '_2.5pct'), paste0(param, '_97.5pct')) %>%
        setNames(c('post50','post2.5','post97.5')) %>%
        filter(!is.na(post50)) %>%
        mutate(param=param)
    }))
  priors <- bind_rows(lapply(
    pars,
    function(param) {
      as.data.frame(as.list(switch(
        param,
        GPP_daily = setNames(specs[c('GPP_daily_mu','GPP_daily_sigma')], c('mu','sigma')),
        ER_daily = setNames(specs[c('ER_daily_mu','ER_daily_sigma')], c('mu','sigma')),
        lnK600_lnQ_nodes = setNames(specs[c('K600_lnQ_nodes_meanlog','K600_lnQ_nodes_sdlog')], c('meanlog','sdlog')),
        err_obs_iid_sigma = setNames(specs[c('err_obs_iid_sigma_scale')], 'scale'),
        err_proc_iid_sigma = setNames(specs[c('err_proc_iid_sigma_scale')], 'scale'),
        K600_daily_sigma = setNames(specs[c('K600_daily_sigma_sigma')], 'sigma')
      ))) %>% distinct() %>% mutate(param=param)
    })) %>%
    select(param, everything())
  distsum <- posteriors %>%
    group_by(param) %>%
    summarize(
      min.p50 = min(post50),
      median.p50 = median(post50),
      max.p50 = max(post50)) %>%
    full_join(priors, by='param') %>%
    gather(stat, value, -param) %>%
    mutate(param_stat=paste0(param, '.', stat)) %>%
    select(-stat, -param) %>%
    spread(param_stat, value)
  
  # Plot priors & posteriors
  distpoints <- function(fitdf, param='K600_daily_sigma', index=TRUE) {
    g <- plot_distribs(specs, param, index=index, style='ggplot2')
    y.range <- ggplot_build(g)$layout$panel_ranges[[1]]$y.range %>% {mean(.) + c(-0.9,0.9)*diff(.)/2} 
    fitdm <- select_(fitdf, paste0(param, '_50pct'), paste0(param, '_2.5pct'), paste0(param, '_97.5pct')) %>%
      setNames(c('p50','p2.5','p97.5')) %>%
      mutate(y=if(length(p50) == 1) mean(y.range) else seq(y.range[1], y.range[2], length.out=length(p50)))
    if(param == 'K600_lnQ_nodes') fitdm <- mutate(fitdm, p2.5=exp(p2.5), p50=exp(p50), p97.5=exp(p97.5))
    g + 
      geom_point(data=fitdm, aes(x=p50, y=y), inherit.aes = FALSE) + 
      geom_errorbarh(data=fitdm, aes(y=y, xmin=p2.5, x=p50, xmax=p97.5), 
                     height=0, inherit.aes=FALSE) +
      guides(fill=FALSE, color=FALSE) + ylab('') + xlab('')
  }
  Kfit <- fit$KQ_binned %>%
    select(K600_lnQ_nodes_50pct=lnK600_lnQ_nodes_50pct, K600_lnQ_nodes_2.5pct=lnK600_lnQ_nodes_2.5pct, K600_lnQ_nodes_97.5pct=lnK600_lnQ_nodes_97.5pct)
  plot_dists <- suppressWarnings(cowplot::plot_grid(
    distpoints(Kfit, 'K600_lnQ_nodes'),
    distpoints(fit$overall, 'err_obs_iid_sigma') + xlab(config_row$model_name) + theme(axis.title.x=element_text(size=6)),
    distpoints(fit$overall, 'err_proc_iid_sigma'),
    distpoints(fit$KQ_overall, 'K600_daily_sigma'),
    distpoints(fit$daily, 'GPP_daily'),
    distpoints(fit$daily, 'ER_daily')
  ))
  ggsave(filename=make_filename('plot_dists', 'png'), plot=plot_dists, width=7, height=6)
  
  # Plot daily K vs Q with the piecewise linear relationship plotted underneath
  Kfit <- Kfit %>% mutate(K600_lnQ_nodes_centers = specs$K600_lnQ_nodes_centers)
  labx <- min(daily_mat$K600_new) + 0.5*diff(range(daily_mat$K600_new))
  laby <- max(daily_mat$Q_new) - 0.1*diff(range(daily_mat$Q_new))
  logbreaks <- rep(c(0.001,0.01,0.1,1,10,100), each=3) * rep(c(1,3,10), times=6)
  logbreaksx <- sort(c(logbreaks, signif(exp(range(Kfit$K600_lnQ_nodes_centers)), digits=2)))
  logbreaksy <- sort(c(logbreaks, signif(exp(range(Kfit$K600_lnQ_nodes_50pct)), digits=2)))
  K600_daily_sigma <- fit$KQ_overall$K600_daily_sigma_50pct
  plot_KQ <- ggplot(Kfit, aes(x=exp(K600_lnQ_nodes_centers), y=exp(K600_lnQ_nodes_50pct))) + 
    geom_ribbon(aes(ymin=exp(K600_lnQ_nodes_50pct)-K600_daily_sigma, ymax=exp(K600_lnQ_nodes_50pct)+K600_daily_sigma), color=NA, fill='turquoise2', alpha=0.1) +
    geom_line(color='turquoise1') + geom_point(size=3, color='turquoise2') +
    geom_point(data=daily_mat, aes(x=Q_new, y=K600_new), color='black', alpha=0.7, inherit.aes=FALSE) +
    scale_x_log10(breaks=logbreaksx) + scale_y_log10(breaks=logbreaksy) + 
    theme_bw() + theme(panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90)) +
    #annotate('text', x=labx, y=laby, label=sprintf('K600_daily_sigma = %0.03f', fit$overall$K600_daily_sigma_50pct)) + 
    xlab(parse(text='Discharge~(m^3~s^-1)')) + ylab(parse(text=sprintf('K600~(d^-1)~"     K600_daily_sigma = %0.03f"', fit$KQ_overall$K600_daily_sigma_50pct))) + 
    ggtitle(config_row$model_name)
  ggsave(filename=make_filename('plot_KQ', 'png'), plot=plot_KQ, width=6, height=6)
  
  # Combine all the numbers and add a handful more
  allstats <- bind_cols(ranges, correlations, Rhatsum, distsum) %>%
    mutate(
      model_name = config_row$model_name,
      K600_daily_sigma_sigma = specs$K600_daily_sigma_sigma,
      runtime = get_fitting_time(model_out)[['elapsed']]
    ) %>%
    select(model_name, everything())
  write.csv(allstats, make_filename('stats','csv'), row.names=FALSE)
}
