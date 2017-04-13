# helper function to summarize_run2_sorta_dones and summarize_run2_models
read_run2_files <- function(config.file='../2_metab_models/run2/out/config.tsv',
                            summary.dir='../2_metab_models/run2/out/summaries',
                            fit.dir='../2_metab_models/run2/out/fits') {
  # read the config file for this run
  config <- read_config(config.file)
  config$model_name <- make_metab_model_name(make_metab_run_title(format(as.Date(config$date), '%y%m%d'), config$tag, config$strategy), config$config.row, config$site)
  
  # get the names of larger output files, which will likely only be available on the computer where models were run/archived
  summary_files <- dir(summary.dir, full.names = TRUE)
  fit_files <- dir(fit.dir, full.names = TRUE)
  if(length(summary_files) == 0 || length(fit_files) == 0) {
    warning('summary_files or fit_files has length 0; this is the wrong computer for summarizing run2')
    return(FALSE)
  }
  
  return(list(config=config, summary_files=summary_files, fit_files=fit_files))
}

# report on the 8 models that got posted with empty fits in run2
summarize_run2_sorta_dones <- function(config.file='../2_metab_models/run2/out/config.tsv', outfile) {
  
  if(check_frozen(outfile)) return(NULL)
  
  run2_files <- read_run2_files(config.file)
  if(is.logical(run2_files) && !run2_files) return(FALSE)
  
  # there are 8 models with empty predictions and the fit$errors value, "dates have differing numbers of rows; observations cannot be combined in matrix"
  login_sb()
  mms <- list_metab_models('1.0.1')
  done_mms <- parse_metab_model_name(mms)$row
  done_sums <- parse_metab_model_name(substring(basename(summary_files), 9))$row
  done_fits <- parse_metab_model_name(substring(basename(fit_files), 5))$row
  setdiff(config$config.row, done_mms) # 0: everything is at least 'done'
  setdiff(done_mms, done_fits) # everything has a fit, but it's empty for the following 8 models:
  setdiff(done_mms, done_sums) # 36 179 180 188 283 284 336 342
  sorta_dones <- sapply(setdiff(done_mms, done_sums), function(row) grep(paste0('-',row,'-'), mms, value=TRUE))
  sorta_done_fits <- lapply(sorta_dones, function(sd) {
    mm <- get_metab_model(sd, version = 'original', update_sb = FALSE)
    filter(get_fit(mm)$daily, !is.na(GPP_daily_50pct))
  }) # every one is empty (all NAs)
  sorta_done_warns <- lapply(sorta_dones, function(sd) {
    mm <- get_metab_model(sd, version = 'original', update_sb = FALSE)
    get_fit(mm)$warnings
  }) # warnings about differing temporal resolutions
  sorta_done_errs <- lapply(sorta_dones, function(sd) {
    mm <- get_metab_model(sd, version = 'original', update_sb = FALSE)
    get_fit(mm)$errors
  }) # error "dates have differing numbers of rows; observations cannot be combined in matrix"
  
  writeLines(c(
    "These models are sorta done. Their model objects got posted, but their fits are empty:",
    capture.output(print(sorta_dones)),
    "\nHere are their fits:",
    capture.output(print(sorta_done_fits)),
    "\nHere are their warnings:",
    capture.output(print(sorta_done_warns)),
    "\nHere are their errors:",
    capture.output(print(sorta_done_errs))
  ), con=outfile)
  
}

# summarize the remaining models
summarize_run2_models <- function(config.file, outfile) {
  
  if(check_frozen(outfile)) return(NULL)
  
  summarize_run2_model <- function(config_row, outdir='../2_metab_models/run2/out/resummaries') {
    
    message('summarizing config_row ', config_row)
    
    # Prepare to save things
    if(!dir.exists(outdir)) dir.create(outdir)
    row_id <- filter(config, config.row==config_row) %>% .$model_name
    make_filename <- function(file_id, ext) {
      file.path(outdir, sprintf("%s %s.%s", row_id, file_id, ext))
    }
    
    #### Read in Files ####
    
    # Get old estimates and/or discharge
    old_estBest <- tryCatch(
      get_ts(c('sitedate_calcLon','gpp_estBest','er_estBest','K600_estBest','dischdaily_calcDMean'), config[config_row, 'site']) %>%
        unitted::v() %>% gather(param, value, gpp, er, K600, dischdaily),
      error=function(e) get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), config[config_row, 'site']) %>%
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
    
    # Read the summary file (daily params) if available
    summary_file <- grep(config$model_name[which(config$config.row == config_row)], summary_files, value=TRUE)
    if(length(summary_file) == 1) {
      smry <- read.table(summary_file, stringsAsFactors=FALSE, header=TRUE, sep="\t")
      new_daily <- smry %>%
        gather(param, value, GPP.daily, ER.daily, K600.daily) %>%
        filter(!is.na(value)) %>%
        mutate(
          date=as.Date(date), 
          param=unname(c('GPP.daily'='GPP','ER.daily'='ER','K600.daily'='K600')[param]),
          age='new') %>%
        select(date, param, age, value)
    } else if(length(summary_file) > 1) {
      stop("found more than 1 summary file for row ", config_row, ": ", paste0(summary_file, collapse=','))
    } else {
      stop("couldn't find a summary file for row ", config_row)
      new_daily <- NULL
    }
    
    # Combine old and new daily estimates, including a df that ignores outlier old
    # values relative to the new ones (for plotting)
    daily <- bind_rows(old_daily, new_daily)
    daily_newscale <- daily %>%
      group_by(param) %>%
      mutate(min=min(value[age == 'new']), max=max(value[age == 'new'])) %>%
      ungroup() %>%
      filter(param=='Q' | (min <= value & value <= max))
    
    # Read the fit file (all params, with Rhats) if available
    fit_file <- grep(config$model_name[which(config$config.row == config_row)], fit_files, value=TRUE)
    if(length(fit_file) == 1) {
      fit <- readRDS(fit_file) # it says .tsv but is really .RDS
    } else if(length(fit_file) > 1) {
      stop("found more than 1 fit file for row ", config_row, ": ", paste0(fit_file, collapse=','))
    } else {
      stop("couldn't find a fit file for row ", config_row)
      fit <- NULL
    }
    
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
        theme_bw() + ggtitle(row_id)
    }
    plot_daily <- fun_plot_daily(daily)
    ggsave(filename=make_filename('plot_daily', 'png'), plot=plot_daily, width=7, height=7)
    plot_daily_newscale <- fun_plot_daily(daily_newscale)
    ggsave(filename=make_filename('plot_daily_newscale', 'png'), plot=plot_daily_newscale, width=7, height=7)
    
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
      theme_bw() + coord_flip() + ggtitle(row_id)
    ggsave(filename=make_filename('plot_Rhats', 'png'), plot=plot_Rhats, width=7, height=7)
    
    # Summarize priors
    specs <- eval(parse(text=config$model_args[config$config.row == config_row]))$specs
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
    Kfit <- fit[[which(!names(fit) %in% c('daily','overall','warnings','errors'))]] %>%
      select(K600_lnQ_nodes_50pct=lnK600_lnQ_nodes_50pct, K600_lnQ_nodes_2.5pct=lnK600_lnQ_nodes_2.5pct, K600_lnQ_nodes_97.5pct=lnK600_lnQ_nodes_97.5pct)
    plot_dists <- suppressWarnings(cowplot::plot_grid(
      distpoints(Kfit, 'K600_lnQ_nodes'),
      distpoints(fit$overall, 'err_obs_iid_sigma') + xlab(row_id) + theme(axis.title.x=element_text(size=6)),
      distpoints(fit$overall, 'err_proc_iid_sigma'),
      distpoints(fit$overall, 'K600_daily_sigma'),
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
    K600_daily_sigma <- fit$overall$K600_daily_sigma_50pct
    plot_KQ <- ggplot(Kfit, aes(x=exp(K600_lnQ_nodes_centers), y=exp(K600_lnQ_nodes_50pct))) + 
      geom_ribbon(aes(ymin=exp(K600_lnQ_nodes_50pct)-K600_daily_sigma, ymax=exp(K600_lnQ_nodes_50pct)+K600_daily_sigma), color=NA, fill='turquoise2', alpha=0.1) +
      geom_line(color='turquoise1') + geom_point(size=3, color='turquoise2') +
      geom_point(data=daily_mat, aes(x=Q_new, y=K600_new), color='black', alpha=0.7, inherit.aes=FALSE) +
      scale_x_log10(breaks=logbreaksx) + scale_y_log10(breaks=logbreaksy) + 
      theme_bw() + theme(panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90)) +
      #annotate('text', x=labx, y=laby, label=sprintf('K600_daily_sigma = %0.03f', fit$overall$K600_daily_sigma_50pct)) + 
      xlab(parse(text='Discharge~(m^3~s^-1)')) + ylab(parse(text=sprintf('K600~(d^-1)~"     K600_daily_sigma = %0.03f"', fit$overall$K600_daily_sigma_50pct))) + 
      ggtitle(row_id)
    ggsave(filename=make_filename('plot_KQ', 'png'), plot=plot_KQ, width=6, height=6)
    
    # Combine all the numbers and add a handful more
    allstats <- bind_cols(ranges, correlations, Rhatsum, distsum) %>%
      mutate(
        model_name = row_id,
        K600_daily_sigma_sigma = specs$K600_daily_sigma_sigma
        # would love to add in runtime, but that takes longer b/c need to download the full model
      ) %>%
      select(model_name, everything())
    write.csv(allstats, make_filename('stats','csv'), row.names=FALSE)
  }
  stop_msgs <- c()
  for(cr in config$config.row[1]) {
    tryCatch(summarize_run2_model(cr), error=function(e) {
      stop_msgs <<- c(stop_msgs, setNames(e$message, cr))
      message('  error: ', e$message)
    })
  }
  # > t(t(stop_msgs))
  # [,1]                                                                                         
  # 36  "couldn't find a summary file for row 36"                                                    
  # 80  "no applicable method for 'select_' applied to an object of class \"c('double', 'numeric')\""
  # 179 "couldn't find a summary file for row 179"                                                   
  # 180 "couldn't find a summary file for row 180"                                                   
  # 188 "couldn't find a summary file for row 188"                                                   
  # 283 "couldn't find a summary file for row 283"                                                   
  # 284 "couldn't find a summary file for row 284"                                                   
  # 336 "couldn't find a summary file for row 336"                                                   
  # 342 "couldn't find a summary file for row 342"                                                   
  # 359 "no applicable method for 'select_' applied to an object of class \"c('double', 'numeric')\""
  # 360 "couldn't find a summary file for row 360"      
  writeLines(c("Errors in summarize_run2_model:", capture.output(print(t(t(stop_msgs))))[-1]), file.path(dirname(config.file), 'summary_errors.txt'))

  stats_csvs <- dir('../2_metab_models/out/resummaries', pattern=' stats.csv', full.names=TRUE)
  all_stats <- bind_rows(lapply(stats_csvs, function(sc) {
    read.csv(sc, header=TRUE, stringsAsFactors=FALSE)
  }))
  write.csv(all_stats, outfile, row.names=FALSE)
}  

summarize_run2_stats <- function(model.stats.file='../2_metab_models/run2/out/model_stats.csv', 
                                 outdir='../2_metab_models/run2/out/resummaries/') {
  
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

start_run2_expert_file <- function(config.file='../2_metab_models/run3/out/config.tsv',
                                   model.stats.file='../2_metab_models/run2/out/model_stats.csv', 
                                   outfile='../2_metab_models/run2/out/expert_file.csv') {
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
