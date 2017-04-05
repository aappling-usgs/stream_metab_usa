#' Use the prep run to choose parameters for the main, Bayesian run
#' 
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import streamMetabolizer
#' @import ggplot2
#' @param prepdir the location of the output files (in their directories as
#'   copied over from condor)
#' @param outdir the directory in which to save the params and plots we create
choose_params <- function(prepdir="../2_metab_config/run1/out/", 
                          outdir="../2_metab_config/run1/out") {
  
  # collect and combine summary data.frames from the jobs
  resultsdirs <- grep('results_', dir(prepdir, full.names=TRUE), value=TRUE)
  results <- bind_rows(lapply(resultsdirs, function(resdir) {
    jobdirs <- grep('job_', dir(resdir, full.names=TRUE), value=TRUE)
    if(length(jobdirs) > 0) {
      bind_rows(lapply(jobdirs, function(jobdir) {
        sumfile <- grep('summary ', dir(jobdir, full.names=TRUE), value=TRUE)
        if(length(sumfile) > 0) {
          smry <- read.table(sumfile, header=TRUE, sep='\t', stringsAsFactors=FALSE)
          select(smry, site_name, date, K600.daily, K600.daily.sd, discharge.daily, data_ply_nrow, timestep_days, code, ply_validity)
        } else NULL
      }))
    } else NULL
  }))
  saveRDS(results, file.path(outdir, 'results.Rds'))
  
  # compute metrics for timestep, K, and Q
  resnest <- results %>%
    group_by(site_name) %>%
    nest() %>%
    # summarize the timesteps
    mutate(Tsteps = lapply(data, function(dat) {
      dat %>%
        filter(ply_validity == 'TRUE') %>%
        group_by(timestep_days) %>%
        summarize(
          num_dates = length(date),
          start_date = min(date),
          end_date = max(date)
        ) %>%
        mutate(
          num_tsteps = length(num_dates),
          timestep_mins = round(timestep_days * 24 * 60, 0.1),
          start_Date = as.Date(start_date),
          end_Date = as.Date(end_date))
    })) %>%
    mutate(Tstats = lapply(Tsteps, function(Ts) {
      tsdat <- Ts %>% filter(num_dates >= 30) # decision: don't model data chunks with fewer than 30 dates
      data_frame(
        tsteps_min = paste0(tsdat$timestep_mins, collapse=','),
        modal_tstep = tsdat$timestep_mins[which.max(tsdat$num_dates)][1]
      )
    })) %>%
    # summarize the median Ks
    mutate(Kstats = lapply(data, function(dat) {
      dat %>%
        filter(!is.na(K600.daily), K600.daily.sd/K600.daily < 0.5) %>% #, K600.daily > 0) %>%
        summarize(
          n = length(K600.daily),
          K600_min = min(K600.daily),
          K600_q25 = quantile(K600.daily, 0.25),
          K600_med = median(K600.daily),
          K600_q50 = quantile(K600.daily, 0.50),
          K600_q75 = quantile(K600.daily, 0.75),
          K600_max = max(K600.daily))
    })) %>%
    # summarize the discharges
    mutate(Qbins = lapply(data, function(dat) {
      logeQ <- filter(dat, ply_validity == 'TRUE')$discharge.daily
      logeQ <- if(length(!is.na(logeQ))) na.omit(log(logeQ))
      if(length(logeQ) > 0) {
        calc_bins(logeQ, "width", width=0.2, center=0) # decision: bin width of 0.2 natural log units
      } else {
        list(vec=1, bounds=c(0,0), names='NA')
      }
    })) %>%
    mutate(Qbounds = lapply(Qbins, function(bins) {
      data_frame(Qnodemin=min(bins$bounds), Qnodemax=max(bins$bounds))
    })) %>%
    mutate(Qstats = lapply(Qbins, function(bins) {
      bintable <- as_data_frame(table(bins$names[bins$vec])) %>% setNames(c('name', 'nbin'))
      data_frame(
        name = bins$names,
        lbound = bins$bounds[-length(bins$bounds)],
        ubound = bins$bounds[-1],
        center = (lbound + ubound)/2,
        median = median(bins$bounds),
        range = max(bins$bounds) - min(bins$bounds),
        numbins = length(bins$bounds)) %>%
        left_join(bintable, by='name') %>%
        mutate(nbin = replace(nbin, is.na(nbin), 0))
    }))
  saveRDS(resnest, file.path(outdir, 'results_nested.Rds'))
  
  # write out the essential info for creating the next config file
  params <- resnest %>%
    unnest(Tstats, Kstats, Qbounds) %>%
    select(site_name, tsteps_min, K600_med, Qnodemin, Qnodemax) %>%
    mutate(K600_med = format(K600_med, digits=3),
           Qnodemin = format(Qnodemin, digits=2),
           Qnodemax = format(Qnodemax, digits=2))
  write.table(params, file.path(outdir, 'params.tsv'), row.names=FALSE, sep='\t', quote=FALSE)
  
  # plot the timesteps
  tsteps <- unnest(resnest, Tsteps)
  g <- ggplot(filter(tsteps, num_tsteps > 1), aes(x=site_name, color=as.factor(timestep_mins))) + 
    geom_errorbar(aes(ymin=start_Date, ymax=end_Date), size=1) +
    scale_color_discrete('Timestep (mins)') +
    theme_bw() +
    ylab('Date') + xlab('Site') +
    theme(axis.text.x=element_blank(), legend.position='bottom') +
    guides(color = guide_legend(nrow = 1)) +
    ggtitle("T: Temporal coverage by timestep length: sites with >1 unique timestep")
  ggsave(file.path(outdir, 'T_by_date_multiT.png'), plot=g, width=8, height=5, dpi=300)
  
  g <- ggplot(filter(tsteps, num_tsteps <= 1), aes(x=site_name, color=as.factor(timestep_mins))) + 
    geom_errorbar(aes(ymin=start_Date, ymax=end_Date)) +
    scale_color_discrete('Timestep (mins)') +
    theme_bw() +
    ylab('Date') + xlab('Site') +
    theme(axis.text.x=element_blank(), legend.position='bottom') +
    guides(color = guide_legend(nrow = 1)) +
    ggtitle("T: Temporal coverage by timestep length: sites with <=1 unique timestep")
  ggsave(file.path(outdir, 'T_by_date_singleT.png'), plot=g, width=8, height=5, dpi=300)
  
  # plot the median Ks
  g <- unnest(resnest, Kstats) %>%
    ggplot(aes(x=n, y=K600_med)) + geom_point() +
    geom_errorbar(aes(ymin=pmax(K600_q25, 0.1), ymax=K600_q75)) + 
    scale_y_log10(breaks=10^(-1:4), labels=sprintf('%5.1f', 10^(-1:4))) + 
    theme_bw() +
    ylab('Median K600') + xlab('Number of daily Ks') +
    ggtitle("K: K600 after filtering to sd/K < 0.5")
  ggsave(file.path(outdir, 'K_by_n.png'), plot=g, width=8, height=4, dpi=300)
  
  g <- unnest(resnest, Kstats) %>%
    arrange(K600_med) %>%
    mutate(site_order = 1:n()) %>%
    ggplot(aes(x=site_order, y=K600_med, color=log(n))) + geom_point() +
    geom_errorbar(aes(ymin=pmax(K600_q25, 0.1), ymax=K600_q75)) + 
    scale_y_log10(breaks=10^(-1:4), labels=sprintf('%5.1f', 10^(-1:4))) + 
    scale_color_continuous("Log number of daily Ks per site") +
    theme_bw() +
    ylab('Median K600 and interquartile range') + xlab('Site, ordered by median K600') + 
    theme(axis.text.x=element_blank(), legend.position='bottom') +
    ggtitle("K: K600 after filtering to sd/K < 0.5")
  ggsave(file.path(outdir, 'K_by_Korder.png'), plot=g, width=8, height=4, dpi=300)
  
  g <- unnest(resnest, Kstats) %>%
    ggplot(aes(x=K600_med)) + geom_density(fill='lightgrey') + 
    geom_rug(sides='b') + 
    scale_x_log10(breaks=c(0.5,1,5,10,50,100)) + theme_bw() +
    xlab("Median K600 (1/d) for each site") + ylab("Density") +
    ggtitle("K: Density of median Ks")
  ggsave(file.path(outdir, 'K_density.png'), plot=g, width=6, height=4, dpi=300)
  
  # plot the discharges
  g <- resnest %>%
    mutate(Q_med = sapply(Qstats, function(qs) unique(qs$median))) %>%
    arrange(Q_med) %>%
    mutate(site_order = 1:n()) %>%
    unnest(Qstats) %>%
    ggplot(aes(x=site_order, y=exp(center), color=log(nbin))) + 
    geom_point(size=0.6, shape=15) + scale_y_log10() + theme_bw() +
    ylab('Bin Centers (m^3 s^-1)') + xlab('Site') + theme(axis.text.x=element_blank()) +
    ggtitle("Q: Bin Qs and sizes, sites arranged by median Q")
  ggsave(file.path(outdir, 'Q_by_Qorder.png'), plot=g, width=8, height=4, dpi=300)
  
  g <- unnest(resnest, Qstats) %>%
    ggplot(aes(x=nbin)) + geom_histogram(binwidth=5) + #scale_y_log10() +
    geom_rug() + theme_bw() +
    xlab("Number (N) of days per Q bin") + ylab("Number of bins having N days/bin") +
    ggtitle('Q: Histogram of bin sizes')
  ggsave(file.path(outdir, 'Q_histogram.png'), plot=g, width=8, height=4, dpi=300)
  
  g <- resnest %>%
    mutate(Q_med = sapply(Qstats, function(qs) unique(qs$median))) %>%
    unnest(Qstats) %>%
    ggplot(aes(x=exp(Q_med), y=numbins)) + geom_point() + theme_bw() +
    scale_x_log10() +
    xlab("Median Q of each site (m^3 s^-1)") + 
    ylab("Number of bins per site") +
    ggtitle('Q: Number of bins per site, by median Q')
  ggsave(file.path(outdir, 'Qnumbins_vs_Qmedian.png'), plot=g, width=6, height=4, dpi=300)
}
