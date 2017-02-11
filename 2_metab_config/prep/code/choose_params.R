#' Use the prep run to choose parameters for the main, Bayesian run
#' 
#' @import tidyr
#' @import dplyr
#' @import streamMetabolizer
#' @import ggplot2
#' @import ggExtra
#' @param prepdir the location of the output files (in their directories as copied over from condor)
create_prep_config <- function(prepdir="../2_metab_config/prep/out/", 
                               smu.config=yaml::yaml.load_file('../2_metab_config/in/metab_configs_config.yml'),
                               outfile="../2_metab_config/prep/out/params.tsv") {
  
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
  
  # compute metrics for K and Q
  resnest <- results %>%
    group_by(site_name) %>%
    nest() %>%
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
    mutate(Qstats = lapply(data, function(dat) {
      bins <- calc_bins(log(dat$discharge.daily), "width", width=0.2, center=0)
      bintable <- as_data_frame(table(bins$names[bins$vec])) %>% setNames(c('name', 'nbin'))
      data_frame(
        name = bins$names,
        lbound = bins$bounds[-length(bins$bounds)],
        ubound = bins$bounds[-1],
        center = (lbound + ubound)/2,
        median = median(bins$bounds),
        ntot = nrow(dat)) %>%
        left_join(bintable, by='name') %>%
        mutate(nbin = replace(nbin, is.na(nbin), 0))
    }))
  
  library(ggplot2)
  library(ggExtra)
  
  # plot the median Ks
  unnest(resnest, Kstats) %>%
    ggplot(aes(x=n, y=K600_med)) + geom_point() +
    geom_errorbar(aes(ymin=pmax(K600_q25, 0.1), ymax=K600_q75)) + 
    scale_y_log10(breaks=10^(-1:4), labels=sprintf('%5.1f', 10^(-1:4))) + theme_bw() +
    ylab('Median K600 after filtering') + xlab('Number of filtered K600s')
  
  (unnest(resnest, Kstats) %>%
      arrange(K600_med) %>%
      mutate(site_order = 1:n()) %>%
      ggplot(aes(x=site_order, y=K600_med, color=log(n))) + geom_point() +
      geom_errorbar(aes(ymin=pmax(K600_q25, 0.1), ymax=K600_q75)) + 
      scale_y_log10(breaks=10^(-1:4), labels=sprintf('%5.1f', 10^(-1:4))) + 
      scale_color_continuous("Log number of filtered K600s per point") +
      theme_bw() +
      ylab('Median K600 and interquartile range after filtering') + xlab('Site') + theme(axis.text.x=element_blank(), legend.position='bottom')) %>%
    ggExtra::ggMarginal(type = "histogram", margins = "y", size = 10, fill = "blue")
  
  unnest(resnest, Kstats) %>%
    ggplot(aes(x=K600_med)) + geom_density(fill='lightgrey') + 
    geom_rug(sides='b') + 
    scale_x_log10(breaks=c(0.5,1,5,10,50,100)) + theme_bw()
  
  # plot the discharges
  resnest %>%
    mutate(Q_med = sapply(Qstats, function(qs) unique(qs$median))) %>%
    arrange(Q_med) %>%
    mutate(site_order = 1:n()) %>%
    unnest(Qstats) %>%
    ggplot(aes(x=site_order, y=exp(center), color=log(nbin))) + geom_point() + scale_y_log10() + theme_bw() +
    ylab('Bin Centers') + xlab('Site') + theme(axis.text.x=element_blank())
  
  # boxplot of bin sizes
  unnest(resnest, Qstats) %>%
    ggplot(aes(x=site_name, y=nbin+1, color=center)) + geom_boxplot(color='grey80', fill='grey95') + 
    geom_point() + theme_bw() + scale_y_log10(breaks=c(1, 11, 101), labels=c(0, 10, 100)) + 
    ylab('Days per Bin') + xlab('Site') + theme(axis.text.x=element_blank())
  
  unnest(resnest, data) %>%
    ggplot(aes(x=site_name, y=discharge.daily)) + geom_boxplot() + scale_y_log10() + theme_bw()
  
  results %>%
    filter(ply_validity == 'TRUE') %>%
    group_by(site_name) %>%
    summarize(
      n = n(),
      qlogrange = diff(range(log(discharge.daily)))
    ) %>%
    ggplot(aes(x=n, y=qlogrange)) + geom_point()
}
