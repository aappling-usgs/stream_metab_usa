#' Use the prep run to choose parameters for the main, Bayesian run
#' 
#' @import dplyr
#' @import ggplot2
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
  
  # summarize the median Ks
  medK600s <- results %>%
    filter(ply_validity == 'TRUE', !is.na(K600.daily), K600.daily.sd/K600.daily < 0.5, K600.daily > 0) %>%
    group_by(site_name) %>%
    summarize(
      n = n(),
      K600_min = min(K600.daily),
      K600_q25 = quantile(K600.daily, 0.25),
      K600_med = median(K600.daily),
      K600_q50 = quantile(K600.daily, 0.50),
      K600_q75 = quantile(K600.daily, 0.75),
      K600_max = max(K600.daily)
    )
  ggplot(medK600s, aes(x=n, y=K600_med)) + geom_point() +
    geom_errorbar(aes(ymin=K600_q25, ymax=K600_q75)) + scale_y_log10() + theme_bw()
  
  results %>%
    filter(ply_validity == 'TRUE') %>%
    ggplot(aes(x=site_name, y=K600.daily)) + geom_boxplot() + scale_y_log10() + theme_bw()
  
  results %>%
    filter(ply_validity == 'TRUE') %>%
    ggplot(aes(x=site_name, y=K600.daily)) + geom_boxplot() + theme_bw() + ylim(-12, 0)
  
  results %>%
    filter(ply_validity == 'TRUE') %>%
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
