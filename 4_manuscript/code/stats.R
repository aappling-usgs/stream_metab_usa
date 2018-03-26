ms_stats <- function(
  dailies_zip='../4_data_release/cache/models/post/daily_predictions.zip',
  diagnostics_rds='../4_data_release/cache/models/diagnostics.rds'
) {
  stats_tmp_dir <- file.path(tempdir(), 'stats')
  unzipped <- list(
    dailies = unzip(zipfile=dailies_zip, exdir=stats_tmp_dir)
  )
  dailies <- readr::read_tsv(unzipped$dailies)
  diagnostics <- readRDS(diagnostics_rds)
  
  dates_per_site <- dailies %>% group_by(site_name) %>% count() # A tibble: 356 x 2
  
  range(dates_per_site$n) # 61 3296
  max(dates_per_site$n)/365.25 # 9.02
  median(dates_per_site$n)/365.25 # 3.229295
  mean(dates_per_site$n)/365.25 # 3.775365
  
  total_number_of_dates <- nrow(dailies) # 490907
  
  dates_by_resolution <- dailies %>% group_by(resolution) %>% count()
  # resolution      n
  #          5  15042
  #         12   2046
  #         15 330625
  #         30  59484
  #         60  83710
  
  library(ggplot2)
  
  # higher temporal resolution leads to longer runtime
  ggplot(diagnostics, aes(x=n_dates, y=run_hrs, color=resolution)) + geom_point() + geom_smooth(method='lm') + facet_wrap(~ saved_steps)
  
  # higher resolution appears to reduce probability of non-convergence, as
  # measured by whether we re-ran the model
  diagnostics %>%
    mutate(res = as.integer(gsub('min','',resolution))) %>%
    group_by(res) %>%
    summarize(
      ss_500 = length(which(saved_steps == 500)),
      ss_2000 = length(which(saved_steps == 2000)),
      frac_rerun = ss_2000/(ss_500+ss_2000))
  # res ss_500 ss_2000 frac_rerun
  #   5     17       1     0.0556
  #  12      1       1     0.500 
  #  15    213      83     0.280 
  #  30     34      24     0.414 
  #  60     23      36     0.610 
  
  runtime <- diagnostics %>% 
    mutate(hrs_per_year_bymodel=365.25*run_hrs/n_dates) %>%
    summarize(
      run_hrs=sum(run_hrs),
      run_days=run_hrs/24,
      n_dates=sum(n_dates),
      hrs_per_year_q05=quantile(hrs_per_year_bymodel, probs=0.05),
      hrs_per_year_q50=quantile(hrs_per_year_bymodel, probs=0.50),
      hrs_per_year_q95=quantile(hrs_per_year_bymodel, probs=0.95),
      hrs_per_year=365.25*run_hrs/n_dates
    )
  # run_hrs run_days n_dates hrs_per_year_q05 hrs_per_year_q50 hrs_per_year_q95 hrs_per_year
  #  17168.     715.  490907             2.41             9.56             47.4         12.8
}
