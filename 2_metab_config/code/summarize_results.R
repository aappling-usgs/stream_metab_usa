library(dplyr)
library(tidyr)
library(ggplot2)
library(mda.streams)
library(streamMetabolizer)

config <- read_config('../2_metab_config/out/config.tsv')
config$model_name <- make_metab_model_name(make_metab_run_title(format(as.Date(config$date), '%y%m%d'), config$tag, config$strategy), config$config.row, config$site)

summary_files <- dir('../2_metab_config/out/summaries', full.names = TRUE)
fit_files <- dir('../2_metab_config/out/fits', full.names = TRUE)

# there are 8 models with empty predictions and the fit$errors value, "dates have differing numbers of rows; observations cannot be combined in matrix"
login_sb()
mms <- list_metab_models('1.0.1')
done_mms <- parse_metab_model_name(mms)$row
done_sums <- parse_metab_model_name(substring(basename(summary_files), 9))$row
done_fits <- parse_metab_model_name(substring(basename(fit_files), 5))$row
setdiff(config$config.row, done_mms) # not done at all: 360
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

# summarize the remaining
summarize_results <- function(config_row) {
  
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
  summary_file <- grep(config$model_name[config_row], summary_files, value=TRUE)
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
    new_daily <- NULL
  }
  
  # Combine old and new daily estimates, including a df that ignores outlier old
  # values relative to the new ones (for plotting)
  daily <- bind_rows(old_daily, new_daily)
  daily_newscale <- daily %>%
    group_by(param) %>%
    mutate(min=min(value[age == 'new']), max=max(value[age == 'new'])) %>%
    ungroup() %>%
    filter(param=='Q' | (min < value & value <= max))
  
  # Read the fit file (all params, with Rhats) if available
  fit_file <- grep(config$model_name[config_row], fit_files, value=TRUE)
  if(length(fit_file) == 1) {
    fit <- readRDS(fit_file) # it says .tsv but is really .RDS
  } else if(length(fit_file) > 1) {
    stop("found more than 1 fit file for row ", config_row, ": ", paste0(fit_file, collapse=','))
  } else {
  }
  
  #### Plots ####
  
  # Plot daily values as time series
  fun_plot_daily <- function(daily) {
    daily %>%
      mutate(
        age=ordered(age, c('old','new')),
        param = ordered(param, c('GPP','ER','K600','Q','Warning','Error'))) %>%
      ggplot(aes(x=date, y=value, color=param, alpha=age, shape=age)) + geom_point() +
      facet_grid(param ~ ., scales='free_y') +
      scale_alpha_manual(values=c(old=0.6, new=1)) +
      scale_shape_manual(values=c(old=20, new=19)) +
      theme_bw()
  }
  plot_daily <- fun_plot_daily(daily)
  plot_daily
  plot_daily_newscale <- fun_plot_daily(daily_newscale)
  plot_daily_newscale
  
  # Calculate correlations among daily values
  daily_mat <- daily %>%
    mutate(par_age = paste(param, age, sep='_')) %>%
    select(-param, -age) %>%
    spread(par_age, value)
  correlations <- data_frame(
    GPP_newold = cor(daily_mat$GPP_new, daily_mat$GPP_old, use='complete.obs'),
    ER_newold = cor(daily_mat$ER_new, daily_mat$ER_old, use='complete.obs'),
    K600_newold = cor(daily_mat$K600_new, daily_mat$K600_old, use='complete.obs'),
    K600ER_old = cor(daily_mat$K600_old, daily_mat$ER_old, use='complete.obs'),
    K600ER_new = cor(daily_mat$K600_new, daily_mat$ER_new, use='complete.obs'),
    K600GPP_old = cor(daily_mat$K600_old, daily_mat$GPP_old, use='complete.obs'),
    K600GPP_new = cor(daily_mat$K600_new, daily_mat$GPP_new, use='complete.obs'),
    GPPER_old = cor(daily_mat$GPP_old, daily_mat$ER_old, use='complete.obs'),
    GPPER_new = cor(daily_mat$GPP_new, daily_mat$ER_new, use='complete.obs')
  )
  
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
    select(stat, everything())
  
}

