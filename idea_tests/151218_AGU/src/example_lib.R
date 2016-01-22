# Figures demonstrating the concurrent variation of X, GPP, and ER

library(mda.streams)
library(dplyr)
library(unitted)
library(tidyr)
library(ggplot2)

build_example <- function(s, pred, dates=as.Date(c("2000-01-01","2030-01-01")), ag_fun=c('mean','max'), day_start=4, day_end=27.99) {
  
  ag_fun <- match.arg(ag_fun)
  
  # pull in predictor data
  pdat <- get_ts(c('sitetime_calcLon', pred), s) %>% v() %>%
    mutate(local.time = sitetime,
           filt.date = as.Date(local.time)) %>% 
    filter(filt.date >= (dates[1] - as.difftime(1,units='days')), filt.date <= (dates[2] + as.difftime(1,units='days'))) %>%
    select(-filt.date)
  
  daily_mean <- function(data_ply, data_daily_ply, day_start, day_end, ...) {
    data_ply %>% summarize_(.dots=sapply(parse_var_src(pred, out='var') %>% setNames(.,.), function(p) paste0(ag_fun, "(", p, ")")))
  }
  pdat_agg <- streamMetabolizer:::mm_model_by_ply(daily_mean, data=pdat, day_start=day_start, day_end=day_end)
  
  # add predictor data to metabolism data
  load('cache/clean_2_GAM.RData')
  clean_2_GAM %>%
    filter(site == s, local.date >= dates[1], local.date <= dates[2]) %>%
    full_join(pdat_agg, by='local.date')
}

#' Plot time series of pred, GPP, and ER
#' 
#' @param s the site to plot
#' @param pred the predictor to plot (e.g., discharge or par)
#' @param dates two Dates, or NAs, limiting the time axis of the plot
plot_example <- function(
  dat=build_example('nwis_08062500', pred='sw_nldas', ag_fun='max'), 
  pred='sw', emphasize=c('quick','slow'), dates=as.Date(c("2000-01-01","2030-01-01")), title) {
  
  emphasize <- match.arg(emphasize)
  
  plotdat <- dat %>%
    filter(local.date >= dates[1], local.date <= dates[2])
  plotdat <- list(
    plotdat %>% select_(.dots=c('local.date', pred, 'GPP', 'ER')) %>% gather_('var', 'val', c(pred, 'GPP', 'ER')),
    plotdat %>% select_(.dots=c('local.date', pred, GPP='GPP.gam', ER='ER.gam')) %>% gather_('var', 'val.gam', c(pred, 'GPP', 'ER'))
  )
  plotdat <- do.call(full_join, c(plotdat, list(by=c('local.date', 'var')))) %>%
    mutate(quick_type=ifelse(var==pred, 'pred', 'day'),
           slow_type=ifelse(var==pred, 'pred', 'gam'))
  
  scale_breaks <- ordered(c('day','gam','pred'), c('pred','day','gam'))
  scale_labs <- c('Daily metabolism','GAM smooth',c(sw='Daily max light', disch='Discharge')[[pred]]) %>% setNames(scale_breaks)
  scale_cols <- c('orange', 'navy', 'navy') %>% setNames(scale_breaks)
  switch(
    emphasize,
    quick={
      scale_pch <- c(19, NA, 19) %>% setNames(scale_breaks)
      scale_lty <- c(1, 0, 1) %>% setNames(scale_breaks)
    }, slow={
      scale_pch <- c(19, NA, 19) %>% setNames(scale_breaks)
      scale_lty <- c(0, 1, 0) %>% setNames(scale_breaks)
    }
  )
  
  ggplot(plotdat, aes(x=local.date)) + 
    geom_point(aes(y=val, color=quick_type, shape=quick_type, linetype=quick_type), size=0.5) +
    switch(
      emphasize,
      quick=geom_line(aes(y=val, color=quick_type, shape=quick_type, linetype=quick_type)),
      slow=geom_line(aes(y=val.gam, color=slow_type, shape=slow_type, linetype=slow_type))) + 
    xlab('Date') + ylab(parse(text=paste(
      'atop(ER,gO[2]~m^-2~d^-1)',
      'atop(GPP,gO[2]~m^-2~d^-1)',
      c(sw='atop(Light,W~m^-2)', disch='atop(Discharge,ft^3~s^-1)')[[pred]],
      sep='~"    "~'))) +
    scale_color_manual(title, breaks=levels(scale_breaks), values=scale_cols, labels=scale_labs) +
    scale_shape_manual(title, breaks=levels(scale_breaks), values=scale_pch, labels=scale_labs) + 
    scale_linetype_manual(title, breaks=levels(scale_breaks), values=scale_lty, labels=scale_labs) +
    theme_classic() + theme(strip.background=element_blank(), strip.text=element_blank()) +
    facet_grid(var ~ ., scales='free_y')

}
