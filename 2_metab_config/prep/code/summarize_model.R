#' For prep MLE run, extract daily K and Q
#' 
#' @import streamMetabolizer
#' @import mda.streams
#' @import dplyr
#' @import unitted
summarize_model(model_out, outdir) {
  library(streamMetabolizer)
  library(mda.streams)
  library(dplyr)
  library(unitted)
  
  fit <- get_fit(model_out)
  site <- get_info(model_out)$config$site
  Q <- get_ts(c('sitedate_calcLon','dischdaily_calcDMean'), site)
  
  smry <- fit %>%
    mutate(site_name=site) %>%
    left_join(v(Q), by=c(date='sitedate')) %>%
    select(site_name, K600.daily, K600.daily.sd, discharge.daily=dischdaily, everything())
  
  write.table(smry, )
}