# Functions to describing timing & duration of GPP & ER seasonal pulses

source('munge/siteyears_lib.R')
library(mda.streams)
library(dplyr)
library(unitted)

describe_siteyears <- function(siteyears) {
  meta <- get_meta('basic') %>% v()
  growcorr <- if(dim(siteyears[[1]])[1] == 185) 73 else 0
  data.frame(
    siteyear = names(siteyears[['GPP']]),
    complete = are_complete_siteyears(siteyears),
    doy_max_gpp = sapply(siteyears[['GPP']], function(col) if(length(which.max(col))==1) which.max(col) else NA) + growcorr,
    doy_max_er = sapply(siteyears[['ER']], function(col) if(length(which.max(col))==1) which.max(col) else NA) + growcorr,
    doy_min_gpp = sapply(siteyears[['GPP']], function(col) if(length(which.min(col))==1) which.max(col) else NA) + growcorr,
    doy_min_er = sapply(siteyears[['ER']], function(col) if(length(which.min(col))==1) which.max(col) else NA) + growcorr,
    stringsAsFactors=FALSE
  ) %>%
    mutate(site = sapply(strsplit(siteyear, ":"), function(sp) sp[1]),
           year = as.numeric(sapply(strsplit(siteyear, ":"), function(sp) sp[2]))) %>%
    left_join(meta, by=c('site'='site_name'))
}