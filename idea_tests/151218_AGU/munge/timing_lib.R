# Functions to describing timing & duration of GPP & ER seasonal pulses

source('munge/siteyears_lib.R')
library(mda.streams)
library(dplyr)
library(unitted)

describe_siteyears <- function(siteyears) {
  meta <- get_meta('basic') %>% v()
  growcorr <- if(dim(siteyears[[1]])[1] == 185) 73 else 0
  fix_empty <- function(x) if(length(x)==0) NA else x
  find_dur <- function(col, frac=0.8) {
    if(mean(col, na.rm=T) < 0 || all(is.na(col))) col <- col * -1
    cummetab <- cumsum(replace(col, is.na(col), 0))
    totmetab <- sum(col, na.rm=TRUE)
    length(which((cummetab >= totmetab*(0.5-frac/2) & cummetab < totmetab*(0.5+frac/2)) | is.na(cummetab)))
  }
  find_fwhm <- function(col, nth=5) {
    if(all(is.na(col)) || mean(col, na.rm=T) < 0) col <- col * -1
    ymax <- max(col, na.rm=TRUE)
    xmax <- which.max(col)
    before_max <- which(!is.na(col)) %>% .[. < xmax]
    after_max <- which(!is.na(col)) %>% .[. > xmax]
    x1 <- before_max[which.min(abs(col[before_max] - ymax/nth))]
    x2 <- after_max[which.min(abs(col[after_max] - ymax/nth))]
    fix_empty(x2 - x1)
  }
  data.frame(
    siteyear = names(siteyears[['GPP']]),
    complete = are_complete_siteyears(siteyears),
    doy_max_gpp = sapply(siteyears[['GPP']], function(col) fix_empty(which.max(col))) + growcorr,
    doy_max_er = sapply(siteyears[['ER']], function(col) fix_empty(which.max(-col))) + growcorr,
    doy_min_gpp = sapply(siteyears[['GPP']], function(col) fix_empty(which.min(col))) + growcorr,
    doy_min_er = sapply(siteyears[['ER']], function(col) fix_empty(which.min(-col))) + growcorr,
    doy_half_gpp = sapply(siteyears[['GPP']], function(col) fix_empty(which(cumsum(replace(col, is.na(col), 0)) > sum(col, na.rm=TRUE)/2)[1])) + growcorr,
    doy_half_er = sapply(siteyears[['ER']], function(col) fix_empty(which(cumsum(replace(col, is.na(col), 0)) < sum(col, na.rm=TRUE)/2)[1])) + growcorr,
    dur_half_gpp = sapply(siteyears[['GPP']], find_dur),
    dur_half_er = sapply(siteyears[['ER']], find_dur),
    fwhm_gpp = sapply(siteyears[['GPP']], find_fwhm),
    fwhm_er = sapply(siteyears[['ER']], find_fwhm),
    tot_gpp = sapply(siteyears[['GPP']], function(col) fix_empty(sum(col, na.rm=TRUE))),
    tot_er = sapply(siteyears[['ER']], function(col) fix_empty(sum(col, na.rm=TRUE))),
    stringsAsFactors=FALSE
  ) %>%
    mutate(site = sapply(strsplit(siteyear, ":"), function(sp) sp[1]),
           year = as.numeric(sapply(strsplit(siteyear, ":"), function(sp) sp[2]))) %>%
    left_join(meta, by=c('site'='site_name'))
}

describe_sites <- function(siteyears=siteyears_GAM, sitegrows=sitegrows_GAM, add_grows=TRUE) {
  # get and analyze data for peaks, completeness, etc.
  sydat <- describe_siteyears(siteyears)
  sygdat <- describe_siteyears(sitegrows)
  
  if(add_grows) {
    for(var in names(sydat)[3:ncol(sydat)]) {
      # fill in the variable where it's an incomplete year in sydat but complete in sygdat
      sydat[!sydat$complete & sygdat$complete,var] <- sygdat[!sydat$complete & sygdat$complete,var]
      sydat[!sydat$complete & sygdat$complete,'complete'] <- TRUE
    }
  }
  
  # simplify to one row per site
  sydatmeans <- sydat %>% 
    filter(complete) %>% 
    group_by(site) %>% 
    summarize(
      years=n(), lat=lat[1], lon=lon[1],
      doy_max_gpp = mean(doy_max_gpp, na.rm=TRUE),
      doy_max_er = mean(doy_max_er, na.rm=TRUE),
      doy_min_gpp = mean(doy_min_gpp, na.rm=TRUE),
      doy_min_er = mean(doy_min_er, na.rm=TRUE),
      doy_half_gpp = mean(doy_half_gpp, na.rm=TRUE),
      doy_half_er = mean(doy_half_er, na.rm=TRUE),
      dur_half_gpp = mean(dur_half_gpp, na.rm=TRUE),
      dur_half_er = mean(dur_half_er, na.rm=TRUE),
      fwhm_gpp = mean(fwhm_gpp, na.rm=TRUE),
      fwhm_er = mean(fwhm_er, na.rm=TRUE),
      tot_gpp = mean(tot_gpp, na.rm=TRUE),
      tot_er = mean(tot_er, na.rm=TRUE))
  
  sydatmeans
}