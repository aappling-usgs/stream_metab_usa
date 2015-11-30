# What are the drivers of synchrony and divergence in the temporal patterns of
# metabolism among streams of North America?

library(mda.streams)
library(streamMetabolizer)
library(dplyr)
outdir <- "idea_tests/151218_AGU/out"

# get data from ScienceBase
sbtools::authenticate_sb(SBUSER, SBPASS)
mms <- grep("0.0.15", list_metab_models(), fixed=T, value=T)
preds_list <- lapply(mms, function(mmname) {
  mm <- get_metab_model(mmname, version='original', update_sb=FALSE)
  k_preds <- get_data_daily(mm)
  predict_metab(mm) %>% 
    mutate(model=mmname) %>%
    dplyr::filter(local.date %in% k_preds$local.date)
})
preds <- bind_rows(preds_list) %>%
  mutate(site = parse_metab_model_name(model, out='site'))
save(preds, file='idea_tests/151218_AGU/out/preds.RData')
meta <- get_meta()
save(meta, file='idea_tests/151218_AGU/out/meta.RData')
mm1 <- get_metab_model(grep("nwis_02234000", mms, value=TRUE))
example_models <- list(
  '1_PRK'=get_metab_model("nwis_02234000-59-151125 0.0.13 PRK_initial"),
  '2_K'=get_metab_model("nwis_02234000-59-151125 0.0.14 K_smooth"),
  '3_PK'=get_metab_model("nwis_02234000-59-151126 0.0.15 PR_fixed_K"))
save(example_models, file='idea_tests/151218_AGU/out/example_models.RData')
  
library(dplyr)
library(unitted)
library(synchrony)
library(tidyr)

#### methods ####

# show how modeling worked
veloc <- select(get_data_daily(example_models[[2]]), local.date, velocity.daily)
eg_preds <- list(
  mutate(predict_metab(example_models[[1]]), phase='1_PRK'),
  mutate(predict_metab(example_models[[2]]), phase='2_K'),
  dplyr::filter(preds, site=="nwis_02234000") %>% 
    .[names(predict_metab(example_models[[2]]))] %>% 
    mutate(phase='3_PR')
) %>% bind_rows() %>%
  left_join(veloc, by='local.date')
# phase 1
ggplot(dplyr::filter(eg_preds, phase %in% c('1_PRK')), aes(x=local.date, y=GPP, color=phase)) + geom_point(size=0.8, alpha=0.8) + theme_bw() + ylim(-10,15)
ggsave(file.path(outdir, 'phase1_GPP.png'), width=6, height=4)
ggplot(dplyr::filter(eg_preds, phase %in% c('1_PRK')), aes(x=local.date, y=ER, color=phase)) + geom_point(size=0.8, alpha=0.8) + theme_bw() + ylim(-45,15)
ggsave(file.path(outdir, 'phase1_ER.png'), width=6, height=4)
# phase 2
ggplot(eg_preds, aes(x=velocity.daily, y=K600, color=phase)) + 
  geom_point(data=dplyr::filter(eg_preds, phase %in% c('1_PRK'))) +
  geom_line(data=dplyr::filter(eg_preds, phase %in% c('2_K')), size=1) +
  geom_line(data=dplyr::filter(eg_preds, phase %in% c('3_PR')), size=1, linetype='dashed') +
  theme_bw() + scale_y_log10() + scale_x_log10()
ggsave(file.path(outdir, 'KvV.png'), width=6, height=4)
# phase 3
ggplot(eg_preds, aes(x=local.date, y=GPP, color=phase)) + geom_point(size=0.8, alpha=0.8) + theme_bw() + ylim(-10,15)
ggsave(file.path(outdir, 'phases_GPP.png'), width=6, height=4)
ggplot(eg_preds, aes(x=local.date, y=ER, color=phase)) + geom_point(size=0.8, alpha=0.8) + theme_bw() + ylim(-45,15)
ggsave(file.path(outdir, 'phases_ER.png'), width=6, height=4)

#### reload ####

# load data from local cache
load('idea_tests/151218_AGU/out/preds.RData')
load('idea_tests/151218_AGU/out/meta.RData')
load('idea_tests/151218_AGU/out/example_models.RData')
mms <- unique(preds$model)
pmms <- parse_metab_model_name(mms) %>% add_rownames('model')

# parse into site-years
growlims <- as.Date(c("2014-03-01","2014-10-01")) %>% format("%j") %>% as.numeric()
sypreds <- preds %>%
  left_join(select(pmms, model, site), by='model') %>%
  mutate(
    doy = as.numeric(format(local.date, "%j")),
    growday = doy > growlims[1] & doy < growlims[2],
    year = as.numeric(format(local.date, "%Y")),
    siteyear = paste0(site, ":", year))
#sypred1 <- filter(sypreds, site=="nwis_02234000")
# summarize site-years
siteyears <- sypreds %>%
  group_by(siteyear) %>%
  summarize(
    site = site[1],
    year = year[1],
    full_year = n() > 300 && min(doy) < 15 && max(doy) > 350,
    full_season = length(doy[growday]) > (growlims[2]-growlims[1]-40) && min(doy[growday]) < growlims[1]+15 && max(doy[growday]) > growlims[2]-15,
    nobs = n(),
    growobs = length(doy[growday]))

#### filter ####
sites <- unique(sypreds$site) %>% setNames(.,.)
# fill in any date gaps with NAs
sypred_gapless <- lapply(sites, function(s) {
  sypred <- dplyr::filter(sypreds, site==s)
  sypredall <- data_frame(local.date=seq.Date(min(sypred$local.date), max(sypred$local.date), by=as.difftime(1, units='days'))) %>%
    left_join(sypred, by='local.date')
})
# simpler data.frames
dfpreds <- lapply(sites, function(s) {
  p <- sypred_gapless[[s]] %>%
    select(local.date, GPP, ER, year, doy, velocity.daily)
})
# create lists of univariate TSes
# library(zoo)
# tspreds <- lapply(dfpreds, function(p) {
#   pm <- p %>%
#     select(GPP, ER, year) %>%
#     as.matrix()
#   zoo(pm, order.by=p[['local.date']]) %>% 
#     ts()
# })


# this approach detects 0 outliers:
# library(tsoutliers)
# detach('package:dplyr') # because tso() uses stats::filter without indicating namespace
# outlier.syts1.gpp <- tsoutliers::tso(tspreds[[17]][,"GPP"], types = c("AO","LS","TC"), maxit.iloop=10)
# outlier.syts1.er <- tsoutliers::tso(tspreds[[17]][,"ER"], types = c("AO","LS","TC"), maxit.iloop=10)
# library(dplyr)

# remove outliers & interpolate
# exclude values too far beyond the central tendency
dfclean <- lapply(dfpreds, function(dfp) {
  vars <- lapply(c('GPP','ER') %>% setNames(.,.), function(v) {
    yraw <- dfp[[v]]
    yi <- yraw*(if(c(v)=='ER') -1 else 1)
    qy <- quantile(yi, c(0.1,0.9), na.rm=TRUE)
    qry <- qy[2]-qy[1]
    yrm <- !(yi > (qy[1]-qry/3) & yi < (qy[2]+qry))
    data_frame(yraw=yraw, yi2=ifelse(!yrm, yraw, NA), yrm = yrm) %>%
      setNames(paste0(v,c('.raw','','.rm')))
  })
  bind_cols(c(list(dfp[!(names(dfp) %in% c('GPP','ER'))]), vars))
})
ggplot(dfclean[[114]], aes(x=local.date, y=ER, col=ER.rm)) + geom_point(alpha=0.8, size=1) + ylim(-40, 5) + theme_bw() + scale_color_discrete("Removed")
ggplot(dfclean[[114]], aes(x=local.date, y=ER.raw, col=ER.rm)) + geom_point(alpha=0.8, size=1) + ylim(-40, 5) + theme_bw() + scale_color_discrete("Removed")
ggplot(dfclean[[279]], aes(x=local.date, y=GPP.raw, col=GPP.rm)) + geom_point(alpha=0.8, size=1) + ylim(-5,20) + theme_bw() + scale_color_discrete("Removed") + xlab("Date") + ylab(parse(text="GPP~(g~m^-2~d^-1)"))
ggsave(file.path(outdir, "outlier_removal.png"), width = 7, height=5)
#ggplot(dfclean[[279]], aes(x=local.date, y=ER, col=ER.rm)) + geom_point() + ylim(-20,5) + theme_bw()

# interpolation to replace NAs with values
library(signal)
dfint <- lapply(dfclean, function(dfc) {
  dates <- as.numeric(dfc[['local.date']])
  vars <- lapply(c('GPP','ER') %>% setNames(.,.), function(v) {
    # get df(var, var.is_interp) where var is ts of original & interpolated
    yo <- dfc[[v]]
    interp <- is.na(yo)
    yi <- interp1(x=dates[!interp], y=yo[!interp], xi=dates[interp], "pchip")
    df <- data_frame(yo, factor(ifelse(interp, "interp", "raw"))) %>% setNames(paste0(v,c("",".src")))
    df[interp, v] <- yi
    df
  })
  bind_cols(c(list(dfc[c('local.date','GPP.rm','ER.rm')]), vars)) %>%
    dplyr::filter(!is.na(GPP) & !is.na(ER))
})
ggplot(dfint[[210]], aes(x=local.date, y=ER, col=ER.src)) + geom_point(alpha=0.8, size=1) + ylim(-25,0) + theme_bw()
# # low-pass filter...nope
# filt <- signal::Ma(2)
# dffilt <- lapply(dfint, function(dfi) {
#   vars <- lapply(c('GPP','ER') %>% setNames(.,.), function(v) {
#     yi <- dfi[[v]]
#     #yi[yi<0] <- 0
#     #yi <- log(yi+1)
#     yf <- signal::filter(filt, yi)
#     #yf <- exp(yf) - 1
#     data_frame(yf = yf) %>%
#       setNames(paste0(v,'.filt'))
#   })
#   bind_cols(c(list(dfi), vars))
# })
# ggplot(dffilt[[44]], aes(x=local.date)) + geom_point(aes(y=GPP, color=GPP.src)) + geom_line(aes(y=GPP.filt)) + ylim(-5,20) + xlim(as.Date("2010-01-01"), as.Date("2011-01-01"))
# detach('package:signal', unload=TRUE) # because signal::filter could get in the way later

# running mean
library(zoo)
dfroll <- lapply(dfint, function(dfi) {
  dates <- as.numeric(dfi[['local.date']])
  vars <- lapply(c('GPP','ER') %>% setNames(.,.), function(v) {
    # get df(var, var.is_interp) where var is ts of original & interpolated
    yo <- dfi[[v]]
    yr7 <- rollmean(yo, k=7, fill=NA, align='center')
    yr31 <- tryCatch({
      rollmean(yo, k=31, fill=NA, align='center')
    }, error=function(e) rep(NA, length(yo)))
    data_frame(yr7=yr7, yr31=yr31) %>% setNames(paste0(v,c(".roll7",".roll31")))
  })
  bind_cols(c(list(dfi), vars)) %>%
    dplyr::filter(!is.na(GPP) & !is.na(ER))
})
ggplot(dplyr::filter(dfroll[[28]], format(local.date, "%Y") == "2014"), aes(x=local.date)) + 
  geom_point(aes(y=GPP), color='blue', alpha=0.4) + 
  geom_line(aes(y=GPP.roll7), color='blue', size=0.9) + 
  geom_line(aes(y=GPP.roll31), color='navy', size=1.5) + theme_bw() + ylim(c(-0.5,2))
ggsave(file.path(outdir, 'rollmeans.png'), width=6, height=4)

#### storm effects ####

# short-term disturbances in the form of storms and cloudy days have strong
# proximate, dampening effects on whole-ecosystem metabolism
dfrolldisch <- lapply(names(dfroll) %>% setNames(.,.), function(dfrn) {
  dfr <- dfroll[[dfrn]]
  disch <- get_ts(c("dischdaily_calcDMean","sitedate_calcLon"), dfrn, method='full_join') %>%
    select(sitedate, dischdaily) %>%
    setNames(c("local.date","disch")) %>%
    unitted::v() # "m^3 s^-1"
  left_join(dfr, disch, by='local.date')
})
ggplot(mutate(dfrolldisch[[72]], GPP.diff=c(NA, diff(GPP.roll7-GPP.roll31))), aes(x=disch, y=GPP.diff)) + geom_point() + theme_bw() + scale_x_log10() + ylim(c(-5,5))
ggplot(mutate(dfrolldisch[[72]], ER.diff=c(NA, diff(ER.roll7-ER.roll31))), aes(x=disch, y=ER.diff)) + geom_point() + theme_bw() + scale_x_log10() + ylim(c(-5,5))
ggplot(dfrolldisch[[72]], aes(x=local.date, y=GPP.roll7)) + geom_line() + theme_bw()
ggplot(dfrolldisch[[72]], aes(x=local.date, y=disch)) + geom_line() + theme_bw()
ggplot(dfrolldisch[[72]], aes(x=local.date, y=ER.roll7)) + geom_line() + theme_bw()

#### synchrony ####

# streams with similar resource and discharge regimes are largely synchronous in
# their overall seasonal patterns
gppsiteyear <- lapply(names(dfroll) %>% setNames(.,.), function(dfr) {
  dfroll[[dfr]] %>% 
    mutate(siteyear=paste0(dfr, "_", format(local.date, '%Y')),
           doy=as.numeric(format(local.date, '%j'))) %>%
    select(siteyear, doy, GPP) %>%
    spread(siteyear, GPP) %>%
    right_join(data_frame(doy=1:366), by='doy') %>%
    select(-doy)
}) %>% bind_cols() %>% as.matrix()
fullcol <- unname(apply(gppsiteyear, MARGIN=2, FUN=function(x) all(!is.na(x[1:365]))))
siteids <- sapply(strsplit(colnames(gppsiteyear), "_"), `[`, 2)
years <- sapply(strsplit(colnames(gppsiteyear), "_"), `[`, 3)

sync_withinsite <- lapply(unique(siteids), function(usite) {
  mycols <- which(fullcol & siteids==usite)
  sync <- synchrony::community.sync(gppsiteyear[1:365,mycols])
  data.frame(site=usite, setNames(as.data.frame(c(sync)), c("sync","meancorr")), ncol=length(mycols), stringsAsFactors=FALSE)
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
sync_withinyear <- lapply(unique(years), function(uyear) {
  mycols <- which(fullcol & years==uyear)
  sync <- synchrony::community.sync(gppsiteyear[1:365,mycols])
  data.frame(year=uyear, setNames(as.data.frame(c(sync)), c("sync","meancorr")), ncol=length(mycols), stringsAsFactors=FALSE)
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
ggplot(sync_withinsite, aes(x=sync)) + geom_density() + theme_bw() + geom_vline(data=sync_withinyear, aes(xintercept=sync)) + xlim(0,1)
ggplot(sync_withinsite, aes(x=meancorr)) + geom_density() + theme_bw() + geom_vline(data=sync_withinyear, aes(xintercept=meancorr)) + xlim(NA,1)

#### peak timing ####

# The median date of peak gross primary productivity is close to the summer
# solstice in June

# ecosystem respiration has two most-common annual peaks, one in early spring
# and another in mid fall
# good example: nwis_02234000
dat <- dfrolldisch[["nwis_02234000"]] %>% dplyr::filter(ER.rm==FALSE & GPP.rm==FALSE) %>% mutate(quarter=cut(as.numeric(format(local.date, "%j")), 366*c(0,0.25,0.5,0.75,1), labels=c('winter','spring','summer','fall')))
ggplot(dat, aes(x=local.date, color=quarter)) + geom_point(aes(y=GPP)) + geom_point(aes(y=ER)) + theme_bw()
ggplot(dat, aes(x=GPP.roll7, y=ER.roll7, color=quarter)) + geom_point() + theme_bw() + facet_wrap(~ quarter)

#### conclusions ####

# differences in peak timing point to systematic differences at the continental
# scale in the timing of light versus organic matter availability, with
# consequent seasonality in net ecosystem productivity
