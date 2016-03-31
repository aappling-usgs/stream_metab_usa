# What are the drivers of synchrony and divergence in the temporal patterns of
# metabolism among streams of North America?

library(mda.streams)
library(streamMetabolizer)
library(dplyr)
outdir <- "idea_tests/151218_AGU/out"

#### sciencebase downloads ####

# get data from ScienceBase
sbtools::authenticate_sb(SBUSER, SBPASS)
# final PR(fixK) model predictions
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

# save metadata in case SB poops out
meta <- get_meta()
save(meta, file='idea_tests/151218_AGU/out/meta.RData')

# discharge & velocity - would be better to get raw disch & veloc, aggregate to
# right time window (hrs 4-28). using pre-calc daily values is a shortcut
sites <- parse_metab_model_name(mms, out='site')
dischveloc_list <- lapply(sites, function(site) {
  df <- get_ts(c('sitedate_calcLon', 'dischdaily_calcDMean', 'velocdaily_calcDMean'), site_name = site, method='full_join') %>%
    mutate(site=site) %>%
    select(local.time=DateTime, local.date=sitedate, site, disch=dischdaily, veloc=velocdaily) # on 2/17/16, local.time=DateTime looks wrong to me b/c DateTime is UTC. but then maybe fine because appears to never be used.
})
dvunits <- unitted::get_units(dischveloc_list[[1]]) # disch="m^3 s^-1", veloc="m s^-1" 
dischveloc <- lapply(dischveloc_list, unitted::v) %>% bind_rows()
save(dischveloc, file='idea_tests/151218_AGU/out/dischveloc.RData')

# example series of metab models from PRK to K to PR
mm1 <- get_metab_model(grep("nwis_02234000", mms, value=TRUE))
example_models <- list(
  '1_PRK'=get_metab_model("nwis_02234000-59-151125 0.0.13 PRK_initial"),
  '2_K'=get_metab_model("nwis_02234000-59-151125 0.0.14 K_smooth"),
  '3_PK'=get_metab_model("nwis_02234000-59-151126 0.0.15 PR_fixed_K"))
save(example_models, file='idea_tests/151218_AGU/out/example_models.RData')

  
library(dplyr)
library(unitted)
library(tidyr)
library(ggplot2)

#### methods ####

# show how modeling worked
load('idea_tests/151218_AGU/out/example_models.RData')
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

# pick up at phase 4 - load data from local cache
load('idea_tests/151218_AGU/out/preds.RData')
load('idea_tests/151218_AGU/out/meta.RData')
load('idea_tests/151218_AGU/out/dischveloc.RData')

# attach date info
growlims <- as.Date(c("2014-03-01","2014-10-01")) %>% format("%j") %>% as.numeric()
sypreds <- preds %>%
  mutate(
    doy = as.numeric(format(local.date, "%j")),
    growday = doy > growlims[1] & doy < growlims[2],
    year = as.numeric(format(local.date, "%Y")),
    siteyear = paste0(site, ":", year))
# get vector of sites
sites <- unique(sypreds$site) %>% setNames(.,.)
# phase 3.5: fill in any date gaps with NAs
sypred_gapless <- lapply(sites, function(s) {
  sypred <- dplyr::filter(sypreds, site==s)
  dv <- dplyr::filter(dischveloc, site==s) %>% select(local.date, disch, veloc)
  sypredall <- data_frame(local.date=seq.Date(min(sypred$local.date), max(sypred$local.date), by=as.difftime(1, units='days'))) %>%
    left_join(sypred, by='local.date') %>%
    left_join(dv, by='local.date')
})
# simpler data.frames
dfpreds <- lapply(sites, function(s) {
  p <- sypred_gapless[[s]] %>%
    select(local.date, GPP, ER, year, doy, disch, veloc)
})

# phase 4: exclude values too far beyond the central tendency
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

# phase 5: interpolate to replace NAs with values
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
  bind_cols(c(list(select(dfc, -GPP, -ER)), vars)) %>%
    dplyr::filter(!is.na(GPP) & !is.na(ER))
})
ggplot(dfint[[210]], aes(x=local.date, y=ER, col=ER.src)) + geom_point(alpha=0.8, size=1) + ylim(-18,0) + theme_bw()
ggsave(file.path(outdir, 'interp.png'), width=6, height=4)

# phase 6: compute running means
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
dat <- dplyr::filter(dfroll[[82]], local.date > as.Date("2014-04-01"), local.date < as.Date("2014-10-01")) %>%
  tidyr::gather(var, val, veloc, GPP, ER)
ggplot(dat, aes(x=local.date, y=val)) + geom_line() + theme_bw() + facet_grid(var ~ ., scales='free_y')
ggsave(file.path(outdir, 'veloc_GPP_ER.png'), width=5, height=4)
ggplot(dfroll[[82]], aes(x=disch, y=GPP)) + geom_point()
ggplot(dfroll[[82]], aes(x=veloc, y=GPP)) + geom_point()
ggsave(file.path(outdir, 'GPPvVeloc.png'), width=5, height=4)
ggplot(dfroll[[82]], aes(x=disch, y=ER)) + geom_point()
ggplot(dfroll[[82]], aes(x=veloc, y=ER)) + geom_point()
ggplot(mutate(dfroll[[82]], GPP.diff=c(diff(GPP-GPP.roll31), NA)), aes(x=veloc, y=GPP.diff)) + geom_point() + theme_bw() + scale_x_log10() + ylim(c(-5,5))
ggsave(file.path(outdir, 'GPPdiffvVeloc.png'), width=5, height=4)
ggplot(mutate(dfroll[[82]], ER.diff=c(diff(ER-ER.roll31), NA)), aes(x=disch, y=ER.diff)) + geom_point() + theme_bw() + scale_x_log10() + ylim(c(-5,5))

#### synchrony ####

# summarize site-years using data from before all that interpolation
siteyears <- sypreds %>%
  group_by(siteyear) %>%
  summarize(
    site = site[1],
    year = year[1],
    full_year = n() > 300 && min(doy) < 15 && max(doy) > 350,
    full_season = length(doy[growday]) > (growlims[2]-growlims[1]-40) && min(doy[growday]) < growlims[1]+15 && max(doy[growday]) > growlims[2]-15,
    nobs = n(),
    growobs = length(doy[growday]))


# streams with similar resource and discharge regimes are largely synchronous in
# their overall seasonal patterns
library(tidyr)
gppsiteyear <- lapply(names(dfroll) %>% setNames(.,.), function(dfr) {
  dfroll[[dfr]] %>% 
    mutate(siteyear=paste0(dfr, "_", format(local.date, '%Y')),
           doy=as.numeric(format(local.date, '%j'))) %>%
    select(siteyear, doy, GPP) %>%
    spread(siteyear, GPP) %>%
    right_join(data_frame(doy=1:366), by='doy') %>%
    select(-doy)
}) %>% bind_cols() %>% as.matrix()
ersiteyear <- lapply(names(dfroll) %>% setNames(.,.), function(dfr) {
  dfroll[[dfr]] %>% 
    mutate(siteyear=paste0(dfr, "_", format(local.date, '%Y')),
           doy=as.numeric(format(local.date, '%j'))) %>%
    select(siteyear, doy, ER) %>%
    spread(siteyear, ER) %>%
    right_join(data_frame(doy=1:366), by='doy') %>%
    select(-doy)
}) %>% bind_cols() %>% as.matrix()
fullcol <- unname(apply(gppsiteyear, MARGIN=2, FUN=function(x) all(!is.na(x[1:365]))))
siteids <- sapply(strsplit(colnames(gppsiteyear), "_"), `[`, 2)
years <- sapply(strsplit(colnames(gppsiteyear), "_"), `[`, 3)

library(synchrony)
sync_withinsite <- lapply(unique(siteids), function(usite) {
  mycols <- which(fullcol & siteids==usite)
  if(length(mycols) > 1) {
  sync <- synchrony::community.sync(gppsiteyear[1:365,mycols], nrands=100, alternative='greater')
  data.frame(site=usite, 
             setNames(as.data.frame(c(sync[c('obs','meancorr','pval')])), c("sync","meancorr","sync.pval")), 
             ncol=length(mycols), stringsAsFactors=FALSE)
  } else {
    data.frame(site=usite, sync=0, meancorr=0, sync.pval=0, ncol=length(mycols), stringsAsFactors = FALSE)[c(), ]
  }
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
sync_withinsite

sync_withinyear <- lapply(unique(years), function(uyear) {
  mycols <- which(fullcol & years==uyear)
  if(length(mycols) > 1) {
    sync <- synchrony::community.sync(gppsiteyear[1:365,mycols], nrands=100, alternative='greater')
    data.frame(year=uyear, 
               setNames(as.data.frame(c(sync[c('obs','meancorr','pval')])), c("sync","meancorr","sync.pval")), 
               ncol=length(mycols), stringsAsFactors=FALSE)
  } else {
    data.frame(site=usite, sync=0, meancorr=0, sync.pval=0, ncol=length(mycols), stringsAsFactors = FALSE)[c(), ]
  }
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
sync_withinyear

sync_all <- bind_rows(
  mutate(sync_withinsite, within='site', across='years'),
  mutate(sync_withinyear, within='year', across='sites'))
ggplot(sync_all, aes(x=sync, fill=across)) + geom_density(color=NA, alpha=0.6) + theme_bw()
ggsave(file.path(outdir, 'sync.png'), width=6, height=4)
ggplot(sync_all, aes(x=meancorr, fill=across)) + geom_density(color=NA, alpha=0.6) + theme_bw()
ggsave(file.path(outdir, 'meancorr.png'), width=6, height=4)

# basic intuition: there's an annual peak. use roll31 for this.
gppsiteyear31 <- lapply(names(dfroll) %>% setNames(.,.), function(dfr) {
  dfroll[[dfr]] %>% 
    mutate(siteyear=paste0(dfr, "_", format(local.date, '%Y')),
           doy=as.numeric(format(local.date, '%j'))) %>%
    select(siteyear, doy, GPP.roll31) %>%
    spread(siteyear, GPP.roll31) %>%
    right_join(data_frame(doy=1:366), by='doy') %>%
    select(-doy)
}) %>% bind_cols() %>% as.matrix()
ersiteyear31 <- lapply(names(dfroll) %>% setNames(.,.), function(dfr) {
  dfroll[[dfr]] %>% 
    mutate(siteyear=paste0(dfr, "_", format(local.date, '%Y')),
           doy=as.numeric(format(local.date, '%j'))) %>%
    select(siteyear, doy, ER.roll31) %>%
    spread(siteyear, ER.roll31) %>%
    right_join(data_frame(doy=1:366), by='doy') %>%
    select(-doy)
}) %>% bind_cols() %>% as.matrix()
fullcol <- unname(apply(gppsiteyear31, MARGIN=2, FUN=function(x) all(!is.na(x[1:365]))))
siteids <- sapply(strsplit(colnames(gppsiteyear31), "_"), `[`, 2)
years <- sapply(strsplit(colnames(gppsiteyear31), "_"), `[`, 3)

tidygpp <- gppsiteyear31 %>% apply(2, function(col) col/sum(col, na.rm=TRUE)) %>%
  as.data.frame %>% .[1:365, fullcol] %>% mutate(doy=1:365) %>% gather_('siteyear', 'gpp', colnames(select(., -doy))) %>%
  mutate(site=sapply(strsplit(as.character(siteyear), "_"), `[`, 2),
         year=sapply(strsplit(as.character(siteyear), "_"), `[`, 3))
ggplot(dplyr::filter(tidygpp, year=='2014', site %in% grep("^101", site, value=TRUE)), aes(x=doy, y=gpp, color=siteyear)) + geom_line() + scale_color_discrete() + theme_bw() + ylab("normalized GPP") + xlab("day of year")
ggsave(file.path(outdir, "gpp_101s.png"), width=6, height=4)
ggplot(dplyr::filter(tidygpp, year=='2014', site %in% grep("^030", site, value=TRUE)), aes(x=doy, y=gpp, color=siteyear)) + geom_line() + scale_color_discrete() + theme_bw() + ylab("normalized GPP") + xlab("day of year")
ggsave(file.path(outdir, "gpp_030s.png"), width=6, height=4)
ggplot(dplyr::filter(tidygpp, year=='2014', site %in% grep("^041", site, value=TRUE)), aes(x=doy, y=gpp, color=siteyear)) + geom_line() + scale_color_discrete() + theme_bw() + ylab("normalized GPP") + xlab("day of year")
ggsave(file.path(outdir, "gpp_041s.png"), width=6, height=4)


#### peak timing ####

# The median date of peak gross primary productivity is close to the summer
# solstice in June
tidygpp <- gppsiteyear31 %>% apply(2, function(col) col/sum(col, na.rm=TRUE)) %>%
  as.data.frame %>% .[1:365, fullcol] %>% mutate(doy=1:365) %>% gather_('siteyear', 'gpp', colnames(select(., -doy))) %>%
  mutate(site=sapply(strsplit(as.character(siteyear), "_"), `[`, 2),
         year=sapply(strsplit(as.character(siteyear), "_"), `[`, 3))
tidyer <- ersiteyear31 %>% apply(2, function(col) col/sum(col, na.rm=TRUE)) %>%
  as.data.frame %>% .[1:365, fullcol] %>% mutate(doy=1:365) %>% gather_('siteyear', 'er', colnames(select(., -doy))) %>%
  mutate(site=sapply(strsplit(as.character(siteyear), "_"), `[`, 2),
         year=sapply(strsplit(as.character(siteyear), "_"), `[`, 3))
peakdates <- full_join(tidygpp, tidyer) %>%
  group_by(siteyear) %>%
  summarize(doy_max_gpp = which.max(gpp),
            doy_max_er = which.max(er)) %>%
  ungroup()
ggplot(gather(peakdates, var, doy_max, doy_max_gpp, doy_max_er) %>% mutate(var=c(doy_max_gpp='GPP', doy_max_er='ER')[var]), aes(x=doy_max, fill=var)) + geom_density(color=NA, alpha=0.6) + theme_bw() + ylab("density of GPP or ER") + xlab("day of year")
ggsave(file.path(outdir, 'peakdates.png'), width=6, height=4)

# ecosystem respiration has two most-common annual peaks, one in early spring
# and another in mid fall

# good example: nwis_02234000
dat <- dfroll[["nwis_02234000"]] %>% dplyr::filter(ER.rm==FALSE & GPP.rm==FALSE) %>% mutate(quarter=cut(as.numeric(format(local.date, "%j")), 366*c(0,0.25,0.5,0.75,1), labels=c('winter','spring','summer','fall')))
ggplot(dat, aes(x=local.date, color=quarter)) + geom_point(aes(y=GPP)) + geom_point(aes(y=ER)) + theme_bw()
ggsave(file.path(outdir, 'gpp_er_ts.png'), width=8, height=4)
ggplot(dat, aes(x=GPP.roll7, y=ER.roll7, color=quarter)) + geom_point() + theme_bw() + facet_wrap(~ quarter)
ggsave(file.path(outdir, 'gpp_er_cor.png'), width=6, height=4)

#### maps ####

tidyslices <- full_join(tidygpp, tidyer) %>%
  full_join(select(meta, site=site_num, lat, lon) %>% unitted::v())
library(ggmap)
all_states <- map_data("state")
basemap <- ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="grey", fill='white')
basemap + geom_point(data=dplyr::filter(tidyslices, doy==300, year=="2014"), aes(x=lon, y=lat, group=NA, color=gpp), size=4, alpha=0.5) + scale_color_gradient(lim=c(0,0.008), low='navy', high='springgreen3')
ggsave(file.path(outdir, 'gpp_day300.png'), width=8, height=6)
basemap + geom_point(data=dplyr::filter(tidyslices, doy==100, year=="2014"), aes(x=lon, y=lat, group=NA, color=gpp), size=4, alpha=0.5) + scale_color_gradient(lim=c(0,0.008), low='navy', high='springgreen3')
ggsave(file.path(outdir, 'gpp_day100.png'), width=8, height=6)
basemap + geom_point(data=dplyr::filter(tidyslices, doy==200, year=="2014"), aes(x=lon, y=lat, group=NA, color=gpp), size=4, alpha=0.5) + scale_color_gradient(lim=c(0,0.008), low='navy', high='springgreen3')
ggsave(file.path(outdir, 'gpp_day200.png'), width=8, height=6)

quantile(tidyslices$gpp, 0.9, na.rm=TRUE)
  
#### conclusions ####

# differences in peak timing point to systematic differences at the continental
# scale in the timing of light versus organic matter availability, with
# consequent seasonality in net ecosystem productivity
