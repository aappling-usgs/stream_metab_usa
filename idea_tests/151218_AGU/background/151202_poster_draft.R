# What are the drivers of synchrony and divergence in the temporal patterns of
# metabolism among streams of North America?

library(mda.streams)
library(streamMetabolizer)
library(dplyr)
library(unitted)
library(tidyr)
library(ggplot2)
outdir <- "idea_tests/151218_AGU/out"

#### sciencebase downloads ####

# get data from ScienceBase
sbtools::authenticate_sb(SBUSER, SBPASS)
# final PR(fixK) model predictions
mms <- grep("0.0.17", list_metab_models(), fixed=T, value=T)
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
    select(local.time=DateTime, local.date=sitedate, site, disch=dischdaily, veloc=velocdaily)
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

# pull K regressions
mmks <- grep("0.0.16", list_metab_models(), fixed=TRUE, value=TRUE)
mmkfits <- lapply(mmks, function(mmk) {
  mm <- get_metab_model(mmk, version='original', update_sb=FALSE)
  get_fit(mm)
})
Kcoefs <- mmkfits %>% lapply(function(fit) { 
  data_frame(
    intercept = coef(fit)[['(Intercept)']],
    slope = coef(fit)[['log(velocity.daily)']],
    rsq = summary(fit)$adj.r.squared)
} ) %>% bind_rows() %>% mutate(model_name=mmks)
save(Kcoefs, file=file.path(outdir,'Kcoefs.RData'))
ggplot(Kcoefs, aes(x=intercept, y=slope, color=rsq)) + geom_point(alpha=0.8) + theme_bw()
ggsave(file.path(outdir, "KvVscatter.png"), width=5, height=4)
ggplot(Kcoefs, aes(x=rsq)) + geom_density(fill='springgreen4', color=NA, alpha=0.8) + theme_bw() + xlab(parse(text="r^2")) + ylab("Density")
ggsave(file.path(outdir, "KvVrsq.png"), width=5, height=4)
filter(Kcoefs, rsq > 0.3 & slope > 0 & intercept > 0)

#### methods ####

# show how modeling worked
mmK <- get_metab_model("nwis_01646500-31-151203 0.0.16 K_smooth")
ggplot(get_data_daily(mmK), aes(x=velocity.daily, y=K600.obs, color=weight)) + geom_point() + geom_line(aes(y=K600)) + theme_bw() + ylim(0,50)
ggsave(file.path(outdir, "KvV.png"), width=5, height=4)
load('idea_tests/151218_AGU/out/preds.RData')
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
ggplot(dplyr::filter(eg_preds, phase %in% c('1_PRK')), aes(x=local.date, y=ER, color=phase)) + geom_point(size=0.8, alpha=0.8) + theme_bw() + ylim(-45,15)
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

# notes when a prediction for K600 is unavailable for a day, 0.0.15 predicts
# PRK. In this case K600 is usually (always?) NaN. If I hadn't weighted the
# predictions by CI, would these days be available?
