# Is stream metabolism synchronous within stream orders, ecoregions, and/or hydrologic regimes?

library(streamMetabolizer)
library(dplyr)
library(ggplot2)
library(mda.streams)
library(powstreams)
login_sb()

# Get all metabolism estimates together
# FIRST TIME:
#model_names <- list_metab_models()
#model_names_0.0.7 <- grep('0.0.7 MLE_for_PRK_wHarvey_and_sw', model_names, value=TRUE)
#metab <- get_metab_model(model_names_0.0.7)
# AFTER THAT:
load('temp/metab_0.0.7.RData')
# metab is now in memory, list of 377 metab models

# Convert to data table
metab_table <- bind_rows(lapply(metab, function(met) mutate(met@fit, site=met@info$site)))
metab_table <- metab_table[complete.cases(metab_table),]
metab_table$doy <- as.Date(paste0("2015-",format(metab_table$date, "%m-%d")))
metab_table$year <- format(metab_table$date, "%Y")

# plot everything all together
ggplot(metab_table, aes(x=doy, y=GPP, group=site)) + geom_point(alpha=0.2) + ylim(-100,100)
ggplot(metab_table, aes(x=doy, y=ER, group=site)) + geom_point(alpha=0.2) + ylim(-100,100)
ggplot(metab_table, aes(x=doy, y=NEP, group=site)) + geom_point(alpha=0.2) + ylim(-100,100)

# restrict to data we think could be good
manual <- read.table('../mda.streams/inst/extdata/Manual_site_review.tsv', sep='\t', header=TRUE, stringsAsFactors = FALSE)
manual[manual$accept.reject.examine=="reject ","accept.reject.examine"] <- "reject"
metab_table$accept.reject.examine <- manual[match(metab_table$site, substr(manual$site, 1, nchar(manual$site)-1)), 'accept.reject.examine']

# get annual numbers for AGU abstract
metab_sum <- metab_table %>%
  group_by(site, year) %>%
  summarize(
    max=max(GPP[GPP < quantile(GPP, 0.9)], na.rm=TRUE),
    which.max=format(date[GPP < quantile(GPP, 0.9)][which.max(GPP[GPP < quantile(GPP, 0.9)])][1], "%m-%d"))
metab_sum$doy.max <- as.Date(paste0("2015-",metab_sum$which.max))
ggplot(metab_sum) + geom_density(aes(x=doy.max))
metab_sum$true.doy.max <- as.numeric(metab_sum$doy.max-as.Date("2015-01-01"), units='days')
ggplot(metab_sum) + geom_density(aes(x=true.doy.max))
date_qs <- quantile(metab_sum$true.doy.max, c(0.25, 0.5, 0.75), na.rm=TRUE)
date_m <- mean(metab_sum$true.doy.max, na.rm=TRUE)
as.Date("2015-01-01") + as.difftime(c(date_qs, date_m), units='days')
# ER
metab_sum <- metab_table %>%
  group_by(site, year) %>%
  summarize(
    max=max(ER[ER < quantile(ER, 0.9)], na.rm=TRUE),
    which.max=format(date[ER < quantile(ER, 0.9)][which.max(ER[ER < quantile(ER, 0.9)])][1], "%m-%d"))
metab_sum$doy.max <- as.Date(paste0("2015-",metab_sum$which.max))
ggplot(metab_sum) + geom_density(aes(x=doy.max))
metab_sum$true.doy.max <- as.numeric(metab_sum$doy.max-as.Date("2015-01-01"), units='days')
ggplot(metab_sum) + geom_density(aes(x=true.doy.max))
date_qs <- quantile(metab_sum$true.doy.max, c(0.25, 0.5, 0.75), na.rm=TRUE)
date_m <- mean(metab_sum$true.doy.max, na.rm=TRUE)
as.Date("2015-01-01") + as.difftime(c(date_qs, date_m), units='days')
date_peak1 <- quantile(metab_sum$true.doy.max[metab_sum$true.doy.max<210], c(0.25,0.5,0.75), na.rm=TRUE)
date_peak2 <- quantile(metab_sum$true.doy.max[metab_sum$true.doy.max>=210], c(0.25,0.5,0.75), na.rm=TRUE)
as.Date("2015-01-01") + as.difftime(c(date_peak1, date_peak2), units='days')

metab_table$NEP <- metab_table$GPP + metab_table$ER
metab_sum <- metab_table %>%
  group_by(site, year) %>%
  summarize(
    max=max(NEP[ER < 0 & GPP > 0], na.rm=TRUE),
    which.max=format(date[ER < 0 & GPP > 0][which.max(NEP[ER < 0 & GPP > 0])][1], "%m-%d"))
metab_sum$doy.max <- as.Date(paste0("2015-",metab_sum$which.max))
ggplot(metab_sum) + geom_density(aes(x=doy.max))
metab_sum$true.doy.max <- as.numeric(metab_sum$doy.max-as.Date("2015-01-01"), units='days')
ggplot(metab_sum) + geom_density(aes(x=true.doy.max))
date_qs <- quantile(metab_sum$true.doy.max, c(0.25, 0.5, 0.75), na.rm=TRUE)
date_m <- mean(metab_sum$true.doy.max, na.rm=TRUE)
as.Date("2015-01-01") + as.difftime(c(date_qs, date_m), units='days')
date_peak1 <- quantile(metab_sum$true.doy.max[metab_sum$true.doy.max<210], c(0.25,0.5,0.75), na.rm=TRUE)
 date_peak2 <- quantile(metab_sum$true.doy.max[metab_sum$true.doy.max>=210], c(0.25,0.5,0.75), na.rm=TRUE)
as.Date("2015-01-01") + as.difftime(c(date_peak1, date_peak2), units='days')

# Browse GPP vs doy for year-site combos
sites <- sample(unique(metab_table$site[metab_table$accept.reject.examine %in% c('accept','examine','')])); sites <- setNames(sites, sites))
sites <- sample(list_sites())
xlims <- as.Date(c("2015-01-01","2015-12-31"))
regimes <- lapply(sites[1:20], function(s) {
  s <- sample(sites, 1)
  dat <- metab_table[metab_table$site==s & !is.na(metab_table$NEP), ]
  ylims <- quantile(dat$NEP, c(0.10,0.90))
  tryCatch(suppressWarnings(print(ggplot(dat, aes(x=doy, y=NEP, color=year)) + ylim(ylims) + xlim(xlims) + 
          geom_hline(yintercept=0, color='gray') + geom_point() + geom_line() + theme_bw() +
          facet_wrap(~year))), error=function(e) e)
  print(get_site_info(s))
  Sys.sleep(7)
#   data.frame(
#     site = s,
#     bumps = readline("bumps? #:"),
#    flashy = readline("flashy? y/n: "), 
#     stringsAsFactors=FALSE)
  2
})

s <- sites[20]
y <- 2012
find_peaks <- function(s, y) {
  dat <- metab_table[metab_table$site==s & metab_table$year==y, ]
  dat$cumsumGPP <- cumsum(dat$GPP-mean(dat$GPP))
  ggplot(dat, aes(x=date, y=cumsumGPP)) + geom_point()
  filtered <- data.frame(filt=c(stats::filter(dat$GPP, filter=rep(1/5, 5), sides=2)))
  #filtered <- data.frame(filt=smwrBase::movingAve(dat$GPP, span=31, order=0))
  filtered$peak <- smwrBase::peaks(-filtered$filt, span=5, ties='middle')
  filtered$date <- dat$date
  cutoff <- quantile(filtered$filt, 0.9, na.rm=TRUE)
  print(ggplot(filtered, aes(x=date, y=filt, color=peak)) + geom_point(size=4) + geom_hline(yintercept=cutoff) + theme_bw())
}  

smooth_quantile <- function(s, y, q=0.9) {
  dat <- metab_table[metab_table$site==s & metab_table$year==y, ]
  dat$quantile <- NA
  for(t in 31:(nrow(dat)-30)) {
    dat[t,'quantile'] <- quantile(dat[(t-30):(t+30),]$GPP, q, na.rm=TRUE)
  }
  print(ggplot(dat, aes(x=date, y=GPP)) + geom_point() + geom_line(aes(y=quantile)) + theme_bw())
  print(ggplot(dat, aes(x=date)) + geom_point(aes(y=K600), color='blue') + geom_point(aes(y=GPP), color='green') + theme_bw())
  print(ggplot(dat, aes(x=date, y=K600)) + geom_point() + theme_bw())
}

lapply(sites[1:20], 
#all_regimes <- regimes
all_regimes <- bind_rows(all_regimes, regimes)