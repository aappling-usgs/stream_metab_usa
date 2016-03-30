# Figure: Regression relating storms and metabolism at one site

if((manual=FALSE)) {
  args <- list(outfile='out/storms_2_regression.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(tidyr)
library(ggplot2)

# get data
source("src/example_lib.R")
# nwis_03034000: model of storm responses by time series. declining GPP, increasing ER with big storms
# nwis_07332622: droughts = low metab; storms = declining GPP, increasing ER
# nwis_02160105: meh
# nwis_11462500: really seems to be a pattern of low GPP and not quite as diminished ER at high discharge
egdat <- build_example('nwis_03034000', pred='disch_nwis', ag_fun='mean', dates=as.Date(c("2012-01-01","2015-01-01")))
# # data for typical regressions - NAs OK
# diffs <- bind_cols(
#   egdat[-1,c('local.date','GPP','ER','disch','GPP.gam','ER.gam')],
#   data.frame(
#     diff_GPP = diff(egdat$GPP),
#     diff_ER = diff(egdat$ER),
#     diff_Q = diff(egdat$disch)))
# # time series analyses - NAs bad
# diffs_ts <- diffs[complete.cases(diffs),]
egdat <- egdat[complete.cases(select(egdat, local.date, disch, ER, GPP)),]

# basic plots really aren't bad. remember that lags are already covered a bit by using daily means
g <- ggplot(gather(select(egdat, local.date, GPP, ER, disch), var, val, GPP, ER), aes(x=disch, y=lag(val,0))) + 
  geom_point(color='orange') + 
  facet_grid(var ~ ., scales='free_y') + 
  scale_x_log10(breaks=c(1,3,10,30,100,300,1000,3000)) + 
  #geom_smooth(method='lm', color='navy') +
  theme_classic() + theme(strip.text=element_blank(), strip.background=element_blank()) +
  ylab(parse(text=paste('atop(ER,gO[2]~m^-2~d^-1)', 'atop(GPP,gO[2]~m^-2~d^-1)', sep='~"        "~'))) + 
  xlab(parse(text="Discharge~(ft^3~s^-1)"))
ggsave(args$outfile, plot=g, width=5, height=4)

# # get my bearings - ts plots
# ggplot(diffs, aes(x=local.date, y=GPP)) + geom_line()
# ggplot(diffs, aes(x=local.date, y=diff_GPP)) + geom_line()
# ggplot(diffs, aes(x=local.date, y=ER)) + geom_line()
# ggplot(diffs, aes(x=local.date, y=diff_ER)) + geom_line()
# 
# # time series analyses (cross correlations)
# ccf(diffs_ts$GPP - diffs_ts$GPP.gam, log(diffs_ts$disch), lag.max=10) # -0.42 at lag 0
# ccf(diffs_ts$GPP, log(diffs_ts$disch), lag.max=10) # -0.4 at lag 0
# ccf(diffs_ts$diff_GPP, diffs_ts$diff_Q, lag.max=10)  # -0.3 at lag 0
# ccf(diffs_ts$GPP - diffs_ts$GPP.gam, diffs_ts$diff_Q, lag.max=10) # -0.16 at lag 1
# ccf(diffs_ts$GPP, diffs_ts$diff_Q, lag.max=10) # -0.15 at lag 1
# ccf(diffs_ts$diff_GPP, diffs_ts$disch, lag.max=10) # -0.12 at lag 0, 0.13 at lag 2
# 
# ccf(diffs_ts$diff_ER, diffs_ts$diff_Q, lag.max=10)  # -0.25 at lag 0, 0.25 at lag 1
# ccf(diffs_ts$ER - diffs_ts$ER.gam, diffs_ts$diff_Q, lag.max=10) # -0.25 at lag 0
# ccf(diffs_ts$diff_ER, diffs_ts$disch, lag.max=10) # 0.2 at lag 1
# ccf(diffs_ts$ER - diffs_ts$ER.gam, diffs_ts$disch, lag.max=10) # 0.12 at lag 1-5
# ccf(diffs_ts$ER, diffs_ts$disch, lag.max=10) # -0.12 at lag -2 to 0
# ccf(diffs_ts$ER, diffs_ts$diff_Q, lag.max=10) # -0.18 at lag 0  #ggplot(diffs_ts, aes(x=diff_Q, y=ER)) + geom_point() + stat_smooth(method='lm')
