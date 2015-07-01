library(mda.streams)
library(dplyr)
library(unitted)
library(streamMetabolizer)
library(lubridate)
library(ggplot2)

#### simulate data ####

# start with real wtr data
real_site <- "nwis_02207135"
dat <- read_ts(download_ts("wtr_nwis", real_site))
times <- data.frame(start_date=as.POSIXct("2014-03-8 22:30:00", tz="UTC"), 
                    end_date=as.POSIXct("2014-03-12 06:00:00", tz="UTC"), stringsAsFactors = FALSE)
dataRetrieval::readNWISsite(mda.streams:::parse_site_name(real_site))
find_site_coords(real_site)

# real_doobs
real_doobs <- read_ts(download_ts("doobs_nwis", real_site))
real_doobs <- real_doobs[real_doobs$DateTime >= times$start_date & real_doobs$DateTime <= times$end_date, ]

# wtr
wtr <- dat[dat$DateTime >= times$start_date & dat$DateTime <= times$end_date, ]

# suntime
file_suntime <- stage_calc_ts(
  sites="styx_001001", var="suntime", src="simLon", verbose=TRUE, 
  inputs=list(utctime=wtr$DateTime, longitude=find_site_coords(real_site)$lon))
suntime <- read_ts(file_suntime)

# depth
file_depth <- stage_calc_ts(
  sites="styx_001001", var="depth", src="simNew", verbose=TRUE,
  inputs=list(utctime=wtr$DateTime, value=u(rep(1, nrow(wtr)), "m")))
depth <- read_ts(file_depth)

# dosat
dosat <- read_ts(stage_calc_ts(
  sites="styx_001001", var="dosat", src="simGG", verbose=TRUE,
  inputs=list(utctime=wtr$DateTime, wtr=wtr$wtr, baro=u(75000, "Pa"))))

# par
par <- read_ts(stage_calc_ts(
  sites="styx_001001", var="par", src="simLat", verbose=TRUE,
  inputs=list(utctime=wtr$DateTime, suntime=suntime$suntime, latitude=find_site_coords(real_site)$lat)))

# combine data
most_data <- v(u(data.frame(
  date.time=suntime$suntime, 
  DO.sat=dosat$dosat, 
  depth=depth$depth, 
  temp.water=wtr$wtr, 
  light=par$par)))

# filter to the exact 1-day (30.5 hr) window we want
true_times <- data.frame(start_date=as.POSIXct("2014-03-09 22:30:00", tz="UTC"), 
                         end_date=as.POSIXct("2014-03-11 06:00:00", tz="UTC"), stringsAsFactors = FALSE)
keep_rows <- c(start=which.min(abs(as.numeric(most_data$date.time - true_times$start_date, units="mins"))),
               end=which.min(abs(as.numeric(most_data$date.time - true_times$end_date, units="mins"))))
most_data <- most_data[keep_rows[1]:keep_rows[2], ]

# count the number of observations per day (24-hr period)
obs_per_day <- nrow(most_data[as.character(as.Date(most_data$date.time)) == "2014-03-10", ])

#### try out metab_bayes_simple ####
# doobs
p <- data.frame(do1=7.6, GPP=2, ER=-2.6, k=15, stream="hiGPPER hik", error="none", oesd=0, oephi=0, pesd=0, pephi=0)
GPP.frac <- most_data$light/sum(most_data$light[as.character(as.Date(most_data$date.time))=="2014-03-10"])
doobs <- calc_DO_mod_with_error(
  GPP.daily = p$GPP, ER.daily = p$ER, K600.daily = p$k, 
  DO.sat = most_data$DO.sat, depth = most_data$depth, temp.water = most_data$temp.water, 
  frac.GPP = GPP.frac, frac.ER = 1/obs_per_day, frac.D = 1/obs_per_day, 
  DO.mod.1 = p$do1, n = nrow(most_data), 
  obs.err.sd = p$oesd, obs.err.phi = p$oephi, proc.err.sd = p$pesd, proc.err.phi = p$pephi)
plot(doobs)
bayes_data <- mutate(most_data, DO.obs=doobs)
#model
# jags_data <- prepjags_bayes_simple(bayes_data)
# jags_out <- runjags_bayes_simple(jags_data, maxCores=4, adaptSteps=100, burnInSteps=400, numSavedSteps=1000, thinSteps=10)
# jags_out
bayes_out <- metab_bayes_simple(bayes_data, maxCores=4, adaptSteps=1000, burnInSteps=1000, numSavedSteps=1000, thinSteps=1)
predict_metab(bayes_out)
bayes_preds <- predict_DO(bayes_out)
ggplot(bayes_preds, aes(x=date.time, group=date)) + geom_point(aes(y=DO.obs), color='blue') + geom_line(aes(y=DO.mod), color='red') + theme_bw()

#### try out metab_mle ####

metab_mle(data=bayes_data, calc_DO_fun=calc_DO_mod_by_diff)

#### specify and run experiments ####

# params for model experiment runs
numerrs <- 15
numstreams <- 4
params <- 
  data.frame(
    do1=rep(c(7, 6, 7.6, 7.4), times=numerrs), # manual tweaks to make both midnight DOs essentially the same
    GPP=rep(c(10,10,2,2), times=numerrs),
    k=rep(c(15, 2.5, 15, 2.5), times=numerrs),
    stream=rep(c("hiGPPER hik", "hiGPPER lok", "loGPPER hik", "loGPPER lok"), times=numerrs),
    error=rep(c("none","obs1","obs2","obs3","obs4","proc1","proc2","proc3","proc4","proc5","autoproc1","autoproc2","autoproc3","autoproc4","autoproc5"), each=numstreams),
    oesd= rep(c(0,0.05,0.1,0.2,0.4,    0,   0,   0,   0,   0,   0,   0,   0,   0,   0), each=numstreams),
    oephi=rep(c(0,   0,  0,  0,  0,    0,   0,   0,   0,   0,   0,   0,   0,   0,   0), each=numstreams),
    pesd= rep(c(0,   0,  0,  0,  0,0.005,0.01,0.02,0.04,0.08,0.04,0.04,0.04,0.04,0.04), each=numstreams),
    pephi=rep(c(0,   0,  0,  0,  0,    0,   0,   0,   0,   0, 0.2, 0.4, 0.6, 0.8, 0.9), each=numstreams)) %>%
  mutate(
    ER=-GPP*1.3)
params <- params %>%
  mutate(error = ordered(error, levels=params[0+seq(1,nrow(params),by=numstreams),"error"]))

fits <- NULL
true_est <- NULL
true_ests <- bind_rows(lapply(1, function(iter) {
  
  print(paste("iteration", iter))
  
  fits <<- lapply(1:nrow(params), function(ip) {
    p <- params[ip,]
    
    # doobs
    GPP.frac <- most_data$light/sum(most_data$light[as.character(as.Date(most_data$date.time))=="2014-03-10"])
    doobs <- calc_DO_mod_with_error(
      GPP.daily = p$GPP, ER.daily = p$ER, K600.daily = p$k, 
      DO.sat = most_data$DO.sat, depth = most_data$depth, temp.water = most_data$temp.water, 
      frac.GPP = GPP.frac, frac.ER = 1/obs_per_day, frac.D = 1/obs_per_day, 
      DO.mod.1 = p$do1, n = nrow(most_data), 
      obs.err.sd = p$oesd, obs.err.phi = p$oephi, proc.err.sd = p$pesd, proc.err.phi = p$pephi)
    all_data <- mutate(most_data, DO.obs=doobs)
    
    #metab_mle(data=all_data, calc_DO_fun=calc_DO_mod_by_diff)
    metab_bayes_simple(all_data, maxCores=4, adaptSteps=100, burnInSteps=500, numSavedSteps=500, thinSteps=1)
  })
  
  true_est <<- bind_rows(lapply(1:length(fits), function(fitnum) {
    p <- params[fitnum,c("GPP","ER","k","stream","error","oesd","pesd","pephi")]
    fit <- fits[[fitnum]]
    #est <- mutate(predict_metab(fit)[2,c("GPP","ER","K600")], GPP.sd=NA, ER.sd=NA, K600.sd=NA)
    est <- get_fit(fit)[2,c("GPP","GPP.sd","ER","ER.sd","K600","K600.sd")]
    data.frame(true=p, est=est)
  })) %>% mutate(iter=iter)
  
  true_est
}))
#save(params, true_ests, file="starter_files/quick_experiment_2.RData")

# true_est_bayes <- true_est
# true_ests_bayes <- true_ests
# fits_bayes <- fits

#### plot model metrics/errors ####

# one of each error type
oneeach <- filter(true_ests, true.error%in%c("none","obs2","proc2","autoproc2"))
ggplot(oneeach, aes(x=true.k, y=est.K600, color=true.error, shape=true.stream)) + geom_abline(color="grey") + geom_point(size=2, alpha=0.9, position=position_jitter(width=.2, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE)
ggplot(oneeach, aes(x=true.ER, y=est.ER, color=true.error, shape=true.stream)) + geom_abline(color="grey") + geom_point(size=2, alpha=0.9, position=position_jitter(width=.2, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE)
ggplot(oneeach, aes(x=true.GPP, y=est.GPP, color=true.error, shape=true.stream)) + geom_abline(color="grey") + geom_point(size=2, alpha=0.9, position=position_jitter(width=.2, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE)

# variation in obs error
obserrs <- filter(true_ests, true.pesd==0)
ggplot(obserrs, aes(x=true.oesd, y=est.K600-true.k, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.K600.sd, ymax=1.96*est.K600.sd))
ggplot(obserrs, aes(x=true.oesd, y=est.ER-true.ER, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.ER.sd, ymax=1.96*est.ER.sd))
ggplot(obserrs, aes(x=true.oesd, y=est.GPP-true.GPP, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.GPP.sd, ymax=1.96*est.GPP.sd))

# variation in proc error
procerrs <- filter(true_ests, true.oesd==0 & true.pephi==0)
ggplot(procerrs, aes(x=true.pesd, y=est.K600-true.k, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.K600.sd, ymax=1.96*est.K600.sd))
ggplot(procerrs, aes(x=true.pesd, y=est.ER-true.ER, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.ER.sd, ymax=1.96*est.ER.sd))
ggplot(procerrs, aes(x=true.pesd, y=est.GPP-true.GPP, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.GPP.sd, ymax=1.96*est.GPP.sd))

# variation in proc error
aperrs <- filter(true_ests, substr(true.error, 1, 4) %in% c("none","auto"))
ggplot(aperrs, aes(x=true.pephi, y=est.K600-true.k, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.K600.sd, ymax=1.96*est.K600.sd))
ggplot(aperrs, aes(x=true.pephi, y=est.ER-true.ER, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.ER.sd, ymax=1.96*est.ER.sd))
ggplot(aperrs, aes(x=true.pephi, y=est.GPP-true.GPP, color=true.stream)) + geom_point(size=2, alpha=0.9, position=position_jitter(width=.0005, height=0)) + theme_bw() + scale_shape_discrete(solid=FALSE) + facet_wrap(~true.stream, scales="free_y") + scale_color_discrete(guide=FALSE) + geom_errorbar(aes(y=0, ymin=-1.96*est.GPP.sd, ymax=1.96*est.GPP.sd))
aperrs %>% group_by(true.stream, true.error) %>% summarize(
  GPP.err=mean(est.GPP-true.GPP), GPP.err.sd=sd(est.GPP-true.GPP),
  ER.err=mean(est.ER-true.ER), ER.err.sd=sd(est.ER-true.ER), 
  k.err=mean(est.K600-true.k), k.err.sd=sd(est.K600-true.k))

#### plot the DO observations and predictions ####

#' requires that you've run the inner loop manually to get a true_est df and a fits list
plot_fit_DO <- function(stream, errtype, fitnum) {
  if(missing(fitnum)) {
    if(is.numeric(stream)) stream <- levels(params$stream)[stream]
    if(is.numeric(errtype)) errtype <- levels(params$error)[errtype]
    fitnum <- which(params$stream==stream & params$error==errtype)
  }
  print(as.data.frame(true_est[fitnum,]))
  fit <- fits[[fitnum]]
  predobs <- predict_DO(fit)
  suppressWarnings(print(
    ggplot(predobs, aes(x=date.time, y=DO.obs)) + geom_point(color="blue") + geom_line(aes(y=DO.mod), color="red")))
}
#levels(true_est$true.stream)
#levels(true_est$true.error)
#plot_fit_DO(f=1)
#plot_fit_DO("hiGPPER hik", "none")
#plot_fit_DO(1, 2)

# best case stream, obs err
plot_fit_DO("hiGPPER lok", "none")
plot_fit_DO("hiGPPER lok", "obs1")
plot_fit_DO("hiGPPER lok", "obs2")
plot_fit_DO("hiGPPER lok", "obs3")
plot_fit_DO("hiGPPER lok", "obs4")
# worst case stream, obs err
plot_fit_DO("loGPPER hik", "none")
plot_fit_DO("loGPPER hik", "obs1")
plot_fit_DO("loGPPER hik", "obs2")
plot_fit_DO("loGPPER hik", "obs3")
plot_fit_DO("loGPPER hik", "obs4")
# best case stream, proc err
plot_fit_DO("hiGPPER lok", "none")
plot_fit_DO("hiGPPER lok", "proc1")
plot_fit_DO("hiGPPER lok", "proc2")
plot_fit_DO("hiGPPER lok", "proc3")
plot_fit_DO("hiGPPER lok", "proc4")
plot_fit_DO("hiGPPER lok", "proc5")
# worst case stream, proc err
plot_fit_DO("loGPPER hik", "none")
plot_fit_DO("loGPPER hik", "proc1")
plot_fit_DO("loGPPER hik", "proc2")
plot_fit_DO("loGPPER hik", "proc3")
plot_fit_DO("loGPPER hik", "proc4")
plot_fit_DO("loGPPER hik", "proc5")
# best case stream, autoproc err
plot_fit_DO("hiGPPER lok", "none")
plot_fit_DO("hiGPPER lok", "autoproc1")
plot_fit_DO("hiGPPER lok", "autoproc2")
plot_fit_DO("hiGPPER lok", "autoproc3")
plot_fit_DO("hiGPPER lok", "autoproc4")
plot_fit_DO("hiGPPER lok", "autoproc5")
# worst case stream, autoproc err
plot_fit_DO("loGPPER hik", "none")
plot_fit_DO("loGPPER hik", "autoproc1")
plot_fit_DO("loGPPER hik", "autoproc2")
plot_fit_DO("loGPPER hik", "autoproc3")
plot_fit_DO("loGPPER hik", "autoproc4")
plot_fit_DO("loGPPER hik", "autoproc5")

