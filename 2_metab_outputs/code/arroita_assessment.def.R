
library(dplyr)
library(unitted)

fitdir <- 'D:/Biologia/Limnologia/METABOLISMOA/EGONALDIA/POWELL CENTER/Modelling/ModelAssessment/fits'
fitfiles <- dir(fitdir, full.names = TRUE)

dailytot <- bind_rows(lapply(fitfiles, function(fitfile) {
  
  # read the KQ file (K values)
  kqfile <- file.path(fitfile, 'daily.tsv')
  kq <- read.table(kqfile, sep='\t', header=TRUE, stringsAsFactors=FALSE)
  
  # get the site name and model resolution
  mminfo <- mda.streams::parse_metab_model_name(basename(fitfile))
  
  # create the df
  out <- kq %>%
    select(
      date=date,
      GPP_daily_50pct=GPP_daily_50pct,
      ER_daily_50pct=ER_daily_50pct,
      K600_daily_50pct=K600_daily_50pct,
      K600_daily_predlog_50pct=K600_daily_predlog_50pct) %>%
    mutate(
      site=mminfo$site, 
      resolution=substring(mminfo$strategy, 7),
      model=paste(site,resolution)) %>%
    select(site, resolution, model, date, GPP_daily_50pct, ER_daily_50pct, K600_daily_50pct, K600_daily_predlog_50pct)
  
  
  # return
  out
}))


KQ_overall <- bind_rows(lapply(fitfiles, function(fitfile) {
  
  # read the KQ file (K values)
  kqfile <- file.path(fitfile, 'KQ_overall.tsv')
  kq <- read.table(kqfile, sep='\t', header=TRUE, stringsAsFactors=FALSE)
  
  # get the site name and model resolution
  mminfo <- mda.streams::parse_metab_model_name(basename(fitfile))
  
  # create the df
  out <- kq %>%
    select(
      K600_daily_sigma_Rhat=K600_daily_sigma_Rhat) %>%
    mutate(
      site=mminfo$site, 
      resolution=substring(mminfo$strategy, 7),
      model=paste(site,resolution)) %>%
    select(site, resolution, model, K600_daily_sigma_Rhat)
  
  
  # return
  out
}))


overall <- bind_rows(lapply(fitfiles, function(fitfile) {
  
  # read the KQ file (K values)
  kqfile <- file.path(fitfile, 'overall.tsv')
  kq <- read.table(kqfile, sep='\t', header=TRUE, stringsAsFactors=FALSE)
  
  # get the site name and model resolution
  mminfo <- mda.streams::parse_metab_model_name(basename(fitfile))
  
  # create the df
  out <- kq %>%
    select(
      err_obs_iid_sigma_Rhat=err_obs_iid_sigma_Rhat,
      err_proc_iid_sigma_Rhat=err_proc_iid_sigma_Rhat) %>%
    mutate(
      site=mminfo$site, 
      resolution=substring(mminfo$strategy, 7),
      model=paste(site,resolution)) %>%
    select(site, resolution, model, err_obs_iid_sigma_Rhat, err_proc_iid_sigma_Rhat)
  
  
  # return
  out
}))



model<-unique(dailytot$model)
K600_daily_sigma_Rhat<-rep(0,length(model))
err_obs_iid_sigma_Rhat<-rep(0,length(model))
err_proc_iid_sigma_Rhat<-rep(0,length(model))
K_range<-rep(0,length(model))
neg_GPP<-rep(0,length(model))
pos_ER<-rep(0,length(model))

assess<-data.frame(model, K600_daily_sigma_Rhat, err_obs_iid_sigma_Rhat, err_proc_iid_sigma_Rhat, K_range, neg_GPP, pos_ER)



for (i in 1:length(unique(dailytot$model))) {
  assess$K600_daily_sigma_Rhat[i]<-KQ_overall$K600_daily_sigma_Rhat[i]
  assess$err_obs_iid_sigma_Rhat[i]<-overall$err_obs_iid_sigma_Rhat[i]
  assess$err_proc_iid_sigma_Rhat[i]<-overall$err_proc_iid_sigma_Rhat[i]
  
  data<-dailytot[dailytot$model==unique(dailytot$model)[i],]
  assess$K_range[i]<-quantile(data$K600_daily_50pct, 0.9, na.rm = T)-quantile(data$K600_daily_50pct, 0.1, na.rm = T)
  assess$neg_GPP[i]<-length(which(data$GPP_daily_50pct<(-0.5)))/length(which(!is.na(data$GPP_daily_50pct)))*100
  assess$pos_ER[i]<-length(which(data$ER_daily_50pct>0))/length(which(!is.na(data$ER_daily_50pct)))*100
  
}

#Assessment<-assess

#write.table(Assessment, "D:/Biologia/Limnologia/METABOLISMOA/EGONALDIA/POWELL CENTER/Modelling/ModelAssessment/Assessment.txt",sep=";")



assess$Confidence<-ifelse(
  assess$K600_daily_sigma_Rhat>1.2, "L", ifelse(
    assess$err_obs_iid_sigma_Rhat>1.2, "L", ifelse(
      assess$err_proc_iid_sigma_Rhat>1.2, "L", ifelse(
        assess$K_range>50, "L", ifelse(
          assess$neg_GPP>50, "L", ifelse(
            assess$pos_ER>50, "L", ifelse(
              assess$K_range>15, "M", ifelse(
                assess$neg_GPP>25, "M", ifelse(
                  assess$pos_ER>25, "M", "H"
                )
              )
            )
          )
        )
      )
    )
  )
)


ref<-read.csv("D:/Biologia/Limnologia/METABOLISMOA/EGONALDIA/POWELL CENTER/Modelling/ModelAssessment/Ref.csv", header = T, sep = ";")
str(ref)

id<-ref$model_name_run3
K600_daily_sigma_Rhat<-assess$K600_daily_sigma_Rhat[match(ref$REF,assess$model)]
err_obs_iid_sigma_Rhat<-assess$err_obs_iid_sigma_Rhat[match(ref$REF,assess$model)]
err_proc_iid_sigma_Rhat<-assess$err_proc_iid_sigma_Rhat[match(ref$REF,assess$model)]
K_range<-assess$K_range[match(ref$REF,assess$model)]
neg_GPP<-assess$neg_GPP[match(ref$REF,assess$model)]
pos_ER<-assess$pos_ER[match(ref$REF,assess$model)]
confidence<-assess$Confidence[match(ref$REF,assess$model)]

assessment<-data.frame(id, K600_daily_sigma_Rhat, err_obs_iid_sigma_Rhat, err_proc_iid_sigma_Rhat, K_range, neg_GPP, pos_ER, confidence)
Assessment2<-assessment

write.table(Assessment2, "D:/Biologia/Limnologia/METABOLISMOA/EGONALDIA/POWELL CENTER/Modelling/ModelAssessment/Assessment2.txt",sep=";")
