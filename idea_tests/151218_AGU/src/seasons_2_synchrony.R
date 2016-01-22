# Figure: Measure of synchrony within vs across sites

if((manual=FALSE)) {
  args <- list(outfile='out/seasons_2_synchrony.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(synchrony)
library(ggplot2)
load('cache/siteyears_clean.RData')
load('cache/siteyears_GAM.RData')

# df describing the columns of each data.frame in siteyears_GAM
siteyear_cols <- data_frame(
  siteyear = names(siteyears_GAM[[1]]),
  site = sapply(strsplit(siteyear, ":"), `[`, 1),
  year = as.numeric(sapply(strsplit(siteyear, ":"), `[`, 2)),
  complete_year_GAM = siteyears_GAM_complete[names(siteyears_GAM[[1]])],
  complete_grow_GAM = sitegrows_GAM_complete[names(siteyears_GAM[[1]])],
  complete_year_clean = siteyears_clean_complete[names(siteyears_clean[[1]])],
  complete_grow_clean = sitegrows_clean_complete[names(siteyears_clean[[1]])])

# function to calculate synchrony within a single set of siteyears and return a 1-row df of results
calc_sync <- function(yeartype=c('year','grow'), smoothtype=c('GAM','clean'), var=c('GPP','ER','K600'), site=NA, year=NA, method='kendall') {
  
  # check arguments
  yeartype <- match.arg(yeartype)
  smoothtype <- match.arg(smoothtype)
  var <- match.arg(var)
  if((!is.na(site) && !is.na(year)) || (is.na(site) && is.na(year))) stop("specify exactly one of site or year")
  
  # select data columns
  complete_col <- paste('complete', yeartype, smoothtype, sep='_')
  if(!is.na(site)) {
    mycols <- siteyear_cols$siteyear[siteyear_cols[[complete_col]] & siteyear_cols$site==site]
  } else {
    mycols <- siteyear_cols$siteyear[siteyear_cols[[complete_col]] & siteyear_cols$year==year]
  }
  dat <- switch(
    smoothtype,
    GAM=switch(yeartype, year=siteyears_GAM, grow=sitegrows_GAM),
    clean=switch(yeartype, year=siteyears_clean, grow=sitegrows_clean))
  dat <- dat[[var]][mycols]
  dat <- dat[complete.cases(dat),]
  mat <- as.matrix(dat)
  
  # measure synchrony among the data columns
  if(length(mycols) > 1) {
    if(ncol(mat) > 7) {
      syncs <- matrix(NA, nrow=100, ncol=3)
      for(i in 1:100) {
        colsub <- sample(ncol(mat), 7, replace=FALSE)
        matsub <- mat[1:nrow(mat), colsub]
        syncs[i,1:3] <- unlist(synchrony::community.sync(matsub, nrands=1, method=method)[c('obs','meancorr','pval')])
      }
      sync <- as.data.frame(t(apply(syncs, MARGIN=2, FUN=mean))) %>% setNames(c("sync","meancorr","sync.pval"))
    } else {
      sync <- as.data.frame(synchrony::community.sync(mat, nrands=1, method=method)[c('obs','meancorr','pval')]) %>% setNames(c("sync","meancorr","sync.pval"))
    }
    data.frame(site=site, year=year, sync, ncol=length(mycols), nrow=nrow(mat), stringsAsFactors=FALSE)
  } else {
    data.frame(site=site, year=year, sync=0, meancorr=0, sync.pval=0, ncol=length(mycols), nrow=nrow(mat), stringsAsFactors = FALSE)[c(), ]
  }
}

# run the calculations & combine results
sync_withinsite <- lapply(unique(siteyear_cols$site), function(usite) {
  calc_sync('year', 'GAM', 'GPP', site=usite, method='kendall')
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
sync_withinsite

sync_withinyear <- lapply(unique(siteyear_cols$year), function(uyear) {
  calc_sync('year', 'GAM', 'GPP', year=uyear, method='kendall')
}) %>% bind_rows() %>% dplyr::filter(ncol>0)
sync_withinyear

sync_all <- bind_rows(
  mutate(sync_withinsite, within='site', across='years'),
  mutate(sync_withinyear, within='year', across='sites'))

# plot
#ggplot(sync_all, aes(x=meancorr, y=sync)) + geom_point() + theme_classic()

g <- ggplot(sync_all, aes(x=meancorr, fill=across)) + 
  geom_density(color=NA, alpha=0.8, adjust=2) + 
  scale_fill_manual('Synchrony\nacross', breaks=c('sites','years'), values=c(sites='navy', years='orange')) +
  theme_classic() + xlab("Mean correlation") + ylab("Density")
ggsave(args$outfile, plot=g, width=4, height=3)
