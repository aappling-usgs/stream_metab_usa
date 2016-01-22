# Munge: remove obvious outliers from predictions

if((manual=FALSE)) {
  args <- list(outfile='out/clean_1_filter.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)

filter_preds <- function(preds_all, values=c('GPP','ER','K600'), quant_bounds=c(low=0.1, high=0.9), buffer_fracs_range=c(low=0.5, high=1)) {
  lapply(unique(preds_all$site), function(s) {
    preds <- filter(preds_all, site==s)
    val_cols <- lapply(values, function(v) {
      cols_to_pull <- paste0(v, c('','.lower','.upper','.orig')) %>% .[. %in% names(preds)]
      val_cols <- preds %>% select_(.dots=cols_to_pull)
      y <- switch(
        v,
        GPP=val_cols[[v]],
        ER=-val_cols[[v]],
        K600=sqrt(val_cols[[v]]))
      quanty <- quantile(y, quant_bounds, na.rm=TRUE)
      rangy <- quanty[2] - quanty[1]
      miny <- quanty[1] - rangy*buffer_fracs_range[['low']]
      maxy <- quanty[2] + rangy*buffer_fracs_range[['high']]
      keepy <- ifelse(is.na(y), FALSE, miny <= y & y <= maxy)
      origcol <- if(paste0(v,'.orig') %in% names(preds_all)) paste0(v,'.orig') else v
      val_cols[[paste0(v,'.orig')]] <- val_cols[[origcol]]
      val_cols[[paste0(v,'.prev')]] <- val_cols[[v]]
      val_cols[[paste0(v,'.keep')]] <- keepy
      val_cols[!keepy, paste0(v, c('','.lower','.upper'))] <- NA
      val_cols
    })
    bind_cols(c(list(select(preds, local.date)), val_cols, list(select(preds, model, strategy, site))))
  }) %>% bind_rows() %>% mutate(all.keep = GPP.keep & ER.keep & K600.keep)
}

# repeatedly filter; data should stabilize after some number of filtrations (~25 for preds_3_PR)
filter_preds_repeatedly <- function(preds_all, n_iter=25, values=c('GPP','ER','K600'), quant_bounds=c(low=0.1, high=0.9), buffer_fracs_range=c(low=0.5, high=1)) {
  clean_filters <- list(preds_all)
  clean_traj <- bind_rows(c(
    list(summarize(
      clean_filters[[1]],
      GPP.keep=length(which(!is.na(GPP))),
      ER.keep=length(which(!is.na(ER))),
      K600.keep=length(which(!is.na(K600)))
    )),
    lapply(2:(n_iter+1), function(cf) { 
      clean_filters[[cf]] <<- filter_preds(clean_filters[[cf-1]], values=values, quant_bounds=quant_bounds, buffer_fracs_range=buffer_fracs_range)
      clean_filters[[cf]] %>% summarize(
        GPP.keep=length(which(GPP.keep)),
        ER.keep=length(which(ER.keep)),
        K600.keep=length(which(K600.keep)))
    }))
  )
  clean_traj %>% print(n=(n_iter+1))
  clean_filters
}

# the actual analysis
load('cache/preds_3_PR.RData')
clean_filters <- filter_preds_repeatedly(preds_3_PR, n_iter=25)
clean_1_filter <- clean_filters[[25]]
save(clean_1_filter, file=args$outfile)

# visualization for code development above
if((manual=FALSE)) {
  library(ggplot2)
  plot_filter_stages <- function(sitechoice="nwis_06894000", iterchoices=c(2,5,10,15,25), var='GPP') {
    for(i in iterchoices[length(iterchoices):1]) {
      print(
        ggplot(filter(clean_filters[[i]], site==sitechoice), 
               aes_string(x='local.date', y=paste0(var,'.prev'), color=paste0(var,'.keep'))) +
          geom_point() + ggtitle(paste0('var=', var, ', iter=', i)))
    }
  }
  # random sample of sites from preds_3_PR: 
  # "nwis_03298150" "nwis_13213100" "nwis_02458450" "nwis_07056515"
  # "nwis_01570500" "nwis_01649190" "nwis_06601200" "nwis_04087030"
  # "nwis_03293530" "nwis_05418400"
  plot_filter_stages('nwis_02458450', var='K600')
}