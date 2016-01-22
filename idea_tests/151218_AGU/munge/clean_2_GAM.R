# Munge: Smooth data with a GAM filter

if((manual=FALSE)) {
  args <- list(outfile='out/clean_2_GAM.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(mgcv) # gamm
library(lubridate) # decimal_date
source('munge/clean_2_lib.R') # Deriv

#' Use a GAM model to smooth the values of GPP, ER, and K600 (each independent 
#' of the others) over time
#' 
#' @param preds_all the data.frame of cleaned predictions
#' @param values variables for which to do the smoothing
#' @param rows_per_basis the number of rows for which one basis should be 
#'   applied in mgcv::s (smoother in the GAM). see ?mgcv::choose.k and ?mgcv::s
#' @param chunk_gap the time.step without data (with all.keep=T), in days, after
#'   which subsequent data should be modeled as a new GAM chunk rather than
#'   connected to previous data
smooth_preds <- function(preds_all, values=c('GPP','ER','K600'), rows_per_basis=45, chunk_gap=45) {
  lapply(sort(unique(preds_all$site)), function(s) {
    preds_site <- filter(preds_all, site==s) %>%
      # cut off leading and trailing all.keep=FALSEs
      filter(local.date >= min(local.date[all.keep]) & local.date <= max(local.date[all.keep])) %>%
      arrange(local.date)
    # cut into ~continuous chunks to accommodate sites without sensors in winter
    preds_site$time.step <- NA
    preds_site$time.step[preds_site$all.keep] <- c(chunk_gap+1, as.numeric(diff(preds_site$local.date[preds_site$all.keep]), units='days'))
    preds_site <- preds_site %>% mutate(chunk = cumsum(ifelse(is.na(time.step) | time.step <= chunk_gap, 0, 1)))
    
    # fit a model for each chunk and recombine into on df for the site
    preds_site_df <- lapply(sort(unique(preds_site$chunk)), function(ch) {
      preds <- filter(preds_site, chunk==ch)
      # fill out days so timestep=1 day for each year's period with all.keep==T
      daterange <- range(preds %>% filter(all.keep) %>% .[['local.date']])
      gapfill <- data_frame(
        local.date=seq(daterange[1], daterange[2], as.difftime(1, units='days'))) %>%
        mutate(model=preds$model[1], strategy=preds$strategy[1], site=preds$site[1], chunk=preds$chunk[1])
      preds <- preds %>%
        right_join(., gapfill, by=names(gapfill)) %>% 
        mutate(
          all.keep = ifelse(!is.na(all.keep), all.keep, FALSE),
          dec.date = lubridate::decimal_date(local.date))
      num_dates <- length(which(preds$all.keep))
      if(num_dates <= rows_per_basis) return(NULL)
      k <- min(num_dates, max(5, round(num_dates / rows_per_basis)))
      message(sprintf("%18s, #%2s: nrow=%4d, nkeep=%4d, k=%3d", s, ch, nrow(preds), num_dates, k))
      
      # fit the GAM
      val_cols <- lapply(values, function(v) {
        val_cols <- select(preds, starts_with(v), all.keep, dec.date)
        formul <- as.formula(paste0(v, ' ~ s(dec.date, k=',k,')'))
        gam_fit <- gam(formul, data = filter(val_cols, all.keep))
        gam_pred <- predict(gam_fit, newdata=val_cols, se.fit=TRUE)
        val_cols[[paste0(v, '.gam')]] <- gam_pred$fit
        val_cols[[paste0(v, '.gam.lower')]] <- gam_pred$fit - 1.96*gam_pred$se.fit
        val_cols[[paste0(v, '.gam.upper')]] <- gam_pred$fit + 1.96*gam_pred$se.fit
        gam_d <- Deriv(gam_fit, n=nrow(val_cols))
        gam_d_ci <- confint(gam_d)
        val_cols[[paste0(v, '.gam.deriv')]] <- gam_d$dec.date$deriv
        val_cols[[paste0(v, '.gam.deriv.lower')]] <- gam_d_ci$dec.date$lower
        val_cols[[paste0(v, '.gam.deriv.upper')]] <- gam_d_ci$dec.date$upper
        val_cols %>% select(-all.keep, -dec.date)
      })
      
      # bind vars & pred data within chunk
      bind_cols(c(list(select(preds, local.date)), val_cols, list(select(preds, all.keep, gam.chunk=chunk, model, strategy, site))))
      
    }) %>% bind_rows() # bind chunks within site
    
    # bind sites
  }) %>% bind_rows()
}

load('cache/clean_1_filter.RData')
clean_2_GAM <- smooth_preds(clean_1_filter)
save(clean_2_GAM, file=args$outfile)
