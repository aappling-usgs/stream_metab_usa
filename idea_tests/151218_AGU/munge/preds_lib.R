# get predictions from ScienceBase for all three modeling phases

library(mda.streams)
library(streamMetabolizer)
library(dplyr)

#' Return a vector of model_names with the specified tag
#' 
#' @tag character, e.g., "0.0.17"
#' @sb_user username
#' @sb_password password
get_mms_with_tag <- function(tag, sb_user, sb_password) {
  sbtools::authenticate_sb(sb_user, sb_password)
  mms_all <- list_metab_models()
  grep(tag, mms_all, fixed=T, value=T)
}

#' Pull predictions for a single model name
#' 
#' @param mmname character model_name
#' @param add_K_info logical. Include K600.obs, weight, velocity.daily, etc.?
get_preds <- function(mmname, add_K_info) {
  message("getting preds for ", mmname)
  mm <- get_metab_model(mmname, version='modern', update_sb=FALSE)
  mmp <- parse_metab_model_name(mmname)
  pred <- 
    predict_metab(mm) %>% 
    mutate(
      model = mmname,
      strategy = mmp$strategy,
      site = mmp$site)
  if(add_K_info) {
    k_pred <- get_data_daily(mm)
    pred <- left_join(pred, select(k_pred, local.date, K600.obs, K600.lower.obs, K600.upper.obs, weight, velocity.daily), by='local.date')
  }
  pred
}

#' Pull predictions from a list of model names
#' 
#' @param mmnames vector of model_names
#' @inheritParams get_preds
#' @param clust optional. a cluster, e.g., one created with makePSOCKcluster()
gather_preds <- function(mmnames, add_K_info, clust, sb_user, sb_password) {
  clusterfun <- function(i) {
    if(is.null(current_session()) || !session_validate()) {
      authenticate_sb(sb_user, sb_password)
    }
    get_preds(mmname=mmnames[i], add_K_info=add_K_info)
  }
  all_preds_list <- 
    if(missing(clust) || is.null(clust)) {
      lapply(1:length(mmnames), clusterfun)
    } else {
      clusterCall(clust, function() {
        library(sbtools)
        library(mda.streams)
        library(streamMetabolizer)
        library(dplyr)
      })
      clust_env <- as.environment(list(get_preds=get_preds, mmnames=mmnames, sb_user=sb_user, sb_password=sb_password))
      clusterExport(clust, names(clust_env), envir=clust_env)
      clusterApplyLB(clust, 1:length(mmnames), clusterfun)
    }
  all_preds <- tryCatch({
    all_preds_list %>% bind_rows()
  }, error=function(e) {
    warning(e)
    warning("couldn't bind rows. returning uncombined list")
    all_preds_list
  })
}