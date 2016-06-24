library(mda.streams)

# list of models in the final batch (PR + fixed K)
make_clusterfun <- function(mms, sb_user, sb_pass) {
  force(mms)
  force(sb_user)
  force(sb_pass)
  function(mnum) {
    if(!sbtools::is_logged_in()) sbtools::authenticate_sb(sb_user, sb_pass)
    mm <- get_metab_model(mms[mnum], on_local_exists='skip', version='original', update_sb=FALSE)
    staged <- stage_metab_ts(mm)
    posted <- post_ts(staged, on_exists='replace')
    Sys.sleep(3)
    repair_ts(c('gpp_estBest','er_estBest','K600_estBest'), mm@info$config$site[1])
    posted
  }
}
sbtools::authenticate_sb(SBUSER, SBPASS)
mms <- list_metab_models("0.1.2")
clusterfun <- make_clusterfun(mms, SBUSER, SBPASS)

clusterfun(1)
test1 <- get_ts('gpp_estBest', 'nwis_01104430')
test1 <- get_ts(c('gpp_estBest','er_estBest','K600_estBest'), 'nwis_01104430')
out1 <- clusterApplyLB(c1, 2, clusterfun)
out1 <- clusterApplyLB(c1, 3, clusterfun)
out2 <- clusterApplyLB(c1, 4:length(mms), clusterfun)

# look for cleanup needs
all <- parse_metab_model_name(mms, out='site', use_names=FALSE)
gpps <- list_sites('gpp_estBest')
ers <- list_sites('er_estBest')
K600s <- list_sites('K600_estBest')
setdiff(all, gpps)
setdiff(all, ers)
setdiff(all, K600s)
gpps <- locate_ts('gpp_estBest', all)
length(which(!is.na(gpps)))
# looks good! 392 apiece.
# 4/23/2016