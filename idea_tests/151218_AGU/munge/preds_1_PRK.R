if((manual=FALSE)) {
  # manually define SBUSER & SBPASS; set up c1 as a cluster
  args <- list(sb_user=SBUSER, sb_password=SBPASS, outfile='cache/preds_1_PRK.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("sb_user", "sb_password", "outfile"))
  library(parallel)
  c1 <- makePSOCKcluster(rep('localhost', parallel::detectCores()-1))
}

source("munge/preds_lib.R")  
mms <- get_mms_with_tag(tag="0.0.13", sb_user=args$sb_user, sb_password=args$sb_password)
preds_1_PRK <- gather_preds(mms, add_K_info=FALSE, clust=c1, sb_user=args$sb_user, sb_password=args$sb_password)
save(preds_1_PRK, file=args$outfile)
