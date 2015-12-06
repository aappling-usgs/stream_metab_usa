# Munge: create list of dfs of GPP & ER & K600 with 365 days per col, 1 col per site-year
# for raw metabolism predictions (PR+fixedK, MLE)

if((manual=FALSE)) {
  # manually define SBUSER & SBPASS
  args <- list(sb_user=SBUSER, sb_password=SBPASS, outfile='out/siteyears_raw.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("sb_user", "sb_password", "outfile"))
}

source('munge/siteyears_lib.R')

# with raw data
load('cache/preds_3_PR.RData')
siteyears_raw <- make_siteyears_dfs(preds_3_PR)
siteyears_raw_complete <- are_complete_siteyears(siteyears_raw)
sitegrows_raw <- make_growyears_dfs(siteyears_raw)
sitegrows_raw_complete <- are_complete_siteyears(sitegrows_raw)
save(siteyears_raw, siteyears_raw_complete,
     sitegrows_raw, sitegrows_raw_complete, 
     file=args$outfile)

