# Munge: create list of dfs of GPP & ER & K600 with 365 days per col, 1 col per site-year
# for GAM metabolism predictions (after filtering + GAM)

if((manual=FALSE)) {
  args <- list(outfile='out/siteyears_GAM.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('munge/siteyears_lib.R')

# with GAM data
load('cache/clean_2_GAM.RData')
clean_2_GAM <- clean_2_GAM %>% mutate(GPP=GPP.gam, ER=ER.gam, K600=K600.gam)
siteyears_GAM <- make_siteyears_dfs(clean_2_GAM)
siteyears_GAM_complete <- are_complete_siteyears(siteyears_GAM)
sitegrows_GAM <- make_growyears_dfs(siteyears_GAM)
sitegrows_GAM_complete <- are_complete_siteyears(sitegrows_GAM)
save(siteyears_GAM, siteyears_GAM_complete,
     sitegrows_GAM, sitegrows_GAM_complete, 
     file=args$outfile)
