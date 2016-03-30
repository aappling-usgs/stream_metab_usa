# Munge: create list of dfs of GPP & ER & K600 with 365 days per col, 1 col per site-year
# for clean metabolism predictions (after filtering)

if((manual=FALSE)) {
  args <- list(outfile='out/siteyears_clean.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('munge/siteyears_lib.R')

# with clean data
load('cache/clean_2_GAM.RData')
clean_2_GAM[!clean_2_GAM$all.keep, c('GPP','ER','K600')] <- NA
siteyears_clean <- make_siteyears_dfs(clean_2_GAM)
siteyears_clean_complete <- are_complete_siteyears(siteyears_clean)
sitegrows_clean <- make_growyears_dfs(siteyears_clean)
sitegrows_clean_complete <- are_complete_siteyears(sitegrows_clean)
save(siteyears_clean, siteyears_clean_complete,
     sitegrows_clean, sitegrows_clean_complete, 
     file=args$outfile)
