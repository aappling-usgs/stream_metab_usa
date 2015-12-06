# Stats: Counts of sites, site-days, and site-years

if((manual=FALSE)) {
  # manually define SBUSER & SBPASS
  args <- list(sb_user=SBUSER, sb_password=SBPASS, outfile='out/intro_datasize.tsv')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("sb_user", "sb_password", "outfile"))
}

library(dplyr)

load('cache/preds_3_PR.RData')
load('cache/siteyears_raw.RData')

datasize <- c(
  sites = length(unique(preds_3_PR$site)),
  sitedays = preds_3_PR %>% filter(!is.na(GPP)) %>% nrow(),
  siteyears = NA,
  siteyears_raw_complete = length(which(siteyears_raw_complete)),
  sitegrows_raw_complete = length(which(sitegrows_raw_complete)))

write.table(datasize, file=args$outfile, sep='\t', row.names=TRUE, col.names=FALSE)
#read.table('out/intro_datasize.tsv')
