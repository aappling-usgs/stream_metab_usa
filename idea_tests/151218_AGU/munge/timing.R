# Data: timing data.frame with timing metrics & metadata, one row per site (averaged over years)

if((manual=FALSE)) {
  args <- list(outfile='cache/timing.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('munge/timing_lib.R')
load('cache/siteyears_GAM.RData')

# calculate & merge
timing <- describe_sites() %>% select(-lat, -lon) %>%
  left_join(unitted::v(get_meta()), by=c(site='site_name'))

save(timing, file=args$outfile)