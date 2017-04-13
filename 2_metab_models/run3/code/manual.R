# manually post those models that ran but couldn't be posted because sciencebase was down
library(mda.streams)
login_sb()

unposted <- dir('../2_metab_models/run3/cluster/condor/results_170406_unposted/', full.names = TRUE)

post_metab_model(unposted, verbose=TRUE, on_exists = 'skip')
up_mmnames <- parse_metab_model_path(unposted, out='model_name', use_names = FALSE)
untagged <- names(which(is.na(setNames(locate_metab_model(up_mmnames), up_mmnames))))
repair_metab_model(untagged)

# summarize the files that got saved locally becuase they weren't initially posted (save the download)
source('../2_metab_models/run3/code/summarize_model.R')
on_sb <- list_metab_models('1.0.2')
posted <- sapply(unposted, function(up) {
  mmname <- parse_metab_model_path(up, out='model_name')
  if(any(grepl(mmname, on_sb))) {
    varname <- load(up)
    mm <- get(varname)
    summarize_model(mm, mmname, '../2_metab_models/run3/out/summaries/')
    file.remove(up)
    return(mmname)
  } else {
    return(c())
  }
})


# download and summarize models
library(mda.streams)
library(dplyr)
login_sb()
mms <- list_metab_models('1.0.2')
mms_parsed <- bind_cols(parse_metab_model_name(mms), data_frame(model_name=mms))

summarized <- c(
  dir('../2_metab_models/run3/out/summaries1', pattern='*.csv'),
  dir('../2_metab_models/run3/out/summaries', pattern='*.csv')) %>% 
  strsplit('-') %>% sapply(`[`, 2) %>% as.numeric
needs_summary <- filter(mms_parsed, !(row %in% summarized))

source('../2_metab_models/run3/code/summarize_model.R')
for(mmname in needs_summary$model_name) {
  message(mmname, ' (', Sys.time(), ')')
  mm <- get_metab_model(mmname, version='original', update_sb = FALSE)
  summarize_model(mm, mmname, '../2_metab_models/run3/out/summaries2/')
  file.remove(dir(tempdir(), pattern='mm_.*.RData', full.names=TRUE))
  file.remove(dir(tempdir(), pattern='nwis.*.rds', full.names=TRUE))
}
