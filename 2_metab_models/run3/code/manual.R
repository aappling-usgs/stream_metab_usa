# manually post those models that ran but couldn't be posted because sciencebase was down
library(mda.streams)
login_sb()

local <- dir('../2_metab_models/run3/cluster/condor/results_170420/', full.names = TRUE)
loc_mmnames <- parse_metab_model_path(local, out='model_name', use_names = FALSE)
on_sb <- list_metab_models('1.0.2')
unposted <- sapply(setdiff(loc_mmnames, on_sb), grep, local, value=TRUE)

post_metab_model(unposted, verbose=TRUE, on_exists = 'skip')
up_mmnames <- parse_metab_model_path(unposted, out='model_name', use_names = FALSE)
untagged <- names(which(is.na(setNames(locate_metab_model(up_mmnames), up_mmnames))))
tagtotry <- untagged
tagtries <- lapply(tagtotry, function(ut) {
  message("trying to repair ", ut)
  tryCatch(
    repair_metab_model(ut),
    error=function(e) e)
})
tagtotry <- tagtotry[sapply(tagtries, function(tt) !isTRUE(unname(tt)) & length(tt) > 0 & !is.na(tt))]
cat(sprintf("https://www.sciencebase.gov/catalog/items?q=%s", tagtotry), sep='\n')

# summarize the files that got saved locally becuase they weren't initially posted (save the download)
source('../2_metab_models/run3/code/summarize_model.R')
on_sb <- list_metab_models('1.0.2')
posted <- sapply(local, function(up) {
  mmname <- parse_metab_model_path(up, out='model_name')
  message(mmname)
  # if(any(grepl(mmname, on_sb))) {
    varname <- load(up)
    mm <- get(varname)
    summarize_model(mm, mmname, '../2_metab_models/run3/out/summaries/')
    tsfiles <- stage_metab_ts(mm, folder='../2_metab_models/run3/out/tses/')
    sapply(tsfiles, function(tsfile) {
      
    })
    # file.remove(up)
    return(mmname)
  # } else {
    # return(c())
  # }
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


# figure out which models didn't actually get posted
mms_query <- sbtools::query_item_in_folder(text='1.0.2', folder=locate_folder('metab_models'), limit=10000) 
mms_num_files <- sapply(mms_query, function(mmd) length(mmd$files))
empty_items <- sapply(mms_query[mms_num_files == 0], function(mmd) mmd$title)
mda.streams:::delete_metab_model(empty_items)
mms_posted_after <- sapply(mms_query, function(mmd) max(sapply(mmd$files, function(f) f$dateUploaded))) > as.POSIXct(posted_after, tz='UTC')


# document which models are in progress
source('../lib/write_status_table.R')
ongoing <- 1 + c(10,100,110,119,24,30,32,34,47,51,59,87,9,179,193,195,232,234)
stat <- read_status_table('../2_metab_models/run3/out/files_metab.tsv') %>%
  mutate(model_name = parse_metab_model_path(filepath, out='model_name')) %>%
  mutate(row = parse_metab_model_name(model_name, out='row')) %>%
  filter(row %in% ongoing)
cat(paste0('  - ', stat$model_name), sep='\n')

remake::delete('metab.run3.condor.prep', remake_file='2_metab.yml')
remake_smu('metab.run3.condor.prep', '2_metab.yml')


# create a metab_run item on sciencebase; archive the config and package bundle
run_title <- parse_metab_model_path(local[1], out='title')
run_dir <- file.path('../2_metab_models/run3/out/', run_title)
dir.create(run_dir)
file.copy('../2_metab_models/run3/cluster/condor/bundle.zip', run_dir)
file.copy('../2_metab_models/run3/out/config.tsv', run_dir)
mda.streams::post_metab_run(folder=run_dir, files=dir(run_dir), on_exists = 'addfiles')


# update bob and maite's spreadsheet

bob_maite <- read.csv('../2_metab_models/run2/out/expert_file.csv', header=TRUE, stringsAsFactors=FALSE)
cfg3 <- read_config('../2_metab_models/run3/out/config.tsv') %>% 
  mutate(resolution=substring(strategy, 7), 
         model_name_run3=make_metab_model_name(make_metab_run_title(format(as.POSIXct(date),'%y%m%d'), tag, strategy), config.row, site))
bm3 <- left_join(bob_maite, select(cfg3, site, resolution, model_name_run3), by=c('site','resolution')) %>%
  select(site, model_name_run2=model_name, model_name_run3, everything())
write.csv(bm3, '../2_metab_models/run3/out/expert_file.csv', row.names=FALSE)
