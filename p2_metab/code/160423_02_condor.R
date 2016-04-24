# start with condor/local_master/local.R. then come here for the Do Science part.
clusterCall(c1, function() {library(mda.streams)})
clusterCall(c1, function() {library(streamMetabolizer)})
sbtools::authenticate_sb(SBUSER, SBPASS)

# info copied from 01_configs.R
runs <- 
  data_frame(
    tag = c('0.1.0','0.1.1','0.1.2'),
    strategy = c('PRK_initial', 'K_smooth', 'PR_fixed_K'),
    date = rep('160423', 3),
    out_path = 'p2_metab/out/') %>%
  mutate(
    run_title = make_metab_run_title(date, tag, strategy),
    run_path = paste0(out_path, run_title),
    config_file = paste0('config_',c('1_PRK','2_K','3_PR'),'.tsv'),
    config_path = paste0(run_path, "/", config_file))

#### PRK ####

cfg <- get_config(runs$run_title[1], config_file=runs$config_file[1], on_local_exists = 'replace')
clusterfun <- config_to_metab_clusterfun(
  config=cfg, return_value="file_name", 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user=SBUSER, sb_password=SBPASS, 
  verbose=TRUE)
metab_out1 <- clusterApplyLB(c1, 1, clusterfun)
metab_out2 <- clusterApplyLB(c1, 2:nrow(cfg), clusterfun)
# mtb <- config_to_metab(cfg, 272) # debugging: dosat ts wasn't properly tagged
metab_out3 <- clusterApplyLB(c1, 272, clusterfun)
# repairs <- repair_metab_model(paste0(sites,"-",1:length(sites),"-160423 0.1.0 PRK_initial")) # length(which(repairs)) was 179!

#### K ####

# oops - config_preds_to_data_column needed debugging
clusterExport(c1, 'reinstall_github')
clusterCall(c1, function() {reinstall_github('mda.streams', 'aappling-usgs', 'master')})
reinstall_github('mda.streams','aappling-usgs','master')
clusterCall(c1, function() {reinstall_github('github', 'streamMetabolizer', 'USGS-R', 'master')})
reinstall_github('streamMetabolizer','USGS-R','master')

cfg <- get_config(runs$run_title[2], config_file=runs$config_file[2], on_local_exists = 'replace')
clusterfun <- config_to_metab_clusterfun(
  config=cfg, return_value="file_name", 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user=SBUSER, sb_password=SBPASS, 
  verbose=TRUE)
test0 <- config_to_metab(cfg, 1, prep_only=F)
metab_out <- clusterApplyLB(c1, 336, clusterfun)
metab_out <- clusterApplyLB(c1, 2:nrow(cfg), clusterfun)
mms <- list_metab_models(runs$tag[2])
parsed <- parse_metab_model_name(mms)
done_sites <- parsed$site
undone_sites <- setdiff(cfg$site, done_sites)
undone_rows <- match(undone_sites, cfg$site)

# retry models that didn't run successfully
sbtools::authenticate_sb(SBUSER, SBPASS)
metab_out <- clusterApplyLB(c1, undone_rows, clusterfun)

# repair models that didn't get tagged
tagged_models <- list_metab_models()
dir_models <- list_metab_models('0.1.1')
repair_metab_model(setdiff(dir_models, tagged_models))

lmms <- list_metab_models(runs$tag[2])
still_need <- setdiff(1:nrow(cfg), lmms$row)
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[still_need[4:5]], clusterfun)
still_title <- do.call(make_metab_model_name, cfg[still_need,c('date','tag','strategy','site','config.row')] %>% mutate(title=make_metab_run_title(date=format(as.Date(date),"%y%m%d"), tag=tag, strategy=strategy)) %>% select(title, row=config.row, site=site) %>% as.list)
repair_metab_model(still_title)

#### PR ####

cfg <- get_config(runs$run_title[3], config_file=runs$config_file[3], on_local_exists = 'replace')
clusterfun <- config_to_metab_clusterfun(
  config=cfg, return_value="file_name", 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user=SBUSER, sb_password=SBPASS, 
  verbose=TRUE)
metab_out <- clusterApplyLB(c1, 1, clusterfun)
metab_out <- clusterApplyLB(c1, 2:nrow(cfg), clusterfun)
# list_metab_models(runs$tag[3])

# repair models that didn't get tagged
tagged_models <- list_metab_models()
dir_models <- list_metab_models('0.1.2')
setdiff(dir_models, tagged_models)
repair_metab_model(setdiff(dir_models, tagged_models))

# redo models that failed
parsed <- parse_metab_model_name(dir_models)
done_sites <- parsed$site
undone_sites <- setdiff(cfg$site, done_sites)
undone_rows <- match(undone_sites, cfg$site)
metab_out <- clusterApplyLB(c1, undone_rows, clusterfun)


