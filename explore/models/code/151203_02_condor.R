# start with condor/on_machine.R. then come here for the Do Science part.
clusterCall(c1, function() {library(mda.streams)})
clusterCall(c1, function() {library(streamMetabolizer)})
sbtools::authenticate_sb(SBUSER, SBPASS)

# info copied from 01_configs.R
runs <- 
  data_frame(
    tag = c('0.0.13','0.0.16','0.0.17'),
    strategy = c('PRK_initial', 'K_smooth', 'PR_fixed_K'),
    date = c('151125','151203','151203'),
    out_path = 'p2_metab/out/') %>%
  mutate(
    run_title = make_metab_run_title(date, tag, strategy),
    run_path = paste0(out_path, run_title),
    config_file = paste0('config_',c('1_PRK','2_K','3_PR'),'.tsv'),
    config_path = paste0(run_path, "/", config_file))

#### PRK ####

# NA

#### K ####

cfg <- get_config(runs$run_title[2], config_file=runs$config_file[2], on_local_exists = 'replace')
clusterfun <- config_to_metab_clusterfun(
  config=cfg, return_value="file_name", 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user=SBUSER, sb_password=SBPASS, 
  verbose=TRUE)
test0 <- config_to_metab(cfg, 88, prep_only=F)
test1 <- clusterfun(88)
metab_out <- clusterApplyLB(c1, 244, clusterfun)
# list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[2])
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-c(244,88)], clusterfun)
lmms <- list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[2])
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
test0 <- config_to_metab(cfg, 88, prep_only=T)
test1 <- clusterfun(88)
metab_out <- clusterApplyLB(c1, 242, clusterfun)
# list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[3])
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-c(242,88)], clusterfun)
sbtools::authenticate_sb(SBUSER, SBPASS)
lmms <- list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[3])
mms3 <- grep("0.0.17", list_metab_models(), fixed=T, value=T)
donerows <- as.numeric(unname(parse_metab_model_name(mms3, out="row")))
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-donerows], clusterfun)
metab_out <- clusterApplyLB(c1, c(287,288), clusterfun)
