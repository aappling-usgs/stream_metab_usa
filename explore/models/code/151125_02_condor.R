# start with condor/on_machine.R. then come here for the Do Science part.
clusterCall(c1, function() {library(mda.streams)})
clusterCall(c1, function() {library(streamMetabolizer)})
sbtools::authenticate_sb(SBUSER, SBPASS)

# info copied from 01_configs.R
runs <- 
  data_frame(
    tag = c('0.0.13','0.0.14','0.0.15'),
    strategy = c('PRK_initial', 'K_smooth', 'PR_fixed_K'),
    date = '151125',
    out_path = 'idea_tests/151218_AGU/out/') %>%
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
# shortsites <- get_meta('metabinput') %>% v() %>% filter(manual.assessment %in% c('accept','examine')) %>% 
#   arrange(metabinput.num_complete) %>% select(site_name, metabinput.num_complete) %>% head
# cfg %>% filter(site=="nwis_01567000") %>% select(site, config.row) # 244 has 655 rows
# cfg %>% filter(site=="nwis_06601200") %>% select(site, config.row) # 166 has 2445 rows
# cfg %>% filter(site=="nwis_03098600") %>% select(site, config.row) # 88 has 2468 rows
test0 <- config_to_metab(cfg, 88)
test1 <- clusterfun(88)
metab_out <- clusterApplyLB(c1, 244, clusterfun)
# list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[1])
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-c(244,88)], clusterfun)


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
# nope, these 3 lack the data for a model: "0 (non-NA) cases". cfg[c(63, 128, 282),]

#### PR ####

cfg <- get_config(runs$run_title[3], config_file=runs$config_file[3], on_local_exists = 'replace')
clusterfun <- config_to_metab_clusterfun(
  config=cfg, return_value="file_name", 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user=SBUSER, sb_password=SBPASS, 
  verbose=TRUE)
test0 <- config_to_metab(cfg, 88, prep_only=F)
test1 <- clusterfun(88)
metab_out <- clusterApplyLB(c1, 242, clusterfun)
# list_metab_models() %>% parse_metab_model_name() %>% filter(tag==runs$tag[3])
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-c(242,88)], clusterfun)
sbtools::authenticate_sb(SBUSER, SBPASS)
mms3 <- grep("0.0.15", list_metab_models(), fixed=T, value=T)
donerows <- as.numeric(unname(parse_metab_model_name(mms3, out="row")))
metab_out <- clusterApplyLB(c1, (1:nrow(cfg))[-donerows], clusterfun)
