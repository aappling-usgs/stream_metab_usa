library(mda.streams)
library(dplyr)
library(unitted)
library(streamMetabolizer)
sbtools::authenticate_sb(SBUSER, SBPASS)

#### all ####

sites <- get_meta('manual') %>% v() %>% filter(manual.assessment %in% c('accept','examine')) %>% .[['site_name']]
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

sapply(runs$run_path, function(f) if(!file.exists(f)) dir.create(f))

#### PRK ####

# already done - 0.0.13 should be fine

#### K ####

cfg_K <- 
  stage_metab_config(
    tag=runs$tag[2], strategy=runs$strategy[2], 
    model = "metab_Kmodel",
    model_args = "list(method='lm', weights='CI', predictors='velocity.daily', transforms=c(K600='log',velocity.daily='log'))",
    site = sites, 
    
    sitetime = choose_data_source("sitetime", sites, logic = "priority local"),
    veloc = choose_data_source("veloc", sites, logic = "priority local"),
    sitedate = choose_data_source("sitedate", sites, logic = "priority local"),
    K600 = choose_data_source("K600", sites, type="pred", src=runs$tag[1], logic = "smooth K"),
    K600lwr = choose_data_source("K600lwr", sites, type="pred", src=runs$tag[1], logic = "smooth K"),
    K600upr = choose_data_source("K600upr", sites, type="pred", src=runs$tag[1], logic = "smooth K"),
    
    doobs = choose_data_source("doobs", sites, logic = "unused var"),
    dosat = choose_data_source("dosat", sites, logic = "unused var"),
    depth = choose_data_source("depth", sites, logic = "unused var"), 
    wtr = choose_data_source("wtr", sites, logic = "unused var"), 
    par = choose_data_source("par", sites, logic = "unused var"),
    
    start_date = NA, end_date = NA, omit_incomplete = TRUE,
    filename = runs$config_path[2])
#mda.streams:::delete_metab_run(runs$run_title[2])
post_metab_run(folder=runs$run_path[2], files=runs$config_file[2])
cfg <- get_config(runs$run_title[2], runs$config_file[2], on_local_exists = 'replace')

#### PR ####

# reload cfg 1 (PRK)
cfgprk <- get_config(runs$run_title[1], runs$config_file[1], on_local_exists = 'replace')

# leave out the 3 rows referring to models that failed at step 2 (K)
mms <- grep(runs$tag[2], list_metab_models(), fixed=T, value=T)
run2_goodrows <- as.numeric(unname(parse_metab_model_name(mms, out='row')))
cfgprk <- cfgprk[run2_goodrows, ]
sites <- cfgprk$site

# get new cols
cfg_PR <- stage_metab_config(
  tag=runs$tag[3], strategy=runs$strategy[3], 
  model = "metab_mle",
  model_args = "list()", 
  site = sites, 
  sitetime = choose_data_source("sitetime", sites),
  doobs = choose_data_source("doobs", sites),
  dosat = choose_data_source("dosat", sites),
  depth = choose_data_source("depth", sites), 
  wtr = choose_data_source("wtr", sites), 
  par = choose_data_source("par", sites),
  sitedate = choose_data_source("sitedate", sites, logic = "priority local"),
  K600 = choose_data_source("K600", sites, type="pred", src=runs$tag[2], logic = "use smoothed K"),
  #K600lwr = choose_data_source("K600lwr", sites, type="pred", src=runs$tag[2], logic = "use smoothed K"),
  #K600upr = choose_data_source("K600upr", sites, type="pred", src=runs$tag[2], logic = "use smoothed K"),
  start_date = NA, end_date = NA, omit_incomplete = TRUE,
  filename = runs$config_path[3])
#mda.streams:::delete_metab_run(runs$run_title[3])
post_metab_run(folder=runs$run_path[3], files=runs$config_file[3])
# repair cfg for sites that got mis-posted in the K_smooth run
cfg <- get_config(runs$run_title[3], runs$config_file[3], on_local_exists = 'replace')
cfg2 <- get_config(runs$run_title[2], runs$config_file[2], on_local_exists = 'replace')
setdiff(cfg2$site, cfg$site) # of these, nwis_01475530, nwis_07056515 were recovered. the other three don't run
sites_to_add <- c('nwis_01475530', 'nwis_07056515')
cfg3b <- stage_metab_config(
  tag=runs$tag[3], strategy=runs$strategy[3], 
  model = "metab_mle",
  model_args = "list()", 
  site = sites_to_add, 
  sitetime = choose_data_source("sitetime", sites_to_add),
  doobs = choose_data_source("doobs", sites_to_add),
  dosat = choose_data_source("dosat", sites_to_add),
  depth = choose_data_source("depth", sites_to_add), 
  wtr = choose_data_source("wtr", sites_to_add), 
  par = choose_data_source("par", sites_to_add),
  sitedate = choose_data_source("sitedate", sites_to_add, logic = "priority local"),
  K600 = choose_data_source("K600", sites_to_add, type="pred", src=runs$tag[2], logic = "use smoothed K"),
  start_date = NA, end_date = NA, omit_incomplete = TRUE,
  filename = NULL)
cfg$config.row <- as.numeric(cfg$config.row)
cfg3b$config.row <- nrow(cfg) + c(1,2)
cfg3final <- bind_rows(cfg, cfg3b)
write_config(cfg3final, runs$config_path[3])
#mda.streams:::delete_metab_run(runs$run_title[3])
post_metab_run(folder=runs$run_path[3], files=runs$config_file[3])

#still_title <- do.call(make_metab_model_name, cfg[still_need,c('date','tag','strategy','site','config.row')] %>% mutate(title=make_metab_run_title(date=format(as.Date(date),"%y%m%d"), tag=tag, strategy=strategy)) %>% select(title, row=config.row, site=site) %>% as.list)
#repair_metab_model(still_title)
