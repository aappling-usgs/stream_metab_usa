library(mda.streams)
library(dplyr)
library(unitted)
library(streamMetabolizer)
sbtools::authenticate_sb(SBUSER, SBPASS)

#### all ####

sites <- get_meta('manual') %>% v() %>% filter(manual.assessment %in% c('accept','examine')) %>% .[['site_name']]
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

sapply(runs$run_path, function(f) if(!file.exists(f)) dir.create(f))

#### PRK ####

cfg_PRK <- 
  stage_metab_config(
    tag=runs$tag[1], strategy=runs$strategy[1], 
    model = "metab_mle",
    model_args = "list()", 
    site = sites, 
    sitetime = choose_data_source("sitetime", sites),
    doobs = choose_data_source("doobs", sites),
    dosat = choose_data_source("dosat", sites),
    depth = choose_data_source("depth", sites), 
    wtr = choose_data_source("wtr", sites), 
    par = choose_data_source("par", sites),
    start_date = NA, end_date = NA, omit_incomplete = TRUE,
    filename = runs$config_path[1])
#mda.streams:::delete_metab_run(runs$run_title[1])
post_metab_run(folder=runs$run_path[1], files=runs$config_file[1])
#locate_metab_run(runs$run_title[1], br=T)

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
mms <- grep("0.0.14", list_metab_models(), fixed=T, value=T)
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
cfg <- get_config(runs$run_title[3], runs$config_file[3], on_local_exists = 'replace')
