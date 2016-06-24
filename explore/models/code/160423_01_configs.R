devtools::install_github('USGS-R/sbtools@v0.18.0')
devtools::install_github('USGS-R/streamMetabolizer@512268d') #‘0.9.5.1’
devtools::install_github('USGS-R/mda.streams@f25677c') #‘0.9.6.1’
# packageVersion('dplyr') # [1] ‘0.4.3’
# packageVersion('unitted') # [1] ‘0.2.5’

library(mda.streams)
library(dplyr)
library(unitted)
library(streamMetabolizer)
sbtools::authenticate_sb(SBUSER, SBPASS)

#### all ####

sites <- list_sites(list(
  all='doobs_nwis',
  any=c('dosat_calcGGbts','dosat_calcGGbconst'),
  any=c('depth_calcDischHarvey','depth_calcDischRaymond'),
  all='wtr_nwis',
  all='par_calcLat'))
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

sapply(runs$run_path, function(f) if(!file.exists(f)) dir.create(f))

#### PRK ####

cfg_PRK <- 
  stage_metab_config(
    tag=runs$tag[1], strategy=runs$strategy[1], 
    model = "metab_mle",
    model_args = "list(specs=specs(mm_name('mle')))", 
    site = sites, 
    sitetime = choose_data_source("sitetime", sites),
    doobs = choose_data_source("doobs", sites),
    dosat = choose_data_source("dosat", sites),
    depth = choose_data_source("depth", sites), 
    wtr = choose_data_source("wtr", sites), 
    par = choose_data_source("par", sites),
    start_date = NA, end_date = NA, omit_incomplete = TRUE,
    filename = runs$config_path[1])
PRK <- read_config(cfg_PRK)
#mda.streams:::delete_metab_run(runs$run_title[1])
post_metab_run(folder=runs$run_path[1], files=runs$config_file[1])
#locate_metab_run(runs$run_title[1], br=T)

#### K ####

run1tag_grep <- gsub('\\.', '\\\\.', runs$tag[1])
cfg_K <- 
  stage_metab_config(
    tag=runs$tag[2], strategy=runs$strategy[2], 
    model = "metab_Kmodel",
    model_args = "list(specs=specs(mm_name('Kmodel', engine='lm'), weights='K600/CI', transforms=c(K600='log',discharge.daily='log')))",
    site = sites, 
    
    sitetime = choose_data_source("sitetime", sites, logic = "priority local"),
    disch = choose_data_source("disch", sites, logic = "priority local"),
    sitedate = choose_data_source("sitedate", sites, logic = "priority local"),
    K600 = choose_data_source("K600", sites, type="pred", src=run1tag_grep, logic = "smooth K"),
    K600lwr = choose_data_source("K600lwr", sites, type="pred", src=run1tag_grep, logic = "smooth K"),
    K600upr = choose_data_source("K600upr", sites, type="pred", src=run1tag_grep, logic = "smooth K"),
    
    doobs = choose_data_source("doobs", sites, logic = "unused var"),
    dosat = choose_data_source("dosat", sites, logic = "unused var"),
    depth = choose_data_source("depth", sites, logic = "unused var"), 
    wtr = choose_data_source("wtr", sites, logic = "unused var"), 
    par = choose_data_source("par", sites, logic = "unused var"),
    
    start_date = NA, end_date = NA, omit_incomplete = TRUE,
    filename = runs$config_path[2])
K <- read_config(cfg_K)
#mda.streams:::delete_metab_run(runs$run_title[2])
post_metab_run(folder=runs$run_path[2], files=runs$config_file[2], on_exists = 'addfiles')
#cfg <- get_config(runs$run_title[2], runs$config_file[2], on_local_exists = 'replace')

#### PR ####

# leave out the 16 sites whose models failed at step 2
mms_PRK <- list_metab_models(runs$tag[1])
mms_K <- list_metab_models(runs$tag[2])
sites <- parse_metab_model_name(mms_K, out='site', use_names=FALSE)

# make new config
run21tag_grep <- gsub('\\.', '\\\\.', runs$tag[2])
cfg_PR <- stage_metab_config(
  tag=runs$tag[3], strategy=runs$strategy[3], 
  model = "metab_mle",
  model_args = "list(specs=specs(mm_name('mle')))", 
  site = sites, 
  sitetime = choose_data_source("sitetime", sites),
  doobs = choose_data_source("doobs", sites),
  dosat = choose_data_source("dosat", sites),
  depth = choose_data_source("depth", sites), 
  wtr = choose_data_source("wtr", sites), 
  par = choose_data_source("par", sites),
  sitedate = choose_data_source("sitedate", sites, logic = "priority local"),
  K600 = choose_data_source("K600", sites, type="pred", src=run21tag_grep, logic = "use smoothed K"),
  start_date = NA, end_date = NA, omit_incomplete = TRUE,
  filename = runs$config_path[3])
PR <- read_config(cfg_PR)
#mda.streams:::delete_metab_run(runs$run_title[3])
post_metab_run(folder=runs$run_path[3], files=runs$config_file[3])
