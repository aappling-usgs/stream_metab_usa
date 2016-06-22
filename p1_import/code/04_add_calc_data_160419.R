# site list has been updated by Jordan
# updated meta_basic with stage_meta_basic, post_meta
# confirmed that meta_dvqcoefs is already/still as up to date as it can be without more data from Jud

library(mda.streams)
library(dplyr)
SBUSER=x
SBPASS=y
done_after=Sys.time()-as.difftime(3,units='days')
done_version='rds'
mda.streams:::build_calc_ts_needs()
trycalcts <- function(varsrc) try_calc_ts(varsrc, done_after=done_after, done_version=done_version, sb_user=SBUSER, sb_pass=SBPASS)

trycalcts('dosat_calcGGbts') # 651/655 complete. the last 4 have nrow(baro) <= 1: nwis_11455142, nwis_323733107011002, nwis_324007107095501, nwis_324955107180902
trycalcts('baro_calcElev') # 545/545 complete
trycalcts('dosat_calcGGbconst') # 514/514 complete
trycalcts('dopsat_calcObsSat') # 645/647 complete. nwis_06601200, nwis_06807000 remain, have 0 non-NA rows
trycalcts('depth_calcDischRaymond') # 429/429 complete
trycalcts('depth_calcDischHarvey') # 266/266 complete
trycalcts('veloc_calcDischRaymond') # 429/429 complete
trycalcts('veloc_calcDischHarvey') # 266/266 complete
trycalcts('sitetime_calcLon') # 687/687 complete
trycalcts('suntime_calcLon') # 687/687 complete
trycalcts('par_calcLat') # 687/687 complete
trycalcts('par_calcSw') # 706/706 complete
trycalcts('sitedate_calcLon') # 687/687 complete
trycalcts('doamp_calcDAmp') # 643/645 complete. other 2 have < 1 day of data
trycalcts('dischdaily_calcDMean') # 416/422 complete. the last 6 have 0 rows of daily data
trycalcts('velocdaily_calcDMean')

# how is a run doing?
summarize_ts_files('doamp_calcDAmp') %>% 
  filter(upload_date > as.POSIXct('2016-04-17') & version=='rds')

# oops - stage_calc_ts allowed tsvs when rdses couldn't be found. i wonder how many tses this affected? but there's no way for me to 
# oops <- summarize_ts_files('par_calcLat') %>% 
#   filter(upload_date > as.POSIXct('2016-04-17') & version=='rds') %>% 
#   select(sb_id=ts_item, files=file_name)
# oops_gone <- mda.streams:::delete_sb_file(oops$sb_id, oops$files)

# 49 sites have disappeared from NWIS since last June. copy the tsv values to rds
doobs_tsv <- list_sites('doobs_nwis', with_ts_version='tsv')
doobs_rds <- list_sites('doobs_nwis', with_ts_version='rds')
doobs_gained <- setdiff(doobs_rds, doobs_tsv)
doobs_lost <- setdiff(doobs_tsv, doobs_rds)

library(unitted)
new_units <- get_var_src_codes(var_src=='doobs_nwis', out='units', drop=TRUE)
tmpnew <- normalizePath(file.path(tempdir(), '/newtsv'))
dir.create(tmpnew)
# INVALID TIMESTEPS (CLEAR DST LAG IN NOV, USUALLY 7AM)
# 18 is invalid nwis_07049691
# 19 is invalid nwis_07054501
# 20 is invalid nwis_07054502
# 21 is invalid nwis_07054527
# 22 is invalid nwis_07056515
# 23 is invalid nwis_07060000
# 24 is invalid nwis_07076000
# 25 is invalid nwis_07261090
# 26 is invalid nwis_08068275
# 27 is invalid nwis_08068400
# 28 is invalid nwis_09258050
# 29 is invalid nwis_09258980
# 38 is invalid nwis_14152000
# 40 is invalid nwis_295827090052800
# 41 is invalid nwis_295906090054200
# 42 is invalid nwis_300009090051600
# 43 successfully posted but might just lack novembers to have obvious timezone problems?
# 44 is invalid nwis_300034090055300
# 45 is invalid nwis_300127090045800
# 46 is invalid nwis_300128090045800

sbtools::authenticate_sb(SBUSER, SBPASS)
for(i in 41:length(doobs_lost)) {
  ts <- get_ts('doobs_nwis', doobs_lost[i], version='tsv', on_local_exists='skip') %>%
    v() %>%
    filter(!is.na(doobs)) %>%
    u(c(NA, new_units))
  tsf <- write_ts(ts, site=doobs_lost[i], var='doobs',src='nwis', folder=tmpnew, version='rds')
  #ts2 <- read_ts(tsf)
  post_ts(tsf)
}
