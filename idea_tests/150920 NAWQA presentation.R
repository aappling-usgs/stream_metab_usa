library(mda.streams)

tag <- "0.0.10"
date <- format(Sys.Date(), "%y%m%d")
out_path <- "idea_tests/150920_NAWQA/"
run_title <- make_metab_run_title(date, tag, "flexibility_demo")
run_path <- paste0(out_path, run_title)
config_file <- "config.tsv"
config_path <- paste0(out_path, run_title, "/", config_file)
if(!dir.exists(run_path)) dir.create(run_path, recursive=TRUE)

site <- "nwis_01388000"
cfg <- stage_metab_config(
  tag="0.0.10", strategy=c("raymond_estk_obs","harvey_estk_obs","harvey_fixk_obs","harvey_estk_proc"), model="metab_mle", 
  model_args=c(c(rep("list(model_specs=specs_mle_obserr())",3), # now specs_mle_oi
                 "list(model_specs=specs_mle_procerr())")), # now specs_mle_pi
  site=site,
  depth=choose_data_source("depth", site=site, logic="compare_raymond_harvey", type="ts", src=c("calcDischRaymond",rep("calcDischHarvey",3))),
  sitedate=choose_data_source("sitedate", site=c(NA,NA,site,NA), logic="match K600", type=c("none","none","ts","none"), src=c(NA,NA,"calcLon",NA)),
  K600=choose_data_source("K600", site=c(NA,NA,site,NA), logic="compare_est_fix_k", type=c("none","none","const","none"), src=c(NA,NA,"10,d^-1",NA)),
  start_date="2014-01-01",end_date="2015-01-01",
  filename=NULL
)
cfg[c("tag","site","strategy","depth.src","K600.src","model_args")]
write.table(cfg, file=config_path, sep="\t", row.names=FALSE)
login_sb()
post_metab_run(folder=run_path, files=c(config_file), on_exists = "addfiles")

#library(streamMetabolizer)
#dat <- config_to_data(cfg[1,],1,metab_mle,list(model_specs=specs_mle_obserr())) # now specs_mle_oi

mtb <- config_to_metab(cfg, rows=1:4)

library(ggplot2)
library(dplyr)
library(tidyr)
ggmtb <- lapply(mtb, function(mm) { predict_metab(mm) %>% select(local.date, GPP, ER, K600) %>% gather(variable, value, GPP, ER, K600) })
ggplot(ggmtb[[4]], aes(x=local.date, y=value, color=variable)) + geom_point() + theme_bw() + facet_grid(variable ~ ., scales="free_y")

mtb_staged <- stage_metab_model(title=run_title, metab_outs=mtb)
post_metab_model(mtb_staged)

library(powstreams)
explore_model()
