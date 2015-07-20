## Use this script to manually start and manage a Condor cluster.

#### Connect ####

#' 1. SSH into HTCondor M000 (130.11.177.99), probably using PuTTY.
#' 
#' 2. If needed, copy files over to the user/condor_R_snow folder on the node,
#' probably using FileZilla. Key files for this script are condor.sub, which
#' calls simple.sh and passes .Renviron to child nodes.

library(parallel)
library(tidyr)
library(dplyr)
library(mda.streams)

# Set our choice between a local or Condor run
cluster <- c("localhost", "condor")[2]

# Start a cluster
switch(
  cluster,
  "condor" = {
    # Start a cluster, wait for them to connect
    c1 = makePSOCKcluster(paste0('machine', 1:50), manual=TRUE, port=4043)
    #' At this point go to putty, cd into condor_R_snow.
    #' 
    #' 3. Edit simple.sh to contain MASTER=YOUR.LOCAL.IP.ADDRESS
    #' 
    #' 4. Type condor_submit condor.sub. This will yield the text:
    #' 
    #' Submitting job(s).......................................................... 
    #' 60 job(s) submitted to cluster XXXX.
    #' 
    #' 5. Query for updates using condor_q. We're looking for jobs to appear in that
    #' list with the cluster ID declared above (whatever number replaces XXXX).
    #' 
    #' 6. We're also looking to see them show up in the local console as replies to 
    #' the makePSOCKcluster call. If the local console appears to have stalled while
    #' condor_q suggests all jobs have been dispatched, you can run condor_submit
    #' condor.sub again to add more worker nodes. Any nodes that don't find a master
    #' just disappear.
    #' 
    #' 7. When the makePSOCKcluster call has finished, we're ready to make 
    #' clusterCalls.
  } ,
  "localhost" = {
    # Alternatively, start a local cluster. If installs are needed, start with a 
    # 1-node cluster to run clusterCall(c1, function(){ install_check(...) }))
    c1 = makePSOCKcluster(rep('localhost', 1))
  })


#### Test ####
hello_world <- clusterCall(c1, function(){ 
  message("anonymous message in a log file")
  return(list(hi="hello world!", session=sessionInfo()))
})
hello_jobs <- clusterApplyLB(c1, 1:77, function(job_id){ 
  message("message from job ", job_id, " in a log file")
  return(paste0("hello world from job ", job_id))
})

#### Configure ####

# Define utility fun and send it to all nodes
install_check <- function(repo, pkg, ghuser) {
  # Install
  run_install <- function() {
    capture.output(
      switch(repo,
             'cran'={ install.packages(pkg, repos='http://cran.rstudio.com') },
             'gran'={ install.packages(pkg, repos='http://owi.usgs.gov/R') },
             'github'={ install_github(paste0(ghuser, "/", pkg)) }))
  }
  out <- tryCatch(
    { run_install() }, 
    error=function(e) { list(output=NA, warnings=NA, errors=e) }, 
    warning=function(w) { list(output=run_install(), warnings=w, errors=NA) })
  
  # Report whether it installed
  itworked <- (pkg %in% unname(installed.packages()[,'Package']))
  return(list(itworked, out))
}
clusterExport(c1, 'install_check')

# Define local utilities for looking at results
all_first_out <- function(out) {
  all(sapply(out, function(o) o[[1]]))
}
view_install_out <- function(out) {
  lapply(out, function(o) { cat(o[[2]], '\n') })
  invisible()
}

pkg_needs <- c('dataRetrieval', 'devtools', 'dplyr', 'geoknife', 'httr', 'jsonlite', 
               'LakeMetabolizer', 'lazyeval', 'lubridate', 'mda.streams', 'methods', 'parallel', 'RCurl', 
               'runjags', 'sbtools', 'streamMetabolizer', 'stringr', 'unitted', 'XML') #'rjags', 

view_installed_packages <- function() {
  inst <- clusterCall(c1, function() { unname(installed.packages()[,"Package"]) })
  pkg_needs=pkg_needs
  suppressWarnings(
    as.data.frame(inst) %>%
      setNames(paste0('n',1:length(c1))) %>%
      gather(key=node, value=pkg, 1:length(c1)) %>%
      filter(pkg %in% pkg_needs) %>%
      mutate(has=TRUE) %>%
      full_join(data.frame(pkg=pkg_needs, node=factor("none"), has=FALSE), by=c("node","pkg","has")) %>%
      spread(key=node, value=has) %>%
      select(-none))
}

# Now actually install packages, checking for completion each time. already installed: methods, parallel
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'devtools') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'dplyr') })) # also installs lazyeval
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'httr') })) # also installs jsonlite, stringr
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'LakeMetabolizer') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'lubridate') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'RCurl') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'reshape2') }))
#all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'rjags') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'runjags') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'XML') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('gran', 'dataRetrieval') }))
clusterCall(c1, function() { library(devtools) })
all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'unitted', 'appling') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'geoknife', 'USGS-R') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'sbtools', 'aappling-usgs') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'streamMetabolizer', 'aappling-usgs') }))
all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'mda.streams', 'aappling-usgs') }))

# DIAGNOSTICS
# for diagnosing the single most recent install run:
out
view_install_out(out)
# there should be a JAGS folder listed here for every node
clusterCall(c1, function() { dir("/usr/local/lib") })

# for seeing overall install status
view_installed_packages()#[,1:6]


if(cluster=="localhost") {
  # stop the 1-node cluster and start a multi-node cluster
  stopCluster(c1)
  # My machine has '8' nodes, which probably means 4 hyperthreaded nodes, so 6
  # may be a maximal use of those resources.
  c1 = makePSOCKcluster(rep('localhost', 6)) # for running models
}

# load packages
clusterCall(c1, function() { library(mda.streams) })


#### Start Science ####

library(mda.streams)
# identify today's folder to hold results
tag="0.0.2"
strategy="retry first full run"
date=as.Date("2015-07-13")
out_dir <- paste0("p2_metab/out/", format(date, "%y%m%d"), " ", tag, " ", strategy)
if(!dir.exists(out_dir)) dir.create(out_dir)


#### Data Inventory ####

# inventory data
sites <- list_sites()
clusterExport(c1, 'sites')
site_ids <- 1:length(sites)
run_site_inventory <- function(site_id, sleep=runif(1, min=0, max=10)) {
  
  # sleep to avoid sending many similar requests all at once
  Sys.sleep(sleep)
  
  # model metabolism & return results or error
  tryCatch({
    summarize_ts(list_datasets(sites[site_id]), sites[site_id])
  }, 
  error=function(e){e})
  
}
site_inventory <- clusterApplyLB(c1, site_ids, run_site_inventory) %>% # LB = load balance
  setNames(sites)
save(site_inventory, file=file.path(out_dir, "site_inventory.RData"))

si <- bind_rows(site_inventory[which(sapply(site_inventory, function(i1) isTRUE(is.data.frame(i1))))])
write.table(si, file=file.path(out_dir, "site_inventory.tsv"), sep="\t", row.names=FALSE)

# explore site inventory
si <- read.table(file.path(out_dir, "site_inventory_4.tsv"), sep="\t", header=TRUE, stringsAsFactors = FALSE) %>% tbl_df
si %>% summarize(num_tbls=length(site), num_dates=sum(num_dates), num_rows=sum(num_rows))
si %>% mutate(src_db=parse_var_src(as.character(var_src), out="src"))
si %>% mutate(src_type=sapply(var_src, function(vs) get_var_src_codes(var_src==vs, out="src_type")),
              src_db=parse_var_src(as.character(var_src), out="src"),
              src_cat=ifelse(src_type=="data", src_db, src_type)) %>% 
  group_by(src_cat) %>% summarize(num_tbls=length(site), num_dates=sum(num_dates), num_rows=sum(num_rows))
si %>% group_by(var_src) %>% summarize(num_tbls=length(site), num_dates=sum(num_dates), num_rows=sum(num_rows)) %>%
  mutate(src_type=sapply(var_src, function(vs) get_var_src_codes(var_src==vs, out="src_type")),
         src_db=parse_var_src(as.character(var_src), out="src"),
         src_cat=ifelse(src_type=="data", src_db, src_type)) %>% arrange(src_cat)
# ts files with all NAs
si %>% filter(num_complete < 50)

# inventory SB items. (in the chunk below, on 7/9/15 item 259 failed with 'Error
# in curl::curl_fetch_memory(url, handle = handle): Timeout was reached' the
# first time through. but then i reran it and it was fine.)
tses <- levels(si$var_src)
sites <- list_sites()
item_inventory <- lapply(sites, function(site) {
  cat(".")
  tryCatch(
    data_frame(
      site=site,
      ts=tses, 
      by_tag=locate_ts(tses, site, by="tag") %>% replace(is.na(.), "na"),
      by_dir=locate_ts(tses, site, by="dir") %>% replace(is.na(.), "na"),
      by_kid=tses %in% list_datasets(site)) %>%
      mutate(all_agree=by_tag==by_dir & by_kid==(by_tag!="na"))
    , error=function(e) e)
}); cat("\n")
save(item_inventory, file=file.path(out_dir, "item_inventory.RData"))
ii <- bind_rows(item_inventory)
write.table(ii, file=file.path(out_dir, "item_inventory.tsv"), sep="\t", row.names=FALSE)

ii <- read.table(file.path(out_dir, "item_inventory.tsv"), sep="\t", header=TRUE, stringsAsFactors = FALSE) %>% tbl_df
# ts items without tags
ii %>% filter(!all_agree)
# ts items that didn't download
si_by_either <- lapply(1:nrow(si), function(sirow) { 
  tryCatch(locate_ts(si[[sirow,'var_src']], si[[sirow,'site']], by="either"), error=function(e) e)
})
si_by_either[which(!sapply(si_by_either, is.character))]
sirow=3414; si_by_either[[sirow]] <- tryCatch(locate_ts(si[[sirow,'var_src']], si[[sirow,'site']], by="either"), error=function(e) e)
si_by_either_vec <- unlist(si_by_either)
si$by_either <- si_by_either_vec
write.table(si, file=file.path(out_dir, "site_inventory_plus.tsv"), sep="\t", row.names=FALSE)
# something went wrong in download
(ts_problems <- setdiff(ii$by_dir, si$by_either))
# ts items without files
ts_prob_file_count <- sapply(ts_problems[-1], function(tp) {
  tryCatch(nrow(sbtools::item_list_files(tp)), error=function(e) as.character(e))
})
names(ts_prob_file_count[-1][as.numeric(ts_prob_file_count[-1]) < 1])
# ts items with multiple files
names(ts_prob_file_count[-1][as.numeric(ts_prob_file_count[-1]) > 1])
# site items without tags
site_item_inventory <- data_frame(
  site=sites, 
  by_tag=locate_site(sites, by="tag") %>% replace(is.na(.), "na"), 
  by_dir=locate_site(sites, by="dir") %>% replace(is.na(.), "na")) 
site_item_inventory %>%
  filter(by_tag!=by_dir | is.na(by_tag))



# to unload and reload a package: detach(package:mda.streams, unload=TRUE)


#### Metabolism ####

# stage & load the config file, with code similar to that in
# p2_metab/code/01_model_metab.R
config_path <- file.path(out_dir, "condor_config.tsv")

# stage config. no sense trying for those sites we know won't have what we need,
# so get a short list using list_sites()
sites <- list_sites(list(
  "sitetime_calcLon", 
  "doobs_nwis", 
  any=c("dosat_calcGGbts","dosat_calcGGbconst"),
  "depth_calcDisch",
  "wtr_nwis",
  any=c("par_nwis","par_calcLat")), 
  logic="all")
# write to file
config_file <- stage_metab_config(
  tag=tag, strategy=strategy, 
  model="metab_mle", model_args="list()",
  site=sites, filename=config_path)
# read from file
config <- read.table(config_path, sep="\t", header=TRUE, colClasses="character")
nrow(config)

# run each line of the config as a cluster job
clusterExport(c1, 'config')
run_ids = 1:nrow(config)
run_config_to_metab <- function(run_id, sleep=runif(1, min=0, max=5)) {
  
  print(run_id)
    
  # sleep to avoid sending many similar requests all at once
  Sys.sleep(sleep)
  
  # model metabolism & return results or error
  tryCatch({
    config_to_metab(config, rows=run_id)[[1]]
  }, 
  error=function(e){e})

}
#metab_out <- clusterApplyLB(c1, run_ids, run_config_to_metab) %>% # LB = load balance
#  setNames(paste0("run_", run_ids))
metab_out <- config_to_metab()
save(metab_out, file.path(out_dir, "metab_out.RData"))

library(streamMetabolizer)
metab_ests <- bind_rows(lapply(names(metab_out), function(monm) {
  mo <- metab_out[[monm]]
  if(is(mo, "metab_model")) {
    predict_metab(mo) %>%
      mutate(run_id=monm, run_dir=out_dir)
  } else {
    NULL
  }
}))

save(metab_ests, file.path(out_dir, "metab_ests.RData"))


#### End ####

stopCluster(c1)


#### Notes ####

# clusterExport(c1, 'somevar') sends somevar to every node

# to kill the cluster, in putty do condor_rm 1508 (e.g.) or (if you want log 
# files) call from R: stopCluster(c1); this will kill the nodes, and then condor
# should notice and clean itself up.