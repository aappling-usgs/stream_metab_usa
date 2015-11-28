library(mda.streams)
tag='0.0.2'
strategy='local_makefile_run'
verbose=TRUE
out_dir <- paste0("p2_metab/out/", format(Sys.Date(), "%y%m%d"), " ", tag, " ", strategy)
if(!dir.exists(out_dir)) dir.create(out_dir)
config_path <- file.path(out_dir, "condor_config.tsv")
if(verbose) message("output will be saved to: ", out_dir)
config_file <- config_path
config <- read.table(config_path, sep="\t", header=TRUE, colClasses="character")
if(verbose) message("running metabolism models for ", nrow(config), " config rows")
#all_out <- lapply(1, function(row_id) {
if(verbose) message(Sys.time())
row_id=1
metab_out <- config_to_metab(config, row_id, verbose=TRUE)[[1]]
metab_out
save(metab_out, file=file.path(out_dir, sprintf("metab_out_%03d.RData", row_id)))