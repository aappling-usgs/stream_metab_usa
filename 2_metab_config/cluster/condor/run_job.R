# put the shared credentials where mda.streams will find them for login_sb()
pwdfile <- file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")
if(!dir.exists(dirname(pwdfile))) dir.create(dirname(pwdfile))
file.copy(basename(pwdfile), pwdfile)

# update & install packages from the local repo
pkgdeps <- rownames(available.packages(repos="file:bundle"))
update.packages(oldPkgs = pkgdeps, repos="file:bundle", ask=FALSE, type='source')
install.packages(
  c('streamMetabolizer', 'mda.streams', 'dplyr', 'tidyr', 'ggplot2', 'unitted', 'devtools'),
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs')
devtools::session_info()

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1]) + 1 # 0-indexed by HTCondor; convert to 1-indexed

# make sure there's a place for the outputs
outdir <- 'job'
if(!dir.exists(outdir)) dir.create(outdir)

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model.R')
source('run_model_job.R')
model_out <- run_model_job(job, outdir, run_fun=run_model)
