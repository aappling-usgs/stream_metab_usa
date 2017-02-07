# update & install packages from the local repo. use an lapply to ensure ordering of installation
pkgdeps <- rownames(available.packages(repos="file:bundle"))
installout <- lapply(
  c('memoise', # must be updated before devtools
    'devtools', 'dplyr', 'tidyr', 'ggplot2',
    'httr', 'xml2', # must be updated before dataRetrieval
    'unitted', # needed for streamMetabolizer
    'streamMetabolizer', 'mda.streams'),
  install.packages, 
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', 
  configure.args='--no-docs --no-html' # not sure this arg is working - looks like html is still getting built
)

# report on package versions
library(streamMetabolizer)
library(mda.streams)
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
