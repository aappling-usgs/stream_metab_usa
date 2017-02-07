# update & install packages from the local repo
library(xml2)
library(httr)
devtools::session_info()

pkgdeps <- rownames(available.packages(repos="file:bundle"))
install.packages(
  c('memoise'), # these must be updated before attempting to install devtools
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', configure.args='--no-docs --no-html')
install.packages(
  c('httr', 'xml2', # these must be updated before attempting ot install dataRetrieval
    'devtools', 'dplyr', 'tidyr', 'ggplot2', 'unitted'), # these might be nice to have before installing streamMetabolizer, mda.streams
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', configure.args='--no-docs --no-html')
install.packages(
  c('streamMetabolizer', 'mda.streams'),
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', configure.args='--no-docs --no-html')

library(httr)
library(devtools)
library(streamMetabolizer)
devtools::session_info()
library(mda.streams)

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
