#' This is the master R script that coordinates package installation 
#' (install_packages.R), getting info from the cluster (commandArgs code
#' section), model handling and posting (run_model_job.R), and the actual
#' modeling process (run_model.R)

# install the packages needed for this job
source('install_packages.R')
install_packages()

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1]) + 1 # 0-indexed by HTCondor; convert to 1-indexed
message(paste0('this is job ', job))

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model_job.R')
source('run_model.R')
model_out <- run_model_job(job, outdir='job', run_fun=run_model, cluster='condor')
