#' This is the master R script that coordinates package installation 
#' (install_packages.R), getting info from the cluster (SLURM_ARRAY_TASK_ID 
#' section), model handling and posting (run_model_job.R), and the actual 
#' modeling process (run_model.R). This script is equivalent to the combination 
#' of run_job.sh and run_job.R for HTCondor, with amendments to acknowledge that
#' we have permanent package installations and storage space on Yeti.

# unzip the package bundle
unzip('bundle.zip')

# ensure there's a directory to install packages locally
libdir <- '/cxfs/projects/usgs/water/owi/powstreams/rLibs'
if(!dir.exists(libdir)) dir.create(libdir)

# install the packages needed for this job
source('install_packages.R')
install_packages(libdir, oldonly=TRUE)

# get the model ID to run
batch <- as.integer(Sys.getenv('SLURM_ARRAY_JOB_ID', 'NA')) # these are exactly the indices you request in #SBATCH --array
job <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA')) # these are exactly the indices you request in #SBATCH --array
message(paste0('this is job ', job, ' in batch ', batch))

# make a directory to store results in, ~as in cluster/condor/run_job.sh
outdir <- sprintf('%d/job_%0.0f', batch, job)
if(!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model_job.R')
source('run_model.R')
model_out <- run_model_job(job, outdir=outdir, run_fun=run_model, cluster='yeti')
