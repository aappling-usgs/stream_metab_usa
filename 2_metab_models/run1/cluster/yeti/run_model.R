# install packages from the local repo
devtools::session_info()

# get the model ID to run
job <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA')) # these are exactly the indices you request in #SBATCH --array

# make sure there's a place for the outputs
outdir <- 'results'
if(!dir.exists(outdir)) dir.create(outdir)

# run the model
source('run_sdh_test.R')
run_sdh_test(job, outdir)
