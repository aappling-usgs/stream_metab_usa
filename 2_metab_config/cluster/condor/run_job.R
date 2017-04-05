#' This is the master R script that coordinates package installation 
#' (install_packages.R), getting info from the cluster (commandArgs code
#' section), model handling and posting (run_model_job.R), and the actual
#' modeling process (run_model.R)

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1]) + 1 # 0-indexed by HTCondor; convert to 1-indexed
message(paste0('this is job ', job, ' (HTCondor job ', job-1, ')'))

# fix the output directory
outdir <- 'job'

# install the packages needed for this job
source('install_packages.R')
tryCatch(
  install_packages(),
  error=function(e) {
    message('package installation had an error; see error file')
    writeLines(c(e$message, capture.output(print(e$call))), file.path(outdir, sprintf("error job_%s.txt", job-1)))
    saveRDS(e, file.path(outdir, sprintf("errRds job_%s.Rds", job-1)))
    stop(e)
  })

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model_job.R')
source('run_model.R')
model_out <- run_model_job(job, outdir=outdir, run_fun=run_model)
