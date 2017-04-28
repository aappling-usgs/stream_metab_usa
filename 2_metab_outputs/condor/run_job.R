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

# summarize the models
status <- read.table('cluster_jobs.tsv', sep='\t', header=TRUE, stringsAsFactors=FALSE)
myrows <- status[status$job.id==job, 'config.row']
source('run_summary_job.R')
run_summary_job(rows=myrows, outdir=outdir)
