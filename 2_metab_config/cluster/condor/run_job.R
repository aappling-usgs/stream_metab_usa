# update & install packages from the local repo. use an lapply to ensure
# ordering of installation
message('installing packages')
oldpkgs <- rownames(old.packages(repos="file:bundle"))
keypkgs <-   c(#'curl','digest','stringi','stringr', # these are out of date on run nodes unless we call them out
  #'memoise', # must be updated before devtools
  #'devtools', 
  'dplyr', 'tidyr', 'ggplot2',
  #'httr', 'xml2', # must be updated before dataRetrieval
  #'unitted', # needed for streamMetabolizer
  'streamMetabolizer', 'mda.streams')
toinstall <- c(setdiff(oldpkgs, keypkgs), keypkgs)
print(toinstall)
installout <- lapply(
  toinstall,
  install.packages, 
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', 
  INSTALL_opts=c('--no-docs','--no-html')
)

# report on package versions
message('describing installed packages')
print(.libPaths())
ip <- installed.packages()
print(ip[order(rownames(ip)),c('LibPath','Version'),drop=FALSE])

# describe session
message('describing session')
print(sessionInfo())
print(devtools::session_info())

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1]) + 1 # 0-indexed by HTCondor; convert to 1-indexed
message(paste0('this is job ', job))

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model.R')
source('run_model_job.R')
model_out <- run_model_job(job, outdir='job', run_fun=run_model)
