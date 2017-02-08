# update & install packages from the local repo. use an lapply to ensure ordering of installation
pkgdeps <- rownames(available.packages(repos="file:bundle"))
installout <- lapply(
  c('curl','digest','stringi','stringr', # these are out of date on run nodes unless we call them out
    'memoise', # must be updated before devtools
    'devtools', 'dplyr', 'tidyr', 'ggplot2',
    'httr', 'xml2', # must be updated before dataRetrieval
    'unitted', # needed for streamMetabolizer
    'streamMetabolizer', 'mda.streams'),
  install.packages, 
  repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib='rLibs', 
  INSTALL_opts=c('--no-docs','--no-html')
)

# report on package versions
print(.libPaths())
print("A")
installed.packages(lib.loc='rLibs')
print("B")
installed.packages()
tryCatch({
  print(packageVersion(streamMetabolizer, lib.loc='rLibs'))
  print(packageVersion(mda.streams, lib.loc='rLibs'))
}, error=function(e) print("couldn't identify packageVersions for sM, m.s"))

# get the model ID to run
args <- commandArgs(trailingOnly = TRUE)
job <- as.numeric(args[1]) + 1 # 0-indexed by HTCondor; convert to 1-indexed

# run, summarize, stage, and post the model. run_model_job() should be
# applicable to both prep and regular metab models. run_model() should be
# specific to each
source('run_model.R')
source('run_model_job.R')
model_out <- run_model_job(job, outdir='job', run_fun=run_model)
