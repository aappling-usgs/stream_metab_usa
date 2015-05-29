# run one metabolism run based on command line input

#install packages first into local library
install.packages('mda.streams', repos='file:packages', lib='rLibs')
install.packages('streamMetabolizer', repos='file:packages', lib='rLibs')

args <- commandArgs(trailingOnly = TRUE)

cat(args, '\n')

run_indx = as.numeric(args[1])+1

library(mda.streams)
library(streamMetabolizer)

## Load the run file indexed by run_indx
## NOTE: The condor.sub file currently expects a file with name 'run_config_table.tsv'
##       If you use a different name, you need to edit condor.sub

#read.table('run_config_table.tsv', sep='\t', header=TRUE, as.is=TRUE, ...)


## Run the model

#execute_metabolism


## write a results file
## This can be anything you want really

#write.table(...)
