prep_summary_jobs <- function(config.file='../2_metab_outputs/out/config.tsv', num.jobs=50) {
  library(dplyr)
  library(mda.streams)
  jobtbl_old <- read_config(config.file) %>%
    select(config.row) %>%
    mutate(config.row=as.numeric(config.row),
           job.id=rep(seq_len(50), length.out=n()))
  
  # use the fit files as a proxy for all of the files for a given site (for now)
  donefits <- dir(file.path(dirname(config.file), 'fits')) %>%
    gsub(' fit\\.tar\\.gz', '', .) %>% 
    parse_metab_model_name(out='row', use_names=F)
  
  jobtbl <- filter(jobtbl_old, !(config.row %in% donefits)) %>%
    mutate(job.id=rep(seq_len(num.jobs), length.out=n())) %>%
    arrange(config.row)
  write.table(jobtbl, file.path(dirname(config.file), 'cluster_jobs.tsv'), row.names=FALSE, sep='\t')
}

# for(fit in dir('../2_metab_outputs/condor/results/results2', full.names=TRUE)) {
#   for(type in c('fits','preds','summaries','tses')) {
#     ftfiles <- dir(file.path(fit,type), full.names=TRUE)
#     file.copy(ftfiles, file.path('../2_metab_outputs/condor/results',type))
#   }
# }
