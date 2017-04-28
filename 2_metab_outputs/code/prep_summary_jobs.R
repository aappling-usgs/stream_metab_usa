prep_summary_jobs <- function(config.file='../2_metab_outputs/condor/config.tsv', num.jobs=50) {
  jobtbl <- read_config(config.file) %>%
    select(config.row) %>%
    mutate(config.row=as.numeric(config.row),
           job.id=rep(seq_len(num.jobs), length.out=n()))
  write.table(jobtbl, file.path(dirname(config.file), 'cluster_jobs.tsv'), row.names=FALSE, sep='\t')
}
