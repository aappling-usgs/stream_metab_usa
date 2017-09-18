make_expert_file <- function(model.stats.file='../2_metab_outputs/out/model_stats.tsv', 
                             outfile='../2_metab_outputs/out/expert_file.tsv') {
  library(mda.streams)
  library(dplyr)
  
  all_stats <- read.table(model.stats.file, header=TRUE, stringsAsFactors=FALSE, sep='\t')
  
  # starter file for bob and maite's expert assessment
  bm_model_rows <- all_stats %>%
    select(model_name, site, resolution, num_days=ER_daily_Rhat.n, saved_steps, max_key_Rhat, is_converged, needs_rerun)
  bm_sum_rows <- bm_model_rows %>%
    group_by(site) %>%
    summarize(model_name='Overall', num_models = length(resolution)) %>%
    ungroup()
  bob_maite <- full_join(bm_model_rows, select(filter(bm_sum_rows, num_models > 1), -num_models), by=c('site','model_name')) %>%
    arrange(site, model_name) %>%
    select(site, everything()) %>%
    mutate(confidence = NA)
  
  # make sure the ordering matches the existing spreadsheet. Need to download
  # Expert Model Assessment from Google Drive as tsv and put it in
  # 2_metab_outputs/in for this to work.
  prev_file <- read.table('../2_metab_outputs/in/Expert Model Assessment.tsv', header=TRUE, stringsAsFactors=FALSE, sep='\t')
  bob_maite <- bob_maite[match(prev_file$model_name_run3, bob_maite$model_name),]
  
  write.table(bob_maite, outfile, row.names=FALSE, sep='\t')
}
