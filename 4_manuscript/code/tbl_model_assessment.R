#' Tally the models assigned to Low, Medium, or High confidence
tbl_model_assessment <- function(
  diagnostics='../4_data_release/cache/models/post/diagnostics.zip',
  outfile='../4_manuscript/tbl/model_assessment.tex'
) {
  diags <- readr::read_tsv(diagnostics)
  
  rating_tbl <- diags %>%
    mutate(
      Rhat_L = K600_daily_sigma_Rhat > 1.2 | err_proc_iid_sigma_Rhat > 1.2,
      Rhat_M = NA,
      Rhat_H = !Rhat_L,
      Krange_L = K_range > 50,
      Krange_M = K_range <= 50 & K_range > 15,
      Krange_H = K_range <= 15,
      GPP_L = neg_GPP > 50,
      GPP_M = neg_GPP <= 50 & neg_GPP > 25,
      GPP_H = neg_GPP <= 25,
      ER_L = pos_ER > 50,
      ER_M = pos_ER <= 50 & pos_ER > 25,
      ER_H = pos_ER <= 25) %>%
    summarize_at(vars(Rhat_L:ER_H), function(rating) length(which(rating==TRUE))) %>%
    mutate(Rhat_M = NA) %>%
    gather(measure, count) %>%
    separate(measure, c('criterion','rating'), sep='\\_') %>%
    spread(rating, count) %>%
    bind_rows(
      as_data_frame(as.list(table(diags$model_confidence))) %>%
        mutate(criterion='Overall')
    ) %>%
    select(criterion, L, M, H) %>%
    mutate(criterion = ordered(criterion, levels=c('Rhat','Krange','GPP','ER','Overall'))) %>%
    arrange(criterion)

  rating_tex <- rating_tbl %>%
    mutate(tex=sprintf('%s & %s & %s & %s \\\\', criterion, L, M, H)) %>%
    pull(tex)
  
  writeLines(rating_tex, outfile)
}

