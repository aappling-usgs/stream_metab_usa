#' Create a config file to use as guidance for metabolism runs
#' 
#' @import mda.streams
#' @import streamMetabolizer
#' @import dplyr
#' @import tidyr
#' 
#' @param config a config for the config
create_run1_config <- function(run1_yaml=read_run1_yaml(), sites,
                               outfile="../2_metab_models/run1/out/config.tsv") {
  cfg <- stage_metab_config(
    tag=run1_yaml[['tag']],
    strategy='MLE_prep_run',
    date='2017-02-06 15:15:15 -0600',
    model='metab_mle',
    model_args="list(specs=specs('m_np_oi_tr_plrckm.nlm'))",
    site=sites,
    disch=choose_data_source('disch', sites),
    filename=outfile
  )
  
  cfgdf <- read_config(cfg)
  
  # summarize the ts srces that were selected, and their relative priorities, for posterity
  src.choices <- cfgdf %>% 
    gather(src.var, src, sitetime.src, doobs.src, dosat.src, depth.src, wtr.src, par.src, disch.src) %>%
    mutate(var=substring(src.var, 1, nchar(src.var)-4)) %>%
    select(var, src)
  vsc <- get_var_src_codes() %>%
    select(var, src, priority) %>% 
    filter(var %in% src.choices$var, src %in% src.choices$src) %>%
    right_join(src.choices, by=c('var','src')) %>%
    group_by(var, src) %>%
    summarize(priority=as.numeric(unique(priority)), count=length(src)) %>%
    mutate(priority=as.numeric(priority)) %>%
    ungroup() %>%
    group_by(var) %>%
    mutate(priority = priority - min(priority) + 1)
  write.table(vsc, '../2_metab_models/run1/out/src_priorities.tsv', sep='\t', row.names=FALSE)
  
  return(cfgdf)
}
