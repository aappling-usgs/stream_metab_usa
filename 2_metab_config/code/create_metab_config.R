#' Create a config file to use as guidance for metabolism runs
#' 
#' @import mda.streams
#' @import streamMetabolizer
#' @import dplyr
#' @import tidyr
#' 
#' @param config a config for the config
create_metab_config <- function(smu.config=yaml::yaml.load_file('../2_metab_config/in/metab_configs_config.yml'),
                                prep.config.file='../2_metab_config/prep/out/config.tsv',
                                param.file='../2_metab_config/prep/out/params.tsv',
                                outfile='../2_metab_config/out/config.tsv') {
  
  prep.config <- read_config('../2_metab_config/prep/out/config.tsv')
  params <- read.table(param.file, header=TRUE, sep='\t', stringsAsFactors=FALSE)
  
  cfg.calcs <- params %>%
    filter(tsteps_min != '', K600_med != 'NA') %>%
    mutate(
      K600_daily_sigma_sigma = 0.02 * pmin(50, pmax(0.5, as.numeric(K600_med))), # cap K600_med at between 0.5 and 50
      ts_min = strsplit(tsteps_min, ',')
    ) %>%
    unnest(ts_min) %>%
    mutate(ts_day = as.numeric(ts_min) / (60*24)) %>%
    mutate(
      specs=sprintf(paste0(
        "list(specs=specs('b_Kb_oipi_tr_plrckm.stan',",
        "required_timestep=%0.08f,", # ts_day
        "K600_lnQ_nodes_centers=seq(%s,%s,by=0.2),", # Qnodemin, Qnodemax, 
        "K600_lnQ_nodediffs_sdlog=0.1,",
        "K600_daily_sigma_sigma=%0.04f,", # K600_daily_sigma_sigma
        "params_out=c('GPP_daily','ER_daily','K600_daily','K600_daily_predlog','lnK600_lnQ_nodes','K600_daily_sigma','err_obs_iid_sigma','err_proc_iid_sigma')",
        "burnin_steps=1000,saved_steps=500,",
        "))"), 
        ts_day,
        Qnodemin, Qnodemax, 
        K600_daily_sigma_sigma)
    ) %>%
    mutate(
      strat=sprintf('bayes_%smin', ts_min)
    ) %>%
    select(site_name, strat, specs)

  cfg <- prep.config %>%
    inner_join(cfg.calcs, by=c(site='site_name')) %>%
    mutate(
      tag=smu.config[['tag']],
      strategy=strat,
      model='metab_bayes',
      model_args=specs,
      config.row=1:n()) %>%
    select_(.dots=names(prep.config))
    
  cfg.file <- write_config(cfg, outfile)
  
}