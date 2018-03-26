library(readr)
library(dplyr)

meta <- readr::read_tsv('../4_data_release/cache/site_data.tsv')

cfg <- readr::read_tsv('../4_data_release/cache/models/config.tsv')

filter(cfg, site %in% paste0('nwis_', c('07075250','07048600'))) %>%
  write_tsv('../explore/180320_justus_lucas_arkansas/config.tsv')

filter(cfg, site_name %in% paste0('nwis_', c('07075250','07048600')))