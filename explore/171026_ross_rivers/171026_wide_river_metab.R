library(dplyr)

coords <- readr::read_tsv('../4_data_release/cache/site_data.tsv')
zipdaily <- '../4_data_release/cache/models/post/daily_predictions.zip'
unzip(zipdaily, exdir=tempdir())
dailies <- readr::read_tsv(file.path(tempdir(), 'daily_predictions.tsv'))

wide_metab_dvq <- 
  # compute median discharge for each site
  dailies %>% 
  group_by(site_name) %>%
  summarize(discharge=median(discharge)) %>%
  # compute width from median discharge
  left_join(coords, by=c('site_name')) %>%
  mutate(width=dvqcoefs.a*discharge^dvqcoefs.b) %>%
  # simplify
  select(site_name, width, lat, lon) %>%
  # filter to wide rivers
  filter(width >= 60) %>%
  # add daily predictions
  left_join(dailies, by='site_name')

readr::write_tsv(wide_metab_dvq, '../explore/171026_ross_rivers/wide_metab_dvq.tsv')

better_wides <- readr::read_csv('../explore/171026_ross_rivers/MetabolismRivers60.csv')

wide_metab_gee <- 
  better_wides %>%
  select(site_name) %>%
  distinct() %>%
  # add daily predictions
  left_join(dailies, by='site_name')

readr::write_tsv(wide_metab_gee, '../explore/171026_ross_rivers/metab_60m.tsv')
