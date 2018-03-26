# Subset daily metabolism predictions for 7 sites in northern virginia at request of John Jastram

library(tidyverse)
library(mda.streams)

# 01645704                Difficult Run Above Fox Lake Near Fairfax,
# 01645762                SF Little Difficult Run Ab Mouth Nr Vienna, VA
# 01646000                Difficult Run near Great Falls, VA
# 01646305                Dead Run At Whann Avenue Near Mclean, VA       
# 01654000                Accotink Creek near Annandale, VA
# 01654500                Long Branch at Route 620 nr Annandale, VA
# 01656903                Flatlick Branch Ab Frog Branch At Chantilly,
nova_sites <- c('01645704','01645762','01646305','01656903','01654500','01646000','01654000')
site_data <- readr::read_tsv('../4_data_release/cache/site_data.tsv') %>%
  filter(nwis_id %in% nova_sites) %>%
  select(nwis_id, long_name, starts_with('struct'))
daily_predictions <- readr::read_tsv('../4_data_release/cache/models/daily_predictions.tsv')
daily_preds <- daily_predictions %>%
  mutate(nwis_id = parse_site_name(site_name, out='sitenum')) %>%
  filter(nwis_id %in% nova_sites) %>%
  select(-nwis_id)

readr::write_csv(daily_preds, '../explore/180221_jastram/daily_predictions.csv')
