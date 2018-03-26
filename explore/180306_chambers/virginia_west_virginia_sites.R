# Subset daily metabolism predictions for 7 sites in northern virginia at request of John Jastram

library(tidyverse)
library(mda.streams)

# 01608500	 SOUTH BRANCH POTOMAC RIVER NEAR SPRINGFIELD, WV  
# 01611500	 CACAPON RIVER NEAR GREAT CACAPON, WV 
# 01673000	 PAMUNKEY RIVER NEAR HANOVER, VA 	
# 02035000	 JAMES RIVER AT CARTERSVILLE, VA 
# 03058000	 WEST FORK R BL STONEWALL JACKSON DAM NR WESTON, WV 
# 03061000	 WEST FORK RIVER AT ENTERPRISE, WV  
# 03067510	 SHAVERS FORK NR CHEAT BRIDGE, WV 
# 03183500	 GREENBRIER RIVER AT ALDERSON, WV  
# 01668000	 RAPPAHANNOCK RIVER NEAR FREDERICKSBURG, VA  ##### NOT FOUND - probably b/c not in list sent to jud ###
vawv_sites <- c('01608500','01611500','03058000','03061000','03067510','03183500','01668000','01673000','02035000')
site_data <- readr::read_tsv('../4_data_release/cache/site_data.tsv') %>%
  filter(nwis_id %in% vawv_sites) %>%
  select(nwis_id, long_name, starts_with('struct'))
daily_predictions <- readr::read_tsv('../4_data_release/cache/models/daily_predictions.tsv')
daily_preds <- daily_predictions %>%
  mutate(nwis_id = parse_site_name(site_name, out='sitenum')) %>%
  filter(nwis_id %in% vawv_sites) %>%
  select(-nwis_id)
diagnostics <- readr::read_tsv('../4_data_release/cache/models/diagnostics_d.tsv')
diags <- filter(diagnostics, site %in% paste0('nwis_', vawv_sites))

readr::write_csv(daily_preds, '../explore/180306_chambers/daily_predictions.csv')

#### explore ###
site_data
diags

# inputs
tsm <- get_ts(c("doobs_nwis", "dosat_calcGGbts", "wtr_nwis", "par_calcLatSw", "depth_calcDischRaymond"), site_name = 'nwis_01668000')
ggplot(v(tsm) %>% gather(var, val, doobs, dosat, wtr, par, depth), aes(x=DateTime, y=val)) + facet_grid(var ~ ., scales='free') + geom_line()

# predictions
ggplot(daily_preds, aes(x=date, y=GPP)) + geom_line() + facet_wrap(~ site_name)
ggplot(daily_preds, aes(x=date, y=ER)) + geom_line() + facet_wrap(~ site_name)
ggplot(daily_preds, aes(x=date, y=K600)) + geom_line() + facet_wrap(~ site_name, scales='free')

