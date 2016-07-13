# Downloading data to decide on a widely-applicable relationship to fit for K vs
# Q (or other predictors)

library(mda.streams)
library(dplyr)
library(ggplot2)
library(unitted)
library(streamMetabolizer)

# depth source comparison
# dhts <- get_ts(c('depth_calcDischHarvey'), site='nwis_08062500')
# drts <- get_ts(c('depth_calcDischRaymond'), site='nwis_08062500')
# dts <- full_join(drts, dhts, by='DateTime') %>% rename(depth_Raymond=depth.x, depth_Harvey=depth.y) %>% v()
# ggplot(dts, aes(x=depth_Raymond, y=depth_Harvey)) + xlim(0,NA) + ylim(0,NA) + geom_abline(color='grey') + geom_line(color='blue') + theme_bw()

sites <- list_sites()
depth_best <- choose_data_source('depth', sites)
sites_w_depth <- which(!is.na(depth_best$src))
login_sb() # uses profile
mms0.1.0 <- list_metab_models("160423 0.1.0 PRK_initial")
block_size <- 87
num_blocks <- ceiling(length(sites_w_depth)/block_size)
login_sb() # uses profile
all_tses <- lapply(1:num_blocks, function(block) {
  site_index_indices <- ((block-1)*block_size + (1:block_size)) %>% .[. <= length(sites_w_depth)]
  tses <- lapply(sites_w_depth[site_index_indices], function(i) {
    message(sprintf('%3d: %s', i, sites[i]))
    mm_title <- search_metab_models(metab_models=mms0.1.0, site=paste0('^', sites[i], '$'), fixed=FALSE)
    mm <- get_metab_model(mm_title)
    if(!is(mm, 'metab_model')) return(tibble())
    preds <- predict_metab(mm) %>%
      select(date, starts_with('K600'), warnings, errors)
    qz <- get_ts(c('sitedate_calcLon','disch_nwis', make_var_src('depth', depth_best$src[i])), site=depth_best$site[i]) %>%
      v() %>% select(-DateTime)
    kqz <- left_join(preds, qz, by=c('date'='sitedate')) %>%
      mutate(site=sites[i], depth_src=depth_best$src[i]) %>%
      select(site, date, starts_with('K600'), disch, depth, K600.warnings=warnings, K600.errors=errors)
  })
  bound_tses <- bind_rows(tses)
  saveRDS(bound_tses, paste0('explore/160705_KvQ/KvQdata',block,'.Rds'))
  tses
})

all_tses <- lapply(1:num_blocks, function(block) {
  readRDS(paste0('explore/160705_KvQ/KvQdata',block,'.Rds'))
}) %>% bind_rows()
saveRDS(all_tses, paste0('explore/160705_KvQ/KvQdata_all.Rds'))
