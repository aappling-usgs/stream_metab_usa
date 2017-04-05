#' Create a site list for the metabolism runs
#' 
#' @import mda.streams
#' @import dplyr
#' @import unitted
#' 
#' @import lib/write_status_table.R
choose_metab_sites <- function(harvey.file='../1_site_data/in/sites_harvey_ask.txt',
                               site.file='../1_site_data/out/site_list.tsv') {
  
  # jud gave us more coefficients than we requested. use their union
  sites_harvey_ask <- make_site_name(readLines(harvey.file), 'nwis')
  sites_harvey_got <- get_meta('dvqcoefs') %>% unitted::v() %>% filter(!is.na(dvqcoefs.c)) %>% .$site_name
  sites_harvey <- union(sites_harvey_ask, sites_harvey_got)
  
  # sites that met the criteria for the most recent ts query
  sites_fresh <- read_status_table(site.file) %>% filter(!no.data) %>% .$site_name
  
  # find what's available on SB with much of the required data for sure
  sites_sb_all <- list_sites(c('doobs_nwis','disch_nwis','wtr_nwis'))
  
  # return sites that are on all three lists: has data, was included in most 
  # recent timeseries data pull, and was part of the hydraulic geometry 
  # discussion (even if we didn't actually get coefficients in the end)
  sites_sb_all %>%
    intersect(sites_fresh) %>%
    intersect(sites_harvey)
}
