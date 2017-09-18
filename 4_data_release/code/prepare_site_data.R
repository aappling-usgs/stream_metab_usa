#' for all site metadata files listed in ..., determine the types, download from
#' sciencebase, and combine and rename and filter into a nice single table
#' relevant to exactly the sites we want
combine_site_data <- function(..., outfile) {
  library(mda.streams)
  library(dplyr)
  library(unitted)

  infiles <- list(...)
  meta_types <- parse_meta_path(unlist(infiles))$type
  meta <- get_meta(meta_types) %>% # pull from sciencebase because get_meta adds useful prefixes
    select(-site_database, -sciencebase_id) %>%
    select(site_name, nwis_id=site_num, nhdplus_id, everything())
  
  # units <- get_units(meta)
  # out <- deunitted(meta)
  
  return(meta) # or list(units, out)?
}
