#' for all site metadata files listed in ..., determine the types, download from
#' sciencebase, and combine and rename and filter into a nice single table
#' relevant to exactly the sites we want
combine_site_data <- function(...) {
  library(mda.streams)
  library(dplyr)
  library(unitted)

  meta_types <- c(...)
  meta <- get_meta(meta_types) %>% # pull from sciencebase because get_meta adds useful prefixes
    select(-site_database, -sciencebase_id) %>%
    select(site_name, nwis_id=site_num, nhdplus_id, everything())
  
  return(meta)
}

write_site_data <- function(meta, outfile) {
  
  out <- deunitted(meta)
  write.table(out, file=outfile, sep='\t', row.names=FALSE, quote=TRUE)
  
}
