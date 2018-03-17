
#' figure out what files are ready to be downloaded from SB
#' @param target_name the nwis_{site.id}_... target name. Format matters, since we parse it
#' @return a vector of file names for this site
ts_files_for_download <- function(target_name, ts.dir){
  
  library(dplyr)
  match.id <- paste0(paste(strsplit(target_name, '[_]')[[1]][1:2], collapse = '_'), '-')
  ts.files <- dir(ts.dir, pattern='files_ts_', full.names=TRUE)
  file.list <- c()
  for (file in ts.files){
    file.meta <- readr::read_tsv(file) %>%  filter(grepl(filepath, pattern = match.id))
    if (nrow(file.meta) == 1 && all(file.meta$posted, file.meta$tagged)){
      file.list <- c(file.list, file.meta$filepath)
    } else if (nrow(file.meta) > 1){
      stop('something is wrong, multiple file matches for ', target_name)
    }
  }
  return(file.list)
}

#' find all files that need to be local for a site, DL them
#' 
#' @param site.id the nwis_{site.no} to be used
#' @return a vector of file paths for the downloaded files

download_release_tses <- function(file.paths){
  for (file.path in file.paths){
    parsed.path <- mda.streams::parse_ts_path(file.path, out = c("dir_name", "file_name", "var_src", "site_name"))
    mda.streams::download_ts(var_src = parsed.path$var_src, site_name = parsed.path$site_name, 
                             folder = parsed.path$dir_name, on_local_exists = 'skip', on_remote_missing = 'stop')
  }
  return(file.paths)
}

post_release_tses <- function(target.name, parent.id, file.paths){
  
  tmpdir <- tempdir()
  zipfile <- file.path(tmpdir, paste0(target.name, '.zip'))
  
  rds.files <- file.paths
  tsv.files <- c()
  for (rds.file in rds.files){
    parsed.path <- mda.streams::parse_ts_path(rds.file, out = c("dir_name", "file_name", "var", "src", "site_name", "ts_name"))
    tsv.file <- make_ts_path(site_name = parsed.path$site_name, ts_name = parsed.path$ts_name, folder = tmpdir, version = 'tsv')
    tsv.file <- gsub(tsv.file,pattern = '.tsv.gz', replacement = '.tsv')
    write.table(x = unitted::deunitted(readRDS(rds.file)), file=tsv.file, sep='\t', row.names=FALSE, quote=TRUE)
    tsv.files <- c(tsv.files, tsv.file)
  }
  
  old.dir <- setwd(tmpdir)
  zip(zipfile = basename(zipfile), files = basename(tsv.files))
  setwd(old.dir)
  key <- strsplit(basename(zipfile), '[.]')[[1]][1]
  
  create_release_item(parent.id, key, zipfile)
  # zip those dogs and post
  unlink(c(zipfile, tsv.files))
}

#### timeseries metadata ####

attributes_timeseries <- function(
  ent.file='../4_data_release/in/attr_timeseries.rds',
  zip.dir='../1_timeseries/cache',
  attr.file.base='../4_data_release/in/attr_timeseries.csv',
  release_sites) {
  
  # get a list of all relevant data files
  rds_files <- dir(zip.dir, pattern='nwis_[[:digit:]]+-ts_[[:alpha:]]+_[[:alpha:]]+.rds$', full.names=TRUE)
  rds_info <- mda.streams::parse_ts_path(rds_files) %>% mutate(file_path=rds_files) %>%
    filter(site_name %in% release_sites)
  
  # read in a single example data file of each type for structure
  examples <- rds_info %>% group_by(var_src) %>% summarize(file_path=file_path[1])
  data_dfs <- lapply(setNames(examples$file_path, examples$var_src), function(example) {
    unitted::v(readr::read_rds(example))
  })
  
  # compute ranges for each column of the many files of each entity type (ftype)
  ranges_dfs <- lapply(setNames(nm=unique(rds_info$var_src)), function(ftype) {
    filesoftype <- rds_info %>% filter(var_src==ftype) %>% pull(file_path)
    ranges_df <- multifile_ranges(filesoftype, coltypes=NA)
  })
  
  # define variables for definitions text that may be used more than once
  vsc <- mda.streams::get_var_src_codes()
  var_defs <- tibble::tribble(
    ~`var_src`, ~`attr-def`, ~`attr-defs`,
    
    # datetime
    'DateTime', 'Date-time in UTC. Acquired from contributing dataset[s] and converted to UTC if necessary', 'This release',
    
    # GLDAS
    'baro_gldas', 'Surface pressure (psurf_f_inst) from GLDAS database', 'GLDAS_NOAH025_3H_V2.0 (see Source Information)',
    'sw_gldas', 'Downward shortwave radiation flux, surface (SWdown_f_tavg) from GLDAS database', 'GLDAS_NOAH025_3H_V2.0 (see Source Information)',
    
    # NLDAS
    'baro_nldas', 'Surface pressure (pressfc) from NLDAS database', 'NLDAS_FORA0125_H.002 (see Source Information)',
    'sw_nldas', 'Downwards shortwave radiation flux, surface (dswrfsfc) from NLDAS database', 'NLDAS_FORA0125_H.002 (see Source Information)',
    
    # NWIS
    'disch_nwis', 'Discharge (parameter 00060) from NWIS database', 'USGS_NWIS (see Source Information)',
    'doobs_nwis', 'Dissolved oxygen concentration (parameter 00300) from NWIS database', 'USGS_NWIS (see Source Information)',
    'wtr_nwis', 'Water temperature (parameter 00010) from NWIS database', 'USGS_NWIS (see Source Information)',
    
    # calculated, inst
    'sitedate_calcLon', 'Date expressed as solar noon at the site. Calculated with streamMetabolizer convert_UTC_to_solartime function', 'This release',
    'sitetime_calcLon', 'Mean solar time. Calculated from DateTime and site longitude with streamMetabolizer convert_UTC_to_solartime function', 'This release',
    'suntime_calcLon', 'Apparent solar time. Calculated from DateTime and site coordinates with streamMetabolizer convert_UTC_to_solartime function', 'This release',
    'dopsat_calcObsSat', 'Percent dissolved oxygen saturation. Calculated from doobs_nwis and dosat_calcGGbts as 100*doobs/dosat', 'This release',
    'dosat_calcGGbconst', 'Hypothetical dissolved oxygen concentration at saturation. Calculated from baro_calcElev with streamMetabolizer calc_DO_sat function, using coefficients from Garcia and Gordon 1992', 'This release',
    'dosat_calcGGbts', 'Hypothetical dissolved oxygen concentration at saturation. Calculated from baro_nldas (or baro_gldas when baro_nldas unavailable) with streamMetabolizer calc_DO_sat function, using coefficients from Garcia and Gordon 1992', 'This release',
    'par_calcLat', 'Photosynthetically active radiation. Calculated from site latitude and suntime with streamMetabolizer calc_light function', 'This release',
    'par_calcSw', 'Photosynthetically active radiation. Calculated from sw_nldas (or sw_gldas when sw_nldas unavailable) with streamMetabolizer convert_PAR_to_SW function', 'This release',
    'par_calcLatSw', 'Photosynthetically active radiation. Merger of par_calcLat and par_calcSw using streamMetabolizer calc_light_merged function', 'This release',
    'baro_calcElev', 'Surface pressure. Calculated from site elevation using streamMetabolizer calc_air_pressure function', 'This release',
    'depth_calcDischHarvey', 'Spatially averaged stream depth. Calculated from discharge and the site-specific hydraulic geometry coefficients in site_data.tsv, where depth=cQ^f', 'This release',
    'depth_calcDischRaymond', 'Spatially averaged stream depth. Calculated from discharge and global hydraulic geometry coefficients from Raymond et al. 2012, where depth=cQ^f', 'This release',
    'veloc_calcDischHarvey', 'Stream velocity. Calculated from discharge and the site-specific hydraulic geometry coefficients in site_data.tsv, where velocity=kQ^m', 'This release',
    'veloc_calcDischRaymond', 'Stream velocity. Calculated from discharge and global hydraulic geometry coefficients from Raymond et al. 2012, where velocity=kQ^m', 'This release',
    
    # calculated, daily
    'doamp_calcDAmp', 'Daily (4am to 3:59am) amplitude in dissolved oxygen percent saturation. Calculated from dopsat_calcObsSat', 'This release',
    'swdaily_calcDMean', 'Daily (4am to 3:59am) average of sw_nldas (or sw_gldas when sw_nldas unavailable)', 'This release',
    'dischdaily_calcDMean', 'Daily (4am to 3:59am) average of disch_nwis', 'This release',
    'velocdaily_calcDMean', 'Daily (4am to 3:59am) average of veloc_calcDischHarvey (or veloc_calcDischRaymond when veloc_calcDischHarvey unavailable)', 'This release'
  ) %>% 
    left_join(select(vsc, var_src, `attr-label`=var, `data-units`=units), by='var_src') %>%
    mutate(`attr-label` = ifelse(var_src == 'DateTime', 'DateTime', `attr-label`))
  
  # create one attr file per entity type
  all_attr_dfs <- lapply(setNames(nm=names(ranges_dfs)), function(ftype) {
    one.attr.file <- gsub('timeseries', paste0('timeseries_', ftype), attr.file.base)
    ranges_df <- ranges_dfs[[ftype]]

    # prepare the basic attr_df skeleton as a data_frame
    if(file.exists(one.attr.file)) file.remove(one.attr.file)
    attribute_skeleton(data_dfs[[ftype]], one.attr.file)
    attr_df <- readr::read_csv(one.attr.file, col_types = 'cccnnc')
    
    # write definitions for each column
    defs_df <- var_defs %>%
      filter(var_src %in% c('DateTime', ftype)) %>%
      select(-var_src)
    
    # combine the skeleton, ranges, and definitions
    attr_df_combined <- attr_df %>%
      select(`attr-label`) %>%
      left_join(select(ranges_df, `attr-label`, `data-min`, `data-max`), by='attr-label') %>%
      left_join(defs_df, by='attr-label') %>%
      select(names(attr_df))
    
    # write the final attribute table
    readr::write_csv(attr_df_combined, path=one.attr.file)
    return(attr_df_combined)
  })
  
  # write as RDS a list containing separate entity information for each entity type
  ent_list <- list(entities=lapply(names(all_attr_dfs), function(ent_name) {
    attr_df <- all_attr_dfs[[ent_name]]
    c(list(
      'data-name'=sprintf('nwis_XXXX-ts_%s.tsv', ent_name),
      'data-description'=sprintf(
        paste('Tab-separated 2-column data table containing date-time stamps (first column) and values for the variable code-named %s;',
              'see attribute definition for 2nd column for variable and data source details.'),
        ent_name)),
      as.attr_list(attr_df)
    )
  }))
  saveRDS(ent_list, ent.file)
}

#### renderer ####

render_timeseries_metadata <- function(out_file, child_yaml, points_list, ent_rds, parent_list, template) {
  child_list <- yaml::yaml.load_file(child_yaml)
  ent_list <- readRDS(ent_rds)
  render(filename=out_file, data=child_list, points_list, ent_list, parent_list, template=template)
}
