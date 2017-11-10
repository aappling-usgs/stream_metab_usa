attributes_metab_config <- function(
  unzipped='../2_metab_models/run3/out/config.tsv',
  attr.file='../4_data_release/in/attr_metab_config.csv') {
  
  # sketch out and read in the attribute table
  attribute_skeleton(unzipped, attr.file)
  attr_df <- readr::read_csv(attr.file, col_types = 'cccnnc')
  
  # learn about the data
  data_df <- readr::read_tsv(unzipped)
  
  # fill out the attribute table
  
  # write the final attribute table
  readr::write_csv(attr_df, path=attr.file)
}
  

attributes_metab_inputs <- function(
  zip.dir='../4_data_release/cache/models/post',
  attr.file='../4_data_release/in/attr_metab_inputs.csv') {
  
  # sketch out and read in the attribute table
  attribute_skeleton(unzipped, attr.file)
  attr_df <- readr::read_csv(attr.file, col_types = 'cccnnc')
  
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'inputs'))
  
  # input_files <- dir(dirname(object.zip), pattern='_input.zip$', full.names=TRUE)
  # # read and summarize each file
  # summaries <- bind_rows(lapply(input_files, function(zip.file) {
  #   message(zip.file)
  #   unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'inputs'))
  #   input_df <- readr::read_tsv(unzipped, col_types = 'Tdddddd')
  #   unlink(unzipped)
  #   as_data_frame(lapply(input_df, range, na.rm=TRUE)) %>%
  #     mutate(
  #       site=gsub('_input\\.zip', '', basename(zip.file)),
  #       variable=c('min','max'))
  # }))
  # # do some data checks. some sites have some bad data, mostly not too bad  
  # filter(summaries, DO.obs < 0) # 5 sites with negative DO.obs, though seldom hugely negative
  # filter(summaries, DO.sat < 0) # 0 sites go negative
  # filter(summaries, depth < 0) # 0 sites go negative
  # filter(summaries, temp.water < -1) # nwis_01646500 has at least one -999999
  # filter(summaries, light < 0) # 0 sites go negative
  # filter(summaries, discharge < 0) # 32 sites go negative
  # 
  # # compute the overall range
  # ranges <- bind_rows(
  #   summarize_all(filter(summaries, variable=='min'), min),
  #   summarize_all(filter(summaries, variable=='max'), max)) %>%
  #   select(-site)
  # 
  # unzipped1 <- unzip(zipfile=input_files[1], exdir=file.path(tempdir(), 'input1'))
  # attribute_skeleton(object, attr.file, ...)
  # obj_df <- readr::read_tsv(object)
  # attr_df <- readr::read_csv(attr.file)
  
  # write the final attribute table
  readr::write_csv(attr_df, path=attr.file)
  
}

attributes_metab_fits <- function(
  zip.file='../4_data_release/cache/models/post/nwis_01124000_15min_fit.zip', # example file
  attr.file='../4_data_release/in/attr_metab_fits.csv') {
  
  unzippeds <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'fits'))
  attr.file.base <- attr.file
  for(unzipped in unzippeds) {
    
    # sketch out and read in the attribute table
    attr.file <- gsub('fits\\.csv', paste0('fits_',tools::file_path_sans_ext(basename(unzipped)),'.csv'), attr.file.base)
    attribute_skeleton(unzipped, attr.file)
    attr_df <- readr::read_csv(attr.file, col_types = 'cccnnc')
    
  
    # write the final attribute table
    readr::write_csv(attr_df, path=attr.file.1fit)  
  }
  
}

attributes_metab_diagnostics <- function(
  zip.file='../4_data_release/cache/models/post/diagnostics.zip',
  attr.file='../4_data_release/in/attr_metab_diagnostics.csv') {
  
  # sketch out and read in the attribute table
  attribute_skeleton(unzipped, attr.file)
  attr_df <- readr::read_csv(attr.file, col_types = 'cccnnc')
  
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'diagnostics'))
}

attributes_daily_preds <- function(
  zip.file='../4_data_release/cache/models/post/daily_predictions.zip',
  attr.file='../4_data_release/in/attr_metab_daily_preds.csv') {
  
  # read in the data file
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'preds'))
  dailies <- readr::read_tsv(unzipped)
  
  # sketch out the skeleton attribute table
  attr.temp <- tempfile(fileext='.csv')
  attribute_skeleton(dailies, attr.temp)
  attr_df <- readr::read_csv(attr.temp, col_types = 'cccnnc')

  # compute data-min and data-max (plus other diagnostics for intermediate use)
  ranges_df <- bind_rows(
    summarise_all(dailies, .funs=funs(format_bound(min(., na.rm=TRUE)))) %>% mutate(stat='data-min'),
    summarise_all(dailies, .funs=funs(format_bound(max(., na.rm=TRUE)))) %>% mutate(stat='data-max'),
    summarise_all(dailies, .funs=funs(format_bound(length(which(is.na(.)))))) %>% mutate(stat='num-NA')
  ) %>%
    gather(`attr-label`, value, -stat) %>%
    spread(stat, value)
  
  # write in / look up definitions & units
  vsunits <- function(variable) {
    unique(get_var_src_codes(metab_var==variable, out='metab_units'))
  }
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`, ~`data-units`,
    'site_name', 'Site identifier, consisting of prefix "nwis_" and the USGS National Water Information System (NWIS) site ID.', NA,
    'resolution', 'The temporal resolution of the input data (time between successive observations) for those dates fitted by this model.', 'minutes',
    'date', 'Primary date to which the fitted values apply, Y-M-D format, for the period from 4am on that date to 3:59am on the following date.', NA,
    'GPP', 'Model estimate of mean gross primary productivity (GPP) for this date. Value is the median of the MCMC distribution.', vsunits('GPP.daily'),
    'GPP.lower', 'Lower bound on the 95% credible interval around the daily GPP estimate. Value is the 2.5th quantile of the MCMC distribution.', vsunits('GPP.daily'),
    'GPP.upper', 'Upper bound on the 95% credible interval around the daily GPP estimate. Value is the 97.5th quantile of the MCMC distribution.', vsunits('GPP.daily'),
    'GPP.n_eff', 'Effective sample size of the MCMC sampling for GPP.', 'samples',
    'GPP.Rhat', 'R-hat statistic of the MCMC sampling for GPP. Values near or below 1.05 indicate convergence of the MCMC chains.', NA,
    'ER', 'Model estimate of mean ecosystem respiration (ER) for this date, where more negative values indicate more respiration. Value is the median of the MCMC distribution.', vsunits('ER.daily'),
    'ER.lower', 'Lower bound (most negative or least positive) on the 95% credible interval around the daily ER estimate. Value is the 2.5th quantile of the MCMC distribution.', vsunits('ER.daily'),
    'ER.upper', 'Upper bound (least negative or most positive) on the 95% credible interval around the daily ER estimate. Value is the 97.5th quantile of the MCMC distribution.', vsunits('ER.daily'),
    'ER.n_eff', 'Effective sample size of the MCMC sampling for ER.', 'samples',
    'ER.Rhat', 'R-hat statistic of the MCMC sampling for ER. Values near or below 1.05 indicate convergence of the MCMC chains.', NA,
    'K600', 'Model estimate of mean reaeration rate coefficient for this date. Value is the median of the MCMC distribution.', vsunits('K600.daily'),
    'K600.lower', 'Lower bound on the 95% credible interval around the daily K600 estimate. Value is the 2.5th quantile of the MCMC distribution.', vsunits('K600.daily'),
    'K600.upper', 'Upper bound on the 95% credible interval around the daily K600 estimate. Value is the 97.5th quantile of the MCMC distribution.', vsunits('K600.daily'),
    'K600.n_eff', 'Effective sample size of the MCMC sampling for K600.', 'samples',
    'K600.Rhat', 'R-hat statistic of the MCMC sampling for K600. Values near or below 1.05 indicate convergence of the MCMC chains.', vsunits('K600.daily'),
    'DO.obs', 'Mean dissolved oxygen concentration for the date (4am to 3:59am).', vsunits('DO.obs'),
    'DO.sat', 'Mean theoretical saturation concentration for the date (4am to 3:59am).',vsunits('DO.sat'),
    'DO.amp', 'Amplitude (difference between minimum and maximum observed values) of the dissolved oxygen concentrations for the date (4am to 3:59am).', vsunits('DO.obs'),
    'DO.psat', 'Mean percent dissolved oxygen saturation for the date (4am to 3:59am).', vsunits('DO.psat'),
    'depth', 'Mean depth, averaged over the reach length and width, for the date (4am to 3:59pm).', vsunits('depth'),
    'temp.water', 'Mean water temperature for the date (4am to 3:59pm).', vsunits('temp.water'),
    'day.length', 'Time elapsed between first and last observations of light > 0 for the date (4am to 3:59pm).', 'hours',
    'light', 'Mean photosynthetic photon flux density (PPFD) for the date (4am to 3:59pm).', vsunits('light'),
    'discharge', 'Mean discharge for the date (4am to 3:59pm).', vsunits('discharge'),
    'velocity', 'Mean water velocity for the date (4am to 3:59pm).', vsunits('velocity')
  ) %>% mutate(
    'attr-defs'=sapply(`attr-label`, function(attr_label) {
      switch(
        attr_label,
        site_name = 'National Water Information System, U.S. Geological Survey',
        resolution=,DO.obs=,DO.sat=,DO.amp=,DO.psat=,depth=,temp.water=,day.length=,light=,discharge=,velocity='This data release',
        'streamMetabolizer R package')
    })
  )
  
  # combine the skeleton, ranges, and definitions
  attr_df_combined <- attr_df %>%
    select(`attr-label`) %>%
    left_join(select(ranges_df, `attr-label`, `data-min`, `data-max`), by='attr-label') %>%
    left_join(defs_df, by='attr-label') %>%
    select(names(attr_df))
  
  # write the final attribute table
  readr::write_csv(attr_df_combined, path=attr.file)
}

metadata_daily_preds <- function(out_file, preds_yaml, points_list, attrs_csv, parent_list, template) {
  preds_list <- yaml::yaml.load_file(preds_yaml)
  attr_list <- as.attr_list(attrs_csv)
  render(filename=out_file, data=preds_list, points_list, attr_list, parent_list, template=fgdc_template)
}

#### helpers ####

format_bound <- function(bound) {
  if(is(bound, 'Date')) {
    format(bound, format='%Y-%m-%d')
  } else if(is(bound, 'POSIX')) {
    fomrmat(bound, format='%Y-%m-%d %H:%M:%S %z')
  } else if(is(bound, 'character')) {
    bound
  } else if(is(bound, 'numeric')) {
    format(bound, digits=4, scientific=FALSE)
  }
}
