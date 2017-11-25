attributes_metab_config <- function(
  zip.file='../4_data_release/cache/models/post/config.zip',
  attr.file='../4_data_release/in/attr_metab_config.csv') {
  
  # read in the data file
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'config'))
  data_df <- readr::read_tsv(unzipped)
  
  # sketch out the skeleton attribute table
  attr.temp <- tempfile(fileext='.csv')
  attribute_skeleton(data_df, attr.temp)
  attr_df <- readr::read_csv(attr.temp, col_types = 'cccnnc')
  
  # compute data-min and data-max (plus other diagnostics for intermediate use)
  ranges_df <- compute_ranges(data_df)
  
  # fill out the attribute table
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`, ~`data-units`,
    'site', 'Site identifier, consisting of prefix "nwis_" and the USGS National Water Information System (NWIS) site ID.', NA,
    'resolution', paste(
      'The temporal resolution of the input data (time between successive observations) for those dates fitted by this model.',
      'Expressed as XXmin, where XX is the number of minutes between observations and "min" indicates the units.',
      'This configuration file can be cross-referenced to the model fit zip files in this data release by concatenating',
      '"site" and "resolution" with an underscore and appending "_fit.zip", e.g., "nwis_01473500_30min_fit.zip".'), 'minutes',
    'required_timestep', paste(
      'Duration of the expected timestep in days. This argument is passed to streamMetabolizer along with the complete input dataset;',
      'streamMetabolizer subsets the input data to only those observations that conform to the specified temporal resolution.',
      'Some sites have inconsistent temporal resolution over their periods of record, so dates with differing resolution were modeled separately',
      'and two or more models are possible for a single site.'), 'days',
    'model_name', paste(
      'The coded model name used to identify a unique model structure in the streamMetabolizer R package.',
      'Every model used b_Kb_oipi_tr_plrckm.stan, a Bayesian (b) state-space model (oipi) with hierarchical partial pooling of gas exchange (Kb),',
      'trapezoid-rule numerical integration (tr), a linear relationship between productivity and light on each day (pl),',
      'constant respiration over each 24-hour period (rc), and gas exchange at each timestep computed from the modeled oxygen at the previous timestep (km).'), NA,
    'K600_lnQ_nodes_centers', paste(
      'R code to generate a sequence of values that span the range of natural logs of discharge at the site, at intervals of 0.2 natural log units.',
      'For "seq(a,b,by=0.2)", the generated sequence runs from a minimum of "a" to a maximum of "b".',
      'The piecewise linear hierarchical relationship between the natural logs of K600 (gas exchange rate coefficient) and Q (discharge)',
      'used these values as the x-axis endpoints of the line segments. The corresponding y values for K600 were fitted by the models.'), 'natural log of discharge where discharge is m^3 s^-1',
    'K600_lnQ_nodediffs_sdlog', paste(
      'Prior expectation of the standard deviation of the differences in estimated natural-log-K600 between adjacent line segment endpoints,',
      'where the means of those differences are always zero.'), NA,
    'K600_daily_sigma_sigma', paste(
      'Hyperparameter describing the expected deviation of daily K600 estimates (K600_daily)',
      'from the value predicted by the piecewise linear relationship for that day (K600_daily_piecewise).',
      'The parameter described by this hyperparameter is K600_daily_sigma, which is used as K600_daily ~ normal(mu=K600_daily_piecewise, sigma=K600_daily_sigma).',
      'This hyperparameter is the scale (= sigma) parameter of a half-normal prior distribution of K600_daily_sigma',
      'such that K600_daily_sigma ~ halfnormal(mu=0, sigma=K600_daily_sigma_sigma). The value of K600_daily_sigma_sigma for each model was selected',
      'based on a round of preliminary maximum-likelihood estimates of K600 at each site and is equal to 2 percent of the median of those estimates.'), NA,
    'burnin_steps', 'The number of steps (iterations) per MCMC chain that were executed and ignored before starting to track the values at each iteration. 4 chains were used per model.', 'MCMC iterations',
    'saved_steps', 'The number of MCMC steps (iterations) per chain that were tracked following the burn-in phase. 4 chains were used per model.', 'MCMC iterations',
    'solar.time.src', paste(
      'The computation method used to translate observation timestamps from clock time to solar mean time for passing to the metabolism estimation function.',
      'The "calcLon" algorithm (used for all models) corresponds to the "calc_solar_time" function in streamMetabolizer.'), NA,
    'DO.obs.src', paste(
      'The data source for the dissolved oxygen concentration observations used to model metabolism.',
      '"nwis" (used for all models) indicates that data were acquired from the USGS National Water Information System, https://waterdata.usgs.gov/nwis.'), NA,
    'DO.sat.src', paste(
      'The data sources and computation method used to estimate the theoretical saturation concentration of dissolved oxygen. The data sources were always',
      'the water temperature from the USGS National Water Information System, https://waterdata.usgs.gov/nwis, and the barometric pressure',
      'from the NASA National Land Data Assimilation System, https://ldas.gsfc.nasa.gov/nldas.',
      '"calcGGbts" (used for all models) indicates that these data sources were combined using the streamMetabolizer "calc_DO_sat" function,',
      'which applies the Garcia-Benson equation (Garcia and Gordon 1992).'), NA,
    'depth.src', paste(
      'The data sources and computation method used to estimate water depth for use in modeling metabolism.',
      'Depth estimates always began with discharge data from USGS National Water Information System, https://waterdata.usgs.gov/nwis.',
      'One of two calculation methods was used for each model. If regionalized hydraulic geometry coefficients were available for the site',
      '(see the "Site identifiers and details" table in this data release), then the "calcDischHarvey" algorithm applied those coefficients to the discharge data',
      'using the formula z=cQ^f where z is depth (m), c (m) and f (unitless) are the supplied coefficients, and Q is discharge (m^3 s^-1).',
      'If regionalized coefficients were unavailable for the site,',
      'then the "calcDischRaymond" algorithm was applied via the "calc_depth" streamMetabolizer function, which fixes the coefficients in the above equation',
      'at c=0.409 and f=0.294 according to a global regression by Raymond et al. 2012.'), NA,
    'temp.water.src', paste(
      'The data source for the water temperature observations used in modeling metabolism.',
      '"nwis" (used for all models) indicates that data were acquired from the USGS National Water Information System, https://waterdata.usgs.gov/nwis.'), NA,
    'light.src', paste(
      'The data sources and computation method used to estimate photosynthetically active radiation for use in modeling metabolism.',
      'The "calcLatSw" algorithm (used for all models) corresponds to the "calc_light_merged" function in streamMetabolizer, which was used to merge',
      'theoretical light curves (computed by streamMetabolizer\'s "calc_light" function) with estimates of shortwave radiation from the',
      'NASA National Land Data Assimilation System, https://ldas.gsfc.nasa.gov/nldas.'), NA,
    'discharge.src', paste(
      'The data source for the river discharge observations used in modeling metabolism.',
      '"nwis" (used for all models) indicates that data were acquired from the USGS National Water Information System, https://waterdata.usgs.gov/nwis'), NA
  ) %>% mutate(
    'attr-defs'=sapply(`attr-label`, function(attr_label) {
      switch(
        attr_label,
        site = 'National Water Information System, U.S. Geological Survey',
        resolution=,solar.time.src=,DO.obs.src=,DO.sat.src=,depth.src=,temp.water.src=,light.src=,discharge.src='This data release',
        required_timestep=,model_name=,K600_lnQ_nodes_centers=,K600_lnQ_nodediffs_sdlog=,K600_daily_sigma_sigma=,burnin_steps=,saved_steps='streamMetabolizer R package',
        NA)
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
  

attributes_metab_inputs <- function(
  zip.dir='../4_data_release/cache/models/post',
  attr.file='../4_data_release/in/attr_metab_inputs.csv') {
  
  # get a list of all relevant data files
  input_files <- dir(zip.dir, pattern='_input.zip$', full.names=TRUE)
  
  # read in a single example data file for structure
  zip.file <- input_files[1]
  unzipped1 <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'config'))
  data_df <- readr::read_tsv(unzipped1, col_types = 'Tdddddd')
  
  # unzip them all, then compute collective ranges
  unzipped <- unlist(lapply(input_files, unzip, exdir=file.path(tempdir(), 'inputs')))
  ranges_df <- multifile_ranges(unzipped, coltypes='Tdddddd')
  
  # # do some data checks. some sites have some bad data, mostly not too bad  
  # nrow(filter(ranges_dfs, DO.obs_min < 0)) # 5 sites with negative DO.obs, though seldom hugely negative
  # nrow(filter(ranges_dfs, DO.sat_min < 0)) # 0 sites go negative
  # nrow(filter(ranges_dfs, depth_min < 0)) # 0 sites go negative
  # nrow(filter(ranges_dfs, temp.water_min < -1)) # nwis_01646500 has at least one -999999
  # nrow(filter(ranges_dfs, light_min < 0)) # 0 sites go negative
  # nrow(filter(ranges_dfs, discharge_min < 0)) # 32 sites go negative
  
  # sketch out the skeleton attribute table
  attr.temp <- tempfile(fileext='.csv')
  attribute_skeleton(data_df, attr.temp)
  attr_df <- readr::read_csv(attr.temp, col_types = 'cccnnc')
  
  # fill out the attribute table
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`,
    'solar.time', 'Mean solar time, which puts the solar zenith at almost exactly noon but also preserves equal timesteps within and across days. Timezone is nominally UTC but is meaningless.',
    'DO.obs', 'Dissolved oxygen concentration.',
    'DO.sat', 'Hypothetical dissolved oxygen concentration at saturation.',
    'depth', 'River depth, treated as the average over the full length and width of the upstream reach that affects dissolved oxygen concentrations.',
    'temp.water', 'Water temperature.',
    'light', 'Photosynthetically active radiation (photon flux density).',
    'discharge', 'River discharge.'
  ) %>%
    mutate(
      'data-units'=sapply(`attr-label`, var_src_units) %>% ifelse(.=='', NA, .),
      'attr-defs'='streamMetabolizer R package')
  
  # combine the skeleton, ranges, and definitions
  attr_df_combined <- attr_df %>%
    select(`attr-label`) %>%
    left_join(select(ranges_df, `attr-label`, `data-min`, `data-max`), by='attr-label') %>%
    left_join(defs_df, by='attr-label') %>%
    select(names(attr_df))
  
  # write the final attribute table
  readr::write_csv(attr_df_combined, path=attr.file)
}

attributes_metab_fits <- function(
  ent.file='../4_data_release/in/attr_metab_fits.rds',
  zip.dir='../4_data_release/cache/models/post',
  attr.file.base='../4_data_release/in/attr_metab_fits.csv') {
  
  # get a list of all relevant data files
  zip_files <- dir(zip.dir, pattern='_[[:digit:]]+min_fit.zip$', full.names=TRUE)
  
  # read in a single example data file of each type for structure
  zip.file <- zip_files[1]
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'config'))
  data_dfs <- lapply(setNames(unzipped, basename(unzipped)), function(unzfile) {
    if(basename(unzfile) != 'warnings.txt') {
      readr::read_tsv(unzfile)
    } else {
      data_frame(text=readLines(unzfile))
    }})
  
  # unzip all the files and compute ranges for each column of the many files of
  # each entity type (ftype)
  unzipped_all <- unlist(lapply(zip_files, function(zf) {
    unzip(zipfile=zf, exdir=file.path(tempdir(), 'fits', tools::file_path_sans_ext(basename(zf))), overwrite=FALSE)
  }))
  ranges_dfs <- lapply(setNames(nm=names(data_dfs)), function(ftype) {
    filesoftype <- grep(paste0("/", ftype, "$"), unzipped_all, value=TRUE)
    ranges_df <- if(ftype != 'warnings.txt') {
      coltypes <- paste(unname(c(character='c', integer='i', numeric='d', Date='D', logical='l')[sapply(data_dfs[[ftype]], class)]), collapse='')
      multifile_ranges(filesoftype, coltypes=coltypes)
    } else {
      unique(unlist(lapply(filesoftype, readLines)))
    }
  })

  # define variables for definitions text that will be used more than once
  distrib <- 'of the post-warmup MCMC distribution of'
  n_eff <- 'Estimated effective sample size of the MCMC sampling for'
  Rhat <- c('R-hat statistic of the MCMC sampling for', 'Values near or below 1.05 indicate convergence of the MCMC chains.')
  dateind <- 'Integer index of a 24-hour period from 4am to the following 3:59am, modeled as a single date.'
  timeind <- 'Integer index of a time within a date, e.g., observations 15-minute resolution are given time_index values of 1 (4am) through 96 (3:45am nearly 24 hours later).'
  
  # create one attr file per entity type
  all_attr_dfs <- lapply(setNames(nm=names(ranges_dfs)), function(ftype) {
    one.attr.file <- gsub('fits', paste0('fits_', tools::file_path_sans_ext(ftype)), attr.file.base)
    ranges_df <- if(ftype != 'warnings.txt') {
      ranges_dfs[[ftype]]
    } else {
      data_frame(
        `attr-label`='text',
        `data-min`=NA,
        `data-max`=NA
      )
    }
    
    # prepare the basic attr_df skeleton as a data_frame
    if(file.exists(one.attr.file)) file.remove(one.attr.file)
    attribute_skeleton(data_dfs[[ftype]], one.attr.file)
    attr_df <- readr::read_csv(one.attr.file, col_types = 'cccnnc')
    
    # write definitions for each column
    defs_df <- switch(
      ftype,
      'daily.tsv'={
        gpp <- 'the GPP_daily parameter, where GPP_daily is the mean gross primary productivity (GPP) for this date'
        er <- 'the ER_daily parameter, where ER_daily is the mean ecosystem respiration (ER) for this date, and more negative values indicate more respiration'
        K600 <- 'the K600_daily parameter, where K600_daily is the mean reaeration rate coefficient, scaled to a Schmidt number of 600, for this date'
        K600pl <- 'the K600_daily_predlog parameter, giving the hierarchical estimate for any date with this date\'s mean daily discharge, in natural log space'
        units_K600pl <- sprintf('ln(%s)', var_src_units('K600.daily'))
        tibble::tribble(
          ~`attr-label`, ~`attr-def`, ~`data-units`,
          'date', 'Primary date to which the fitted values apply, Y-M-D format, for the period from 4am on that date to 3:59am on the following date.', NA,
          'GPP_daily_mean', sprintf('Mean %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_se_mean', sprintf('Standard error of the mean %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_sd', sprintf('Standard deviation %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_25pct', sprintf('The 25th quantile %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_50pct', sprintf('The 50th quantile %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_75pct', sprintf('The 75th quantile %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, gpp), var_src_units('GPP.daily'),
          'GPP_daily_n_eff', sprintf('%s %s.', n_eff, gpp), 'samples',
          'GPP_daily_Rhat', sprintf('%s %s. %s', Rhat[1], gpp, Rhat[2]), NA,
          'ER_daily_mean', sprintf('Mean %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_se_mean', sprintf('Standard error of the mean %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_sd', sprintf('Standard deviation %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_25pct', sprintf('The 25th quantile %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_50pct', sprintf('The 50th quantile %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_75pct', sprintf('The 75th quantile %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, er), var_src_units('ER.daily'),
          'ER_daily_n_eff', sprintf('%s %s.', n_eff, er), 'samples',
          'ER_daily_Rhat', sprintf('%s %s. %s', Rhat[1], er, Rhat[2]), NA,
          'K600_daily_mean', sprintf('Mean %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_se_mean', sprintf('Standard error of the mean %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_sd', sprintf('Standard deviation %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_25pct', sprintf('The 25th quantile %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_50pct', sprintf('The 50th quantile %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_75pct', sprintf('The 75th quantile %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, K600), var_src_units('K600.daily'),
          'K600_daily_n_eff', sprintf('%s %s.', n_eff, K600), 'samples',
          'K600_daily_Rhat', sprintf('%s %s. %s', Rhat[1], K600, Rhat[2]), NA,
          'K600_daily_predlog_mean', sprintf('Mean %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_se_mean', sprintf('Standard error of the mean %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_sd', sprintf('Standard deviation %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_25pct', sprintf('The 25th quantile %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_50pct', sprintf('The 50th quantile %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_75pct', sprintf('The 75th quantile %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, K600pl), units_K600pl,
          'K600_daily_predlog_n_eff', sprintf('%s %s.', n_eff, K600pl), 'samples',
          'K600_daily_predlog_Rhat', sprintf('%s %s. %s', Rhat[1], K600pl, Rhat[2]), NA,
          'valid_day', 'TRUE if the input data for this date were considered valid and included in the model, FALSE otherwise', NA,
          'warnings', 'Date-specific warnings about input data', NA,
          'errors', 'Date-specific problems with input data that prevented model fitting on that date and drove the setting of valid_day to FALSE', NA
        ) %>% mutate(
          'attr-defs'='streamMetabolizer R package'
        )
        
      },
      'KQ_overall.tsv'={
        K600sig <- 'the K600_daily_sigma parameter, giving the fitted estimate of the standard deviation of K600_daily values relative to the exp(K600_daily_predlog) values on the same dates'
        tibble::tribble(
          ~`attr-label`, ~`attr-def`, ~`data-units`,
          'date_index', sprintf('Always NA in KQ_overall.tsv. %s', dateind), NA,
          'time_index', sprintf('Always NA in KQ_overall.tsv. %s', timeind), NA,
          'index', 'Always 1 in KQ_overall.tsv. Integer index of the parameters described in later columns.', NA,
          'K600_daily_sigma_mean', sprintf('Mean %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_se_mean', sprintf('Standard error of the mean %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_sd', sprintf('Standard deviation %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_25pct', sprintf('The 25th quantile %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_50pct', sprintf('The 50th quantile %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_75pct', sprintf('The 75th quantile %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, K600sig), var_src_units('K600.daily'),
          'K600_daily_sigma_n_eff', sprintf('%s %s.', n_eff, K600sig), 'samples',
          'K600_daily_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], K600sig, Rhat[2]), NA
        ) %>% mutate(
          'attr-defs'='streamMetabolizer R package'
        )
        
      },
      'overall.tsv'={
        OIsig <- 'the err_obs_iid_sigma parameter, giving the fitted standard deviation of observation errors (differences between observed and modeled oxygen concentrations)'
        units_OIsig <- var_src_units('DO.obs')
        PIsig <- 'the err_proc_iid_sigma parameter, giving the fitted standard deviation of process errors (differences between rates of oxygen concentration change as modeled by the overall state-space model and the deterministic component of the model)'
        units_PIsig <- var_src_units('GPP.daily')
        lp <- 'log posterior density for each MCMC iteration'
        units_lp <- NA
        tibble::tribble(
          ~`attr-label`, ~`attr-def`, ~`data-units`,
          'date_index', sprintf('Always NA in overall.tsv. %s', dateind), NA,
          'time_index', sprintf('Always NA in overall.tsv. %s', timeind), NA,
          'index', 'Always 1 in overall.tsv. Integer index of the parameters described in later columns.', NA,
          'err_obs_iid_sigma_mean', sprintf('Mean %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_se_mean', sprintf('Standard error of the mean %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_sd', sprintf('Standard deviation %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_25pct', sprintf('The 25th quantile %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_50pct', sprintf('The 50th quantile %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_75pct', sprintf('The 75th quantile %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, OIsig), units_OIsig,
          'err_obs_iid_sigma_n_eff', sprintf('%s %s.', n_eff, OIsig), 'samples',
          'err_obs_iid_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], OIsig, Rhat[2]), NA,
          'err_proc_iid_sigma_mean', sprintf('Mean %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_se_mean', sprintf('Standard error of the mean %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_sd', sprintf('Standard deviation %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_25pct', sprintf('The 25th quantile %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_50pct', sprintf('The 50th quantile %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_75pct', sprintf('The 75th quantile %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, PIsig), units_PIsig,
          'err_proc_iid_sigma_n_eff', sprintf('%s %s.', n_eff, PIsig), 'samples',
          'err_proc_iid_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], PIsig, Rhat[2]), NA,
          'lp___mean', sprintf('Mean %s %s.', distrib, lp), units_lp,
          'lp___se_mean', sprintf('Standard error of the mean %s %s.', distrib, lp), units_lp,
          'lp___sd', sprintf('Standard deviation %s %s.', distrib, lp), units_lp,
          'lp___2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, lp), units_lp,
          'lp___25pct', sprintf('The 25th quantile %s %s.', distrib, lp), units_lp,
          'lp___50pct', sprintf('The 50th quantile %s %s.', distrib, lp), units_lp,
          'lp___75pct', sprintf('The 75th quantile %s %s.', distrib, lp), units_lp,
          'lp___97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, lp), units_lp,
          'lp___n_eff', sprintf('%s %s.', n_eff, lp), 'samples',
          'lp___Rhat', sprintf('%s %s. %s', Rhat[1], lp, Rhat[2]), NA
        ) %>% mutate(
          'attr-defs'='streamMetabolizer and Stan R packages'
        )
        
      },
      'KQ_binned.tsv'={
        kqn <- paste(
          'one of the lnK600_lnQ_nodes parameters describing a site-specific, piecewise linear relationship between ln(K600) and ln(Q).',
          'Each indexed parameter is the fitted ln(K600) value corresponding to a fixed value of ln(Q).',
          'The values of ln(Q) for each model are defined in the K600_lnQ_nodes_centers column of config.zip.')
        units_kqn <- sprintf('ln(%s)', var_src_units('K600.daily'))
        tibble::tribble(
          ~`attr-label`, ~`attr-def`, ~`data-units`,
          'date_index', sprintf('Always NA in KQ_binned.tsv. %s', dateind), NA,
          'time_index', sprintf('Always NA in KQ_binned.tsv. %s', timeind), NA,
          'index', 'Integer index of the lnK600_lnQ_nodes parameter described in later columns, where each indexed value of lnK600_lnQ_nodes gives a junction point of the fitted piecewise linear relationship between K600 and discharge (Q).', NA,
          'lnK600_lnQ_nodes_mean', sprintf('Mean %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_se_mean', sprintf('Standard error of the mean %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_sd', sprintf('Standard deviation %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_2.5pct', sprintf('The 2.5th quantile %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_25pct', sprintf('The 25th quantile %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_50pct', sprintf('The 50th quantile %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_75pct', sprintf('The 75th quantile %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_97.5pct', sprintf('The 97.5th quantile %s %s.', distrib, kqn), units_kqn,
          'lnK600_lnQ_nodes_n_eff', sprintf('%s %s.', n_eff, kqn), 'samples',
          'lnK600_lnQ_nodes_Rhat', sprintf('%s %s. %s', Rhat[1], kqn, Rhat[2]), NA
        ) %>% mutate(
          'attr-defs'='streamMetabolizer R package'
        )
        
      },
      'warnings.txt'={
        data_frame(
          `attr-label`='text',
          `attr-def`='Warnings produced for this model run (all dates) by the Stan MCMC software.',
          `attr-defs`='streamMetabolizer and Stan R packages',
          `data-units`=NA
        )
        
      }
    )
    
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
      'data-name'=ent_name,
      'data-description'=paste(switch(
        ent_name, 
        'daily.tsv'=c(
          'MCMC distribution statistics for daily model estimates of mean gross primary production (GPP), ecosystem respiration (ER), and the gas exchange rate coefficient (K600).'),
        'KQ_overall.tsv'=c(
          'MCMC distribution statistics for K600_daily_sigma, a parameter relating specific daily estimates of the gas exchange rate coefficient (K600)',
          'to predictions from the piecewise linear relationship between K600 and discharge (Q) fitted for each monitoring site.'),
        'overall.tsv'=c(
          'MCMC distribution statistics for the fitted standard deviations of observation errors and process errors and',
          'the overall log posterior probability of all parameter values in each MCMC iteration.'
        ),
        'KQ_binned.tsv'=c(
          'MCMC distribution statistics for parameters describing the ln(K600) values of the endpoints and breakpoints of the piecewise linear relationship',
          'between K600 and Q fitted for each monitoring site.'
        ),
        'warnings.txt'=c(
          'Warnings given by the Stan software for each MCMC model run.'
        )
      ), collapse=' ')),
      as.attr_list(attr_df)
    )
  }))
  saveRDS(ent_list, ent.file)
}

attributes_metab_diagnostics <- function(
  zip.file='../4_data_release/cache/models/post/diagnostics.zip',
  attr.file='../4_data_release/in/attr_metab_diagnostics.csv') {
  
  # read in the data file
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'diagnostics'))
  data_df <- readr::read_tsv(unzipped)
  
  # sketch out the skeleton attribute table
  attr.temp <- tempfile(fileext='.csv')
  attribute_skeleton(data_df, attr.temp)
  attr_df <- readr::read_csv(attr.temp, col_types = 'cccnnc')
  
  # compute data-min and data-max (plus other diagnostics for intermediate use)
  ranges_df <- compute_ranges(data_df)
  
  # fill out the attribute table
  Rhat <- c('R-hat statistic of the MCMC sampling for', 'Values near or below 1.05 indicate convergence of the MCMC chains.')
  K600sig <- 'the K600_daily_sigma parameter, giving the fitted estimate of the standard deviation of K600_daily values relative to the exp(K600_daily_predlog) values on the same dates'
  OIsig <- 'the err_obs_iid_sigma parameter, giving the fitted standard deviation of observation errors (differences between observed and modeled oxygen concentrations)'
  PIsig <- 'the err_proc_iid_sigma parameter, giving the fitted standard deviation of process errors (differences between rates of oxygen concentration change as modeled by the overall state-space model and the deterministic component of the model)'
  K600 <- 'the K600_daily parameter, where K600_daily is the mean reaeration rate coefficient, scaled to a Schmidt number of 600'
  
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`, ~`data-units`,
    'site', 'Site identifier, consisting of prefix "nwis_" and the USGS National Water Information System (NWIS) site ID.', NA,
    'resolution', 'The temporal resolution of the input data (time between successive observations) for those dates fitted by this model.', 'minutes',
    'K600_daily_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], K600sig, Rhat[2]), NA,
    'err_obs_iid_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], OIsig, Rhat[2]), NA,
    'err_proc_iid_sigma_Rhat', sprintf('%s %s. %s', Rhat[1], PIsig, Rhat[2]), NA,
    'K_median', sprintf('Median of the daily estimates of %s.', K600), var_src_units('K600.daily'),
    'K_range', sprintf('Difference between the 90th and 10th quantiles of the daily estimates of %s.', K600), var_src_units('K600.daily'),
    'neg_GPP', 'Percent of daily estimates of gross primary productive (GPP) that are negative (unrealistic).', 'percent',
    'pos_ER', 'Percent of daily estimates of ecosystem respiration (ER) that are positive (unrealistic).', 'percent',
    'model_confidence', 'Assessment of overall confidence in the model based on other diagnostics in this table, scored as "L" (low confidence), "M" (medium), or "H" (high).', NA,
    'site_min_confidence', 'Least-confident assessment rating of all models for this site, scored as "L" (low confidence), "M" (medium), or "H" (high).', NA,
    'site_confidence', 'All unique assessment ratings of models for this site, scored as "L" (low confidence), "M" (medium), or "H" (high), and comma-separated when multiple unique ratings exist.', NA
  ) %>%
    mutate(
      'attr-defs'='streamMetabolizer R package')
  
  # combine the skeleton, ranges, and definitions
  attr_df_combined <- attr_df %>%
    select(`attr-label`) %>%
    left_join(select(ranges_df, `attr-label`, `data-min`, `data-max`), by='attr-label') %>%
    left_join(defs_df, by='attr-label') %>%
    select(names(attr_df))
  
  # write the final attribute table
  readr::write_csv(attr_df_combined, path=attr.file)
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
  ranges_df <- compute_ranges(dailies)
  
  # write in / look up definitions & units
  est <- 'Model estimate of %s. Value is the median of the MCMC distribution.'
  CI <- '%s bound %son the 95%% credible interval around the daily %s estimate. Value is the %sth quantile of the post-warmup MCMC distribution.'
  n_eff <- 'Estimated effective sample size of the MCMC sampling for %s.'
  Rhat <- 'R-hat statistic of the MCMC sampling for %s. Values near or below 1.05 indicate convergence of the MCMC chains.'
  gpp <- 'GPP, the mean rate of gross primary productivity for this date'
  er <- 'ER, the mean rate of ecosystem respiration for this date, where more negative values indicate more respiration'
  K600 <- 'K600, the mean reaeration rate coefficient, scaled to a Schmidt number of 600, for this date'
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`, ~`data-units`,
    'site_name', 'Site identifier, consisting of prefix "nwis_" and the USGS National Water Information System (NWIS) site ID.', NA,
    'resolution', 'The temporal resolution of the input data (time between successive observations) for those dates fitted by this model.', 'minutes',
    'date', 'Primary date to which the fitted values apply, Y-M-D format, for the period from 4am on that date to 3:59am on the following date.', NA,
    'GPP', sprintf(est, gpp), var_src_units('GPP.daily'),
    'GPP.lower', sprintf(CI, 'Lower', '', 'GPP', '2.5'), var_src_units('GPP.daily'),
    'GPP.upper', sprintf(CI, 'Upper', '', 'GPP', '97.5'), var_src_units('GPP.daily'),
    'GPP.n_eff', sprintf(n_eff, 'GPP'), 'samples',
    'GPP.Rhat', sprintf(Rhat, 'GPP'), NA,
    'ER', sprintf(est, er), var_src_units('ER.daily'),
    'ER.lower', sprintf(CI, 'Lower', '(most negative or least positive) ', 'ER', '2.5'), var_src_units('ER.daily'),
    'ER.upper', sprintf(CI, 'Upper', '(least negative or most positive) ', 'ER', '97.5'), var_src_units('ER.daily'),
    'ER.n_eff', sprintf(n_eff, 'ER'), 'samples',
    'ER.Rhat', sprintf(Rhat, 'ER'), NA,
    'K600', sprintf(est, K600), var_src_units('K600.daily'),
    'K600.lower', sprintf(CI, 'Lower', '', 'K600', '2.5'), var_src_units('K600.daily'),
    'K600.upper', sprintf(CI, 'Upper', '', 'K600', '97.5'), var_src_units('K600.daily'),
    'K600.n_eff', sprintf(n_eff, 'K600'), 'samples',
    'K600.Rhat', sprintf(Rhat, 'K600'), NA,
    'DO.obs', 'Mean dissolved oxygen concentration for the date (4am to 3:59am).', var_src_units('DO.obs'),
    'DO.sat', 'Mean theoretical saturation concentration for the date (4am to 3:59am).',var_src_units('DO.sat'),
    'DO.amp', 'Amplitude (difference between minimum and maximum observed values) of the dissolved oxygen concentrations for the date (4am to 3:59am).', var_src_units('DO.obs'),
    'DO.psat', 'Mean percent dissolved oxygen saturation for the date (4am to 3:59am).', var_src_units('DO.psat'),
    'depth', 'Mean depth, averaged over the reach length and width, for the date (4am to 3:59pm).', var_src_units('depth'),
    'temp.water', 'Mean water temperature for the date (4am to 3:59pm).', var_src_units('temp.water'),
    'day.length', 'Time elapsed between first and last observations of light > 0 for the date (4am to 3:59pm).', 'hours',
    'light', 'Mean photosynthetic photon flux density (PPFD) for the date (4am to 3:59pm).', var_src_units('light'),
    'discharge', 'Mean discharge for the date (4am to 3:59pm).', var_src_units('discharge'),
    'velocity', 'Mean water velocity for the date (4am to 3:59pm).', var_src_units('velocity')
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

#### renderer ####

render_metab_metadata <- function(out_file, child_yaml, points_list, attrs_csv, parent_list, template) {
  child_list <- yaml::yaml.load_file(child_yaml)
  attr_list <- as.attr_list(attrs_csv)
  render(filename=out_file, data=child_list, points_list, attr_list, parent_list, template=template)
}

render_metab_fit_metadata <- function(out_file, child_yaml, points_list, ent_rds, parent_list, template) {
  child_list <- yaml::yaml.load_file(child_yaml)
  ent_list <- readRDS(ent_rds)
  render(filename=out_file, data=child_list, points_list, ent_list, parent_list, template=template)
}

#### helpers ####

# look up a metab units from the mda.streams var_src_codes
var_src_units <- function(variable) {
  unique(mda.streams::get_var_src_codes(metab_var==variable, out='metab_units'))
}

# compute data-min and data-max (plus other diagnostics for intermediate use)
compute_ranges <- function(data_df) {
  ranges_df <- bind_rows(
    summarise_all(data_df, .funs=funs(format_bound(min(., na.rm=TRUE)))) %>% mutate(stat='data-min'),
    summarise_all(data_df, .funs=funs(format_bound(max(., na.rm=TRUE)))) %>% mutate(stat='data-max'),
    summarise_all(data_df, .funs=funs(format_bound(length(which(is.na(.)))))) %>% mutate(stat='num-NA')
  ) %>%
    gather(`attr-label`, value, -stat) %>%
    spread(stat, value)
  return(ranges_df)
}

multifile_ranges <- function(files, coltypes='Tdddddd') {
  ranges_dfs <- bind_rows(lapply(seq_along(files), function(i) {
    unz.file <- files[i]
    message(paste0(i, '\t', unz.file))
    data_df <- readr::read_tsv(unz.file, col_types = coltypes)
    
    ranges_1df <- summarise_all(data_df, .funs=funs(
      min=if(any(!is.na(.))) min(., na.rm=TRUE) else NA,
      max=if(any(!is.na(.))) max(., na.rm=TRUE) else NA,
      numNA = length(which(is.na(.))),
      numTot = length(.))) %>% 
      mutate(file = basename(unz.file))
    return(ranges_1df)
  }))
  # reduce the df of 1 row per site to 1 row for all sites (min, max, etc. in different columns)
  ranges_df_wide <- as.data.frame(lapply(setNames(nm=setdiff(names(ranges_dfs), 'file')), function(colname) {
    range_col <- ranges_dfs[[colname]]
    type <- strsplit(colname, '_')[[1]] %>% .[length(.)]
    switch(
      type,
      min=format_bound(if(any(!is.na(range_col))) min(range_col, na.rm=TRUE) else NA),
      max=format_bound(if(any(!is.na(range_col))) max(range_col, na.rm=TRUE) else NA),
      numNA=format_bound(sum(range_col)),
      numTot=format_bound(sum(range_col))
    )
  }), stringsAsFactors=FALSE)
  # merge min, max into their own columns, 1 row per variable
  ranges_df <- full_join(
    full_join(transmute(gather(ranges_df_wide, var_stat, `data-min`, ends_with('_min')), `attr-label`=gsub('_min', '', var_stat), `data-min`=`data-min`),
              transmute(gather(ranges_df_wide, var_stat, `data-max`, ends_with('_max')), `attr-label`=gsub('_max', '', var_stat), `data-max`=`data-max`),
              by='attr-label'),
    full_join(transmute(gather(ranges_df_wide, var_stat, `num-NA`, ends_with('_numNA')), `attr-label`=gsub('_numNA', '', var_stat), `num-NA`=`num-NA`),
              transmute(gather(ranges_df_wide, var_stat, `num-tot`, ends_with('_numTot')), `attr-label`=gsub('_numTot', '', var_stat), `num-tot`=`num-tot`),
              by='attr-label'),
    by='attr-label')
}

format_bound <- function(bound) {
  if(is(bound, 'Date')) {
    format(bound, format='%Y-%m-%d')
  } else if(is(bound, 'POSIXt')) {
    format(bound, format='%Y-%m-%d %H:%M:%S %z')
  } else if(is(bound, 'Date')) {
    format(bound, format='%Y-%m-%d')
  } else if(is(bound, 'character')) {
    bound
  } else if(is(bound, 'numeric')) {
    format(bound, digits=4, scientific=FALSE)
  } else if(is(bound, 'logical')) {
    as.character(bound)
  } else {
    stop(paste('unrecognized class:', paste(class(bound), collapse=', ')))
  }
}
