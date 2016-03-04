# ScienceBase Data Status & History

### This NEWS file is current as of 3/4/2016

## 12/3/15 - 12/4/15

Posted model run `"151203 0.0.17 PR_fixed_K"` as `gpp_estBest`, `er_estBest`, and `K600_estBest`

- pseudo-hierarchical approach: (1) fit GPP, ER, and K600 all at once with MLE, (2) fit regressions to estimated K600 vs log(discharge), then (3) use regression predictions to fix K600 and refit GPP and ER again with MLE

- step (2) above weighted daily estimates by 1/CI for the regression. I now think this overweighted some low K600 values, so a next step is to rerun with weights of K600/CI instead. See [issue 64](https://github.com/USGS-CIDA/stream_metab_usa/issues/64).

- uses simulated light curves (no clouds; `par_calcLon`), site-specific hydraulic geometry coefficients from Jud Harvey (`depth_calcDischHarvey`), and best available data on DO (`doobs_nwis`), water temperature (`wtr_nwis`), etc.

- replaces model results only for sites marked 'accept' or 'examine'

- old model results (8/1/15) remain in place for other sites


## 8/5/15 - 8/21/15

Posted daily data. NB: these daily values are computed for the midnight-to-midnight window, which often differs from the window used to estimate GPP, ER, and K600

- daily discharge and velocity (`dischdaily` & `velocdaily`)

- daily DO amplitudes (`doamp`)


## 8/1/15

Posted model run `"150730 0.0.7 MLE_for_PRK_wHarvey_and_sw"` as `gpp_estBest`, `er_estBest`, and `K600_estBest`

- daily time translations. `sitedate` is a time series where the `DateTime` column is the local, longitude-specific noon in UTC and the `sitedate` column is the date in Date format. `sitetimedaily` is a time series where the `DateTime` column is the local, longitude-specific noon in UTC and `sitetimedaily` is the local, longitude-specific noon as noon itself. 

- includes metabolism models for all sites with sufficient data

- uses observed light from NLDAS (`par_calcSw`), depth estimated with  from Jud Harvey (`depth_calcDischHarvey`), and best available data on DO (`doobs_nwis`), water temperature (`wtr_nwis`), etc.


## 7/1/15 - 8/1/15

Posted first generation of input data; see Page 2 of [Powstreams Cheatsheet](https://drive.google.com/open?id=0B-c5tErcTY2fMlA3OE55NGhjeWc) for complete definitions. start_date = 2006-10-01; end_date = 2015-06-07

- time translations. The `DateTime` column of every timeseries is in UTC. `sitetime` is the mean solar time, i.e., the local time at the exact longitude of the site, such that noon is close to solar noon and every day is exactly 24 hours long. `suntime` is the apparent solar time, such that noon is precisely the time of solar maximum

- estimates of depth & velocity (based on discharge) using site-specific hydraulic geometry coefficients from Jud Harvey (`depth_calcDischHarvey`, `veloc_calcDischHarvey`) and global coefficients from Raymond et al. 2012 (`depth_calcDischRaymond`, `veloc_calcDischRaymond`)

- estimates of dissolved oxygen at saturation (`dosat`, `dopsat`) based on DO and water temperature

- observations of dissolved oxygen (`doobs`), water temperature (`wtr`), barometric pressure (`baro`), discharge (`disch`), photosynthetically active radiation using lat/lon (`par_calcLon`; smooth daily curves with no clouds) or NLDAS downscaled light estimates (`par_calcSw`; rougher curves including intermittent clouds)
