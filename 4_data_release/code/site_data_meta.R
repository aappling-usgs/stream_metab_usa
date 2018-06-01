describe_site_data_attrs <- function(site_data, attr_file) {
  
  # sketch out the skeleton attribute table
  attr_temp <- tempfile(fileext='.csv')
  attribute_skeleton(site_data, attr_temp)
  attr_df <- readr::read_csv(attr_temp, col_types = 'cccnnc')
  
  # fill out the attribute table
  defs_df <- tibble::tribble(
    ~`attr-label`, ~`attr-def`, ~`attr-defs`, ~`data-units`,
    "site_name", "Unique site identifiers for this dataset, formed by prefixing nwis_id (next attribute) with 'nwis_'", "Producer defined", NA,
    "nwis_id", "Site number in National Water Information System", "National Water Information System, United States Geological Survey, https://help.waterdata.usgs.gov/faq/sites/do-station-numbers-have-any-particular-meaning", NA,
    "nhdplus_id", "Common identifier (ComID) of the stream reach containing the monitoring site", "National Hydrography Dataset Plus, Version 2 (NHDPlusV2), http://www.horizon-systems.com/NHDPlus/NHDPlusV2_home.php", NA,
    "long_name", "Extended site name", "National Water Information System, United States Geological Survey", NA,
    "lat", "Decimal latitude", NA, "degrees N",
    "lon", "Decimal longitude", NA, "degrees E",
    "coord_datum", "Coordinate datum for lat and lon", "Coordinate Datum Codes (dec_coord_datum_cd or coord_datum_cd), National Water Information System, United States Geological Survey, https://help.waterdata.usgs.gov/coord_datum_cd", NA,
    "alt", "Altitude above mean sea level", "National Water Information System, United States Geological Survey, https://waterdata.usgs.gov/usa/nwis/current?submitted_form=introduction", "ft",
    "alt_datum", "Vertical datum", "Vertical Datum Codes (alt_datum_cd), National Water Information System, United States Geological Survey, https://help.waterdata.usgs.gov/code/alt_datum_cd_query?fmt=html", NA,
    "site_type", "Type of water body at monitoring site", "Site Type Codes (site_tp_cd), National Water Information System, United States Geological Survey, https://help.waterdata.usgs.gov/site_tp_cd", NA,
    "dvqcoefs.c", "Empirical coefficient, c, of depth estimation equation, where depth=cQ^f, depth is in m, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "dvqcoefs.f", "Empirical coefficient, f, of depth estimation equation, where depth=cQ^f, depth is in m, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "dvqcoefs.a", "Empirical coefficient, a, of width estimation equation, where width=aQ^b, width is in m, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "dvqcoefs.b", "Empirical coefficient, b, of width estimation equation, where width=aQ^b, width is in m, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "dvqcoefs.k", "Empirical coefficient, k, of velocity estimation equation, where velocity=kQ^m, velocity is in m s^-1, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "dvqcoefs.m", "Empirical coefficient, m, of velocity estimation equation, where velocity=kQ^m, velocity is in m s^-1, and Q is discharge in m^3 s^-1", "Gomez-Velez et al. 2015", NA,
    "struct.canal_flag", "Flag for presence of NHDPlusV2-identified canal or ditch upstream of probe location. 95: nearest canal or ditch is beyond the 95th quantile of daily average distances to 80% oxygen turnover. 80: nearest canal or ditch is beyond the 80th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 95). 50: nearest canal or ditch is beyond the 50th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 80). 0: nearest canal or ditch is closer than the distance represented by 50.", "Producer defined", NA,
    "struct.dam_flag", "Flag for presence of dam upstream of probe location. 95: nearest dam is beyond the 95th quantile of daily average distances to 80% oxygen turnover. 80: nearest dam is beyond the 80th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 95). 50: nearest dam is beyond the 50th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 80). 0: nearest dam is closer than the distance represented by 50.", "Producer defined", NA,
    "struct.npdes_flag", "Flag for presence of NPDES point discharge upstream of probe location. 95: nearest NPDES point is beyond the 95th quantile of daily average distances to 80% oxygen turnover. 80: nearest NPDES point is beyond the 80th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 95). 50: nearest NPDES point is beyond the 50th quantile of daily average distances to 80% oxygen turnover (and closer than the distance represented by 80). 0: nearest NPDES point is closer than the distance represented by 50.", "Producer defined", NA
  )
  
  # compute data min and max, using NA for character fields
  ranges_df <- data_frame(
    `attr-label`=attr_df$`attr-label`,
    'data-min'=sapply(`attr-label`, function(attr_label) {
      col <- site_data[[attr_label]]
      if(is.numeric(col)) format(min(col, na.rm=TRUE), digits = 3) else NA
    }),
    'data-max'=sapply(`attr-label`, function(attr_label) {
      col <- site_data[[attr_label]]
      if(is.numeric(col)) format(max(col, na.rm=TRUE), digits = 3) else NA
    }))
  
  # combine the skeleton, ranges, and definitions
  attr_df_combined <- attr_df %>%
    select(`attr-label`) %>%
    left_join(select(ranges_df, `attr-label`, `data-min`, `data-max`), by='attr-label') %>%
    left_join(defs_df, by='attr-label') %>%
    select(names(attr_df))
  
  # write the final attribute table
  readr::write_csv(attr_df_combined, path=attr_file)
}