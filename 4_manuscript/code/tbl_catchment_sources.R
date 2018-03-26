#' Create a table of catchment polygon data sources
tbl_catchment_sources <- function(
  catchments=catchments_as_written,
  metafile='../4_data_release/out/site_catchment_shapefile.xml',
  outfile='../4_manuscript/tbl/catchment_sources.tex'
) {
  srccount <- table(catchments$dat_src)
  sources <- names(srccount)
  metaxml <- xml2::as_list(xml2::read_xml(metafile))
  srcinfos <- metaxml$metadata$dataqual$lineage[names(metaxml$metadata$dataqual$lineage) == 'srcinfo']
  
  source_df <- tibble::tribble(
    ~abbreviation, ~Description, ~Reference, ~longcite,
    'epa_basins', 'EPA BASINS', '\\cite{u.s.environmentalprotectionagency_basins_2017}', 'U.S. Environmental Protection Agency. 2017. BASINS 4.1 (Better Assessment Science Integrating point & Non-point Sources) Modeling Framework. National Exposure Research Laboratory, RTP, North Carolina.',
    'Falcone 2011', 'USGS GAGES-II', '\\cite{falcone_gagesii_2011}', 'Falcone, J. A. 2011. GAGES-II: Geospatial Attributes of Gages for Evaluating Streamflow.',
    'gagesii_basins', 'USGS GAGES-II', '\\cite{falcone_gagesii_2011}', 'Falcone, J. A. 2011. GAGES-II: Geospatial Attributes of Gages for Evaluating Streamflow.',
    'Falcone et al. 2017', 'Falcone et al. 2017', '\\cite{falcone_watershed_2017}', 'Falcone, J. A., N. T. Baker, and C. V. Price. 2017. Watershed boundaries for study sites of the U.S. Geological Survey Surface Water Trends project.',
    'Nakagaki et al. 2016', 'Nakagaki et al. 2016', '\\cite{nakagaki_geospatial_2016}', 'Nakagaki, N., S. L. Qi, J. W. Frey, D. T. Button, N. T. Baker, T. E. Burley, and P. C. Van Metre. 2016. Geospatial database of the study boundary, sampled sites, watersheds, and riparian zones developed for the U.S. Geological Survey Midwest Stream Quality Assessment. http://dx.doi.org/10.5066/F7CN7202.',
    'StreamStats', 'USGS StreamStats', '\\cite{u.s.geologicalsurvey_streamstats_2017}', 'U.S. Geological Survey. 2017. StreamStats. https://water.usgs.gov/osw/streamstats.',
    'U.S. Geological Survey 2013', 'USGS National Map Viewer', '\\cite{u.s.geologicalsurvey_national_2013}', 'U.S. Geological Survey. 2013. National Hydrography Database: The National Map Viewer. https://viewer.nationalmap.gov/viewer/index.html?p=nhd.',
    'Wieczorek 2012', 'Wieczorek 2012', '\\cite{wieczorek_usgs_2012}', 'Wieczorek, M. E. 2012. USGS Streamgage NHDPlus Version 1 Basins 2011. http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgagebasins.xml.'
  )
  srccount_df <- data_frame(abbreviation=names(srccount), `Number of Basins`=unname(srccount))
  src_tbl <- left_join(srccount_df, source_df, by='abbreviation') %>%
    select(-abbreviation) %>%
    group_by(Description, Reference) %>%
    summarize(`Number of Basins`=sum(`Number of Basins`)) %>%
    arrange(desc(`Number of Basins`))
  
  src_tex <- src_tbl %>%
    mutate(tex=sprintf('%s & %s & %s \\\\', Description, Reference, `Number of Basins`)) %>%
    pull(tex)
  
  writeLines(src_tex, outfile)
}

