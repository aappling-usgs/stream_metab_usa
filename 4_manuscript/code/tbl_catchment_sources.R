#' Create a table of catchment polygon data sources
tbl_catchment_sources <- function(catchments=catchments_as_written, metafile='../4_data_release/out/site_catchment_shapefile.xml', outfile='../4_manuscript/tbl/catchment_sources.csv') {
  srccount <- table(catchments$dat_src)
  sources <- names(srccount)
  metaxml <- xml2::as_list(xml2::read_xml(metafile))
  srcinfos <- metaxml$dataqual$lineage[names(metaxml$dataqual$lineage) == 'srcinfo']
  
  cites <- purrr::map_df(sources, function(sourcename) {
    # acquire the corresponding source record simply as a check that it exists
    thissrc <- which(sapply(srcinfos, function(srcinfo) grepl(sourcename, srcinfo$srccitea)))
    srcinfo <- srcinfos[[thissrc]]
    
    data_frame(
      Abbreviation=sourcename,
      NumBasins=srccount[[sourcename]],
      Reference=c(
        'epa_basins'='U.S. Environmental Protection Agency. 2017. BASINS 4.1 (Better Assessment Science Integrating point & Non-point Sources) Modeling Framework. National Exposure Research Laboratory, RTP, North Carolina.',
        'Falcone 2011'='Falcone, J. A. 2011. GAGES-II: Geospatial Attributes of Gages for Evaluating Streamflow.',
        'Falcone et al. 2017'='Falcone, J. A., N. T. Baker, and C. V. Price. 2017. Watershed boundaries for study sites of the U.S. Geological Survey Surface Water Trends project.',
        'gagesii_basins'='Falcone, J. A. 2011. GAGES-II: Geospatial Attributes of Gages for Evaluating Streamflow.',
        'Nakagaki et al. 2016'='Nakagaki, N., S. L. Qi, J. W. Frey, D. T. Button, N. T. Baker, T. E. Burley, and P. C. Van Metre. 2016. Geospatial database of the study boundary, sampled sites, watersheds, and riparian zones developed for the U.S. Geological Survey Midwest Stream Quality Assessment. http://dx.doi.org/10.5066/F7CN7202.',
        'StreamStats'='U.S. Geological Survey. 2017. StreamStats. https://water.usgs.gov/osw/streamstats.',
        'U.S. Geological Survey 2013'='U.S. Geological Survey. 2013. National Hydrography Database: The National Map Viewer. https://viewer.nationalmap.gov/viewer/index.html?p=nhd.',
        'Wieczorek 2012'='Wieczorek, M. E. 2012. USGS Streamgage NHDPlus Version 1 Basins 2011. http://water.usgs.gov/GIS/metadata/usgswrd/XML/streamgagebasins.xml.'
      )[sourcename]
    )
  }) %>%
    arrange(Reference)
  
  readr::write_csv(cites, outfile)
}

