library(dplyr)
library(readr)

#' Create a table of timeseries data sources
tbl_timeseries_sources <- function(
  metafile='../4_data_release/out/timeseries.xml',
  outfile='../4_manuscript/tbl/timeseries_sources.tex'
) {
  metaxml <- xml2::as_list(xml2::read_xml(metafile))
  
  source_df <- bind_rows(lapply(metaxml$metadata$eainfo, function(details) {
    var_src <- details[[1]]$enttypl[[1]] %>%
      gsub('nwis_XXXX-ts_', '', ., fixed=TRUE) %>%
      gsub('.tsv', '', ., fixed=TRUE) %>%
      gsub('_', '\\_', ., fixed=TRUE)
    definition <- details[[3]]$attrdef[[1]] %>%
      gsub('_', '\\_', ., fixed=TRUE) %>%
      gsub('NWIS database', '\\cite{u.s.geologicalsurvey_national_2017}', ., fixed=TRUE) %>%
      gsub('NLDAS database', '\\cite{mitchell_multiinstitution_2004, xia_continentalscale_2012}', ., fixed=TRUE) %>%
      gsub('GLDAS database', '\\cite{rodell_global_2004}', ., fixed=TRUE) %>%
      gsub(' in site_data.tsv', '', ., fixed=TRUE) %>%
      gsub('Garcia and Gordon 1992', '\\cite{garcia_oxygen_1992}', ., fixed=TRUE) %>%
      gsub('Raymond et al. 2012', '\\cite{raymond_scaling_2012}', ., fixed=TRUE) %>%
      gsub('cQ^f', '$cQ^f$', ., fixed=TRUE) %>%
      gsub('kQ^m', '$kQ^m$', ., fixed=TRUE)
    descrip_prov <- strsplit(definition, split='. ', fixed=TRUE)[[1]]
    method <- ifelse(details[[3]]$attrdefs[[1]] == 'This release', 'Calculated', 'Downloaded')
    units <- details[[3]]$attrdomv$rdom$attrunit[[1]] %>%
      gsub('NA', 'unitless', ., fixed=TRUE) %>%
      gsub('%', '\\%', ., fixed=TRUE) %>%
      gsub('^3', '$^3$', ., fixed=TRUE) %>%
      gsub('^-1', '$^{-1}$', ., fixed=TRUE) %>%
      gsub('^-2', '$^{-2}$', ., fixed=TRUE) %>%
      gsub('umol', '$\\mu$ mol', ., fixed=TRUE)
    data_frame(`Variable Name`=var_src, Definition=sprintf("%s (%s)", descrip_prov[1], units), Provenance=descrip_prov[2], Method=method)
  })) %>%
    arrange(desc(Method), `Variable Name`)
  
  src_tex <- source_df %>%
    mutate(tex=sprintf('%s & %s & %s \\\\', `Variable Name`, Definition, Provenance)) %>%
    pull(tex)
  
  writeLines(src_tex, outfile)
}

