#' Tally the models assigned to Low, Medium, or High confidence
tbl_struct_assessment <- function(
  metafile='../4_data_release/cache/site_data.tsv',
  outfile='../4_manuscript/tbl/struct_assessment.tex'
) {
  meta <- readr::read_tsv(metafile)
  
  rating_tbl <- 
    bind_rows(list(
      as_data_frame(as.list(table(v(meta)$struct.dam_flag))) %>% mutate(Structure='Dam'),
      as_data_frame(as.list(table(v(meta)$struct.canal_flag))) %>% mutate(Structure='Canal/ditch'),
      as_data_frame(as.list(table(v(meta)$struct.npdes_flag))) %>% mutate(Structure='NPDES'),
      as_data_frame(as.list(table(pmin(v(meta)$struct.npdes_flag, v(meta)$struct.canal_flag, v(meta)$struct.dam_flag)))) %>% mutate(Structure='Any')
    )) %>%
    select(Structure, everything())
  rating_tbl
  
  rating_tex <- rating_tbl %>%
    mutate(tex=sprintf('%s & %s & %s & %s & %s \\\\', Structure, `0`, `50`, `80`, `95`)) %>%
    pull(tex)
  
  writeLines(rating_tex, outfile)
}

