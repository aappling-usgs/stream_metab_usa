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
    data_df <- switch(
      tools::file_ext(unz.file),
      'tsv' = readr::read_tsv(unz.file, col_types = coltypes),
      'rds' = unitted::v(readr::read_rds(unz.file)),
      stop(paste('unrecognized file type:', tools::file_path_sans_ext(unz.file)))
    )
    
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
