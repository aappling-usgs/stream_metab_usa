source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "type", "on_exists", "verbose", "outfile"))

add_site_metadata <- function(type='basic', on_exists='merge', verbose=TRUE, outfile) {
  # stage and post
  if(type != 'basic') stop("not prepared for non-basic meta")
  meta_file <- stage_meta(sites=list_sites()) # currently assumes type='basic' and names file that way
  post_meta(meta_file, on_exists=on_exists, verbose=verbose)
  
  # report & return
  message("basic metadata are fully posted to SB")
  writeLines(as.character(Sys.time()), outfile)
  invisible()
}
do.call(add_site_metadata, args[c('type', 'on_exists', 'verbose', 'outfile')])