#' Replace existing metadata files with new ones
#' 
#' 
sb_post_meta <- function(meta.file) {
  # try the post operation
  post_meta(meta.file, on_exists = 'replace')
  
  # for those meta files that were successfully posted just now, check the tags 
  # and repair if needed
  types <- parse_meta_path(meta.file)$type
  posted <- locate_meta(types, by='dir')
  repair_meta(types[!is.na(posted)])
  
  # check for metafiles that somehow still haven't been properly posted and/or 
  # tagged
  m_by_tag <- locate_meta(parse_meta_path(meta.file)$type, by='tag')
  m_by_dir <- locate_meta(parse_meta_path(meta.file)$type, by='dir')
  failures <- meta.file[which(is.na(m_by_tag) | is.na(m_by_dir))]
  
  # stop if there were failures
  if(length(failures) > 0) stop("failed to post metadata file[s]: ", paste(failures, collapse=', '))
  
  # if there were no failures, return the meta types we posted
  return(types)
}
