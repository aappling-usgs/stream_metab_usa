sb_post_meta <- function(meta.file) {
  post_meta(meta.file, on_exists = 'replace')
  
  m_by_tag <- locate_meta(parse_meta_path(meta.file)$type, by='tag')
  m_by_dir <- locate_meta(parse_meta_path(meta.file)$type, by='dir')
  failures <- meta.file[which(is.na(m_by_tag) | is.na(m_by_dir))]
  
  if(length(failures) > 0) stop("failed to post metadata file[s]: ", paste(failures, collapse=', '))
  
  # if there were no failures, return the meta types we posted
  return(parse_meta_path(meta.file)$type)
}
