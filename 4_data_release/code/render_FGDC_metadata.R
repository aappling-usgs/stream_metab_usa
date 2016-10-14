
#' @param template.file a text file to be used as the template
#' @param file.out file to write the output to
#' @param \dots lists to use as text in rendering
#' @details 
#' order matters for ... arguments. The LAST argument will overwrite anything that proceeds it. 
#' That means that \code{render_FGDC_metadata(template.file, file.out, list(dog='Larry'), list(dog='Cindy'))}
#' will use \code{dog='Cindy'}.
render_FGDC_metadata <- function(template.file, file.out, ...){
  text.lists <- list(...)
  text <- text.lists[[1]]
  if (length(text.lists) > 1){
    for (i in 2:length(text.lists)){
      text <- list_append_replace(text, text.lists[[i]])
    }
  }
  
  template <- paste(readLines(template.file ),collapse = '\n')
  cat(whisker::whisker.render(template, text), file = file.out)
  xml <- xml2::read_xml(file.out)
  xml2::write_xml(xml, file.out)
}


