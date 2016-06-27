write_status_table <- function(table, filename){
  write.table(table, file=filename, sep='\t', row.names=FALSE)
}

read_status_table <- function(filename) {
  read.table(file=filename, sep='\t', header = TRUE, stringsAsFactors = FALSE)
}