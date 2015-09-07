dC <- function(df, file) {
  # Tweak staid
  staid <- paste0("nwis_", rownames(df))
  PC <- as.character(round(df$PctCen, 1))
  df <- cbind(SiteNumber=c("", staid), PctCen=c("", PC))
  write.table(df, file=file, quote=FALSE, sep="\t", row.names=FALSE)
  invisible()
}