# function to dump data to tab delimited file with line 2 as units.
dumpdf <- function(df, file, chem) {
  require(smwrBase)
  # Tweak staid
  staid <- paste0("nwis_", df$site_no)
  # transform date/time
  DT <- with(df, setTZ(sample_dt + as.timeDay(sample_tm), tzone_cd, force.stz=TRUE))
  attr(DT, "tzone") <- "UTC"
  EDT <- with(df, setTZ(sample_end_dt + as.timeDay(sample_end_tm), tzone_cd, force.stz=TRUE))
  attr(EDT, "tzone") <- "UTC"
  units <- c("", "UTC", "UTC", "", unlist(chem))
  for(col in names(chem)) {
    df[[col]] <- as.character(round(df[[col]], 3)) # make it look prettier
  }
  df <- cbind(site_no=staid, DateTime=format(DT), EndDateTime=format(EDT),
              medium_cd=df$medium_cd, as.matrix(df[names(chem)]))
  # sort by staid
  df <- df[order(staid), ]
  df <- rbind(units, df)
  colnames(df)[1:4] <- c("SiteNumber", "DateTime", "EndDateTime", "MediumCode")
  write.table(df, file=file, quote=FALSE, sep="\t", row.names=FALSE)
  invisible()
}
