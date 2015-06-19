process_make_args <- function(expect_args=c("sb_user","sb_password","outfile")) {
  
  # Print date
  message("timestamp: ", Sys.time(), "\n")
  
  # Load libraries
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(sbtools))
  suppressPackageStartupMessages(library(mda.streams))
  
  # Parse command-line arguments
  argdf <- do.call(rbind, lapply(commandArgs(TRUE), function(arg) {
    loc_equals <- as.numeric(gregexpr("=", arg)[1][1])
    varname <- substr(arg, 1, loc_equals-1)
    if(loc_equals < nchar(arg)) {
      value <- substring(arg, as.numeric(loc_equals)+1)
      
    } else {
      value <- NA
    }
    data.frame(key=varname, value=value, stringsAsFactors=FALSE)
  }))
  args <- as.list(argdf$value) %>%
    setNames(argdf$key) %>%
    lapply(function(value) {
      if(value %in% c("TRUE","FALSE")) c("TRUE"=TRUE, "FALSE"=FALSE)[[value]] else value
    })
  
  # Check that the args are what we expected
  if(length(missed <- setdiff(expect_args, names(args))) > 0) 
    stop("missing args: ", paste(missed, collapse=", "))
  if(length(extra <- setdiff(names(args), expect_args)) > 0) 
    stop("extra args: ", paste(extra, collapse=", "))

  # Print args, hiding the sb_password
  message("args:")
  args_for_print <- argdf
  if(!is.na(args_for_print[args_for_print$key=="sb_password","value"])) args_for_print[args_for_print$key=="sb_password","value"] <- "*****"
  print(args_for_print)
  
  # Log in
  message("\nsigning into ScienceBase with the password you set.\n")
  authenticate_sb(args$sb_user, args$sb_password) 

  return(args)
}

