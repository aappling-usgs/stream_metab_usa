source("lib/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "var", "on_exists", "verbose"))

#' Pull water chemistry data from NWIS onto ScienceBase
source("lib/add_nwis_data.R")
add_nwis_data(var=args$var, on_exists=args$on_exists, sb_user=args$sb_user, sb_password=args$sb_password, verbose=args$verbose)
