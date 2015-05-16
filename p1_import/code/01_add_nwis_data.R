# Parse command-line arguments

args <- commandArgs(TRUE)
for (i in 1:length(args)){
  eval(parse(text=args[i])) # expecting args sb_user, sb_password, and outfile
}
cat(paste0("Setting up site data as ", sb_user, "\n"))


# Load libraries
library(dataRetrieval)
library(sbtools)
library(mda.streams)
library(powstreams)

# Enter sandbox
# sbtools::set_endpoint("production") # the default
# sbtools::set_endpoint("development") # this doesn't work off campus (except maybe with VPN?)

# Log in
if(exists("sb_password")) 
  session = authenticate_sb(sb_user, sb_password) 
else 
  session = authenticate_sb(sb_user)

# Find the sites root ("Sites_dev" folder)
sites_root <- sbtools::query_item_identifier(scheme="mda_streams_dev", type="sites_root", key="uber")

# Add sites data
site_roots <- item_list_children(sites_root$id, current_session(), limit=1000)$id
sites <- sapply(site_roots, mda.streams:::get_title, session=current_session()) #get_title should probably be exported
vars <- c(disch='00060',doobs='00300',stage='00072',wtr='00010')
for(site in sites) {
  for(varname in names(vars)) {

    # don't replace existing files
    if(item_exists(scheme='mda_streams_dev', type=make_ts_variable(varname), key=site, session=current_session())) next
    
    # download from NWIS
    nwis_ts <- mda.streams::get_nwis_df(site=site, variable_name = varname, p_code=vars[varname])
    if(nrow(nwis_ts) == 0) next # if we got no data, just go to the next one
    
    # format and write to file
    nwis_ts[,1] <- strftime(nwis_ts[,1], usetz = T, tz = 'UTC') # lock in the timezone. coerce to char
    fpath = tempfile(fileext = paste0('.',mda.streams:::get_ts_extension(), '.gz'))
    gz1 <- gzfile(fpath, "w")
    write.table(nwis_ts,  gz1, sep=mda.streams:::get_ts_delim(), row.names=FALSE, quote = FALSE)
    close(gz1)
    
    # push to SB
    # as in post_ts(site=site, data=nwis_ts, session=current_session())
    ts_varname <- names(nwis_ts)[-1]
    print(c(site, varname, ts_varname))
    ts_item <- item_create(parent_id=names(sites)[match(site,sites)], title=ts_varname, current_session())
    item_append_files(ts_item, files=fpath, session=current_session())
    item_update_identifier(ts_item, scheme='mda_streams_dev', type=ts_varname, key=site, session=current_session())
  }
}
