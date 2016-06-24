library(mda.streams)

sbtools::authenticate_sb(SBUSER, SBPASS)

# list of models in the final batch (PR + fixed K)
mms <- grep("0.0.15", list_metab_models(), value=TRUE, fixed=TRUE)
out <- lapply(mms, function(mmname) {
  message(mmname)
  mm <- get_metab_model(mmname, on_local_exists='skip', version='original', update_sb=FALSE)
  staged <- stage_metab_ts(mm)
  posted <- post_ts(staged, on_exists='replace')
})
# done 12/3/15