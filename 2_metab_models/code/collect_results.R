library(dplyr)
library(mda.streams)
resdir <- '../2_metab_models/out'
resdirs <- dir(resdir, pattern='results_[[:alnum:]]*$', full.names=TRUE)
jobdirs <- unlist(lapply(resdirs, dir, pattern='job_', full.names=TRUE))
jobcont <- bind_rows(lapply(jobdirs, function(jobdir) {
  cont <- dir(jobdir)
  model <- unique(gsub('^[[:alpha:]]*( |_)', '', tools::file_path_sans_ext(cont)))
  data_frame(
    jobdir=jobdir,
    model=model[1],
    has_partial=any(grepl('^partial', cont)),
    has_fit=any(grepl('^fit', cont)),
    has_summary=any(grepl('^summary', cont)),
    has_error=any(grepl('^error', cont)),
    partial=if(has_partial) grep('^partial', cont, value=TRUE) else NA,
    fit=if(has_fit) grep('^fit', cont, value=TRUE) else NA,
    summary=if(has_summary) grep('^summary', cont, value=TRUE) else NA,
    error=if(has_error) grep('^error', cont, value=TRUE) else NA)
}))
jobinfo <- bind_cols(jobcont, parse_metab_model_name(jobcont$model, use_names = FALSE))

filter(jobinfo, has_fit, !has_summary)
summaryfiles <- filter(jobinfo, has_summary) %>%
  mutate(summary_file=file.path(jobdir, summary)) %>%
  {.$summary_file}
dir.create('../2_metab_models/out/summaries')
for(s in seq_along(summaryfiles)) {
  file.copy(summaryfiles[s], '../2_metab_models/out/summaries')
}

fitfiles <- filter(jobinfo, has_fit) %>%
  mutate(fit_file=file.path(jobdir, fit)) %>%
  {.$fit_file}
dir.create('../2_metab_models/out/fits')
for(s in seq_along(fitfiles)) {
  file.copy(fitfiles[s], '../2_metab_models/out/fits')
}
