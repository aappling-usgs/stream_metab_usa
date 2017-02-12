collect_errors <- function(resdir='../2_metab_config/prep/out/results_170210D',
                           outfile=paste0(resdir, '_errors.txt')) {
  jobdirs <- dir(resdir, full.names=TRUE)
  job.tbl <- data_frame(
    jobdir = jobdirs,
    job = as.numeric(substring(basename(jobdirs), 5)),
    errfile = sapply(jobdirs, function(jobdir) {
      errfi <- grep("^error ", dir(jobdir), value=TRUE)
      if(length(errfile) > 1) warning("multiple error files; only using the first")
      if(length(errfile) > 0) errfi[1] else NA
    })) %>%
    { bind_cols(
      ., parse_metab_model_name(substring(.$errfile, 7, nchar(.$errfile) - 4))
    ) } %>%
    select(job, row, site, everything())

  errs <- file.path(job.tbl$jobdir, job.tbl$errfile) %>%
    setNames(basename(job.tbl$jobdir)) %>%
    lapply(function(errfile) {
      if(!is.na(errfile)) {
        readLines(errfile)
      } else {
        NA
      }
    })
  err.uniq <- unique(errs)
  err.tbl <- c(
    list(
      list(
        error='success!',
        jobs=sort(as.numeric(substring(setdiff(basename(jobdirs), names(errs)), 5)))
      )
    ),
    lapply(err.uniq, function(eu) {
      list(
        error = eu,
        jobs = sort(as.numeric(substring(names(which(sapply(errs, function(err) isTRUE(all.equal(err, eu))))), 5)))
      )
    })) %>%
    lapply(function(e) {
      e$rows = job.tbl$row[match(e$jobs, job.tbl$job)]
      e$sites = job.tbl$site[match(e$jobs, job.tbl$job)]
      e
    })
  outtxt <- unlist(lapply(err.tbl, function(e) {
    c(e$error, if(length(e$jobs) > 0) sprintf('%3d  %3d  %s', e$jobs, e$rows, e$sites) else '[no jobs]', '')
  }))
  writeLines(outtxt, outfile)
  err.tbl
}
