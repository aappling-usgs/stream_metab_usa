collect_errors <- function(resdir='../2_metab_config/prep/out/results_170210D',
                           outfile=paste0(resdir, '_errors.txt')) {
  status <- read.table(file.path(dirname(resdir), 'files_metab.tsv'), header=TRUE, sep='\t', stringsAsFactors=FALSE) %>%
    filter(!posted)
  jobdirs <- dir(resdir, full.names=TRUE)
  job.tbl <- data_frame(
    jobdir = jobdirs,
    job = as.numeric(substring(basename(jobdirs), 5)),
    errfile = sapply(jobdirs, function(jobdir) {
      errfi <- grep("^error ", dir(jobdir), value=TRUE)
      if(length(errfi) > 1) warning("multiple error files; only using the first")
      if(length(errfi) > 0) errfi[1] else NA
    }),
    sumfile = sapply(jobdirs, function(jobdir) {
      sumfi <- grep("^summary ", dir(jobdir), value=TRUE)
      if(length(sumfi) > 1) warning("multiple summary files; only using the first")
      if(length(sumfi) > 0) sumfi[1] else NA
    }),
    model_path = status[job+1,'filepath']) %>%
    { bind_cols(., parse_metab_model_path(.$model_path)) } %>%
    select(job, row, site, everything())

  done <- setNames(!is.na(job.tbl$sumfile), basename(job.tbl$jobdir))
  errs <- lapply(setNames(1:nrow(job.tbl), basename(job.tbl$jobdir)), function(rownum) {
    jtrow <- job.tbl[rownum, ]
    errfile <- jtrow$errfile
    if(!is.na(errfile)) {
      readLines(file.path(jtrow$jobdir, errfile))
    } else {
      sumfile <- jtrow$sumfile
      if(!is.na(sumfile)) {
        'Success!'
      } else {
        'Incomplete run'
      }
    }
  })
  err.uniq <- unique(errs)
  err.tbl <- lapply(err.uniq, function(eu) {
      list(
        error = eu,
        jobs = sort(as.numeric(substring(names(which(sapply(errs, function(err) isTRUE(all.equal(err, eu))))), 5)))
      )
    }) %>%
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
