#' collect errors from a results_xxxxxx directory into lists of model_names that
#' were successful, are still in progress, or reported an error. assumes that 
#' the resdir contains a folder called results_[runid] and a status.file called 
#' cluster_jobs_[runid].tsv, where the rows of the status file match up 1:1
#' (with 0-indexing of jobs on condor) with the job IDs on the cluster.
collect_errors <- function(resdir='../2_metab_config/run1/out',
                           runid='170217',
                           status.file=paste0('cluster_jobs_', runid, '.tsv'),
                           outfile=paste0(resdir, '/results_', runid, '_errors.txt')) {
  
  library(dplyr)
  
  # read in file info from job directories within resdir/results_runid
  jobdirs <- dir(file.path(resdir, paste0('results_', runid)), full.names=TRUE)
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
    }))
  
  # use the status file specific to this run ID to match job IDs to config rows
  status <- read.table(file.path(resdir, status.file), header=TRUE, sep='\t', stringsAsFactors=FALSE)
  job.tbl <- mutate(
    job.tbl,
    model_path = status[job+1,'filepath']) %>%
    { bind_cols(., parse_metab_model_path(.$model_path)) } %>%
    arrange(row) %>%
    select(job, row, site, everything())

  # assign a status to each job: specific errors, Success, or Incomplete run (in
  # progress)
  done <- setNames(!is.na(job.tbl$sumfile), basename(job.tbl$jobdir))
  errs <- lapply(setNames(1:nrow(job.tbl), basename(job.tbl$jobdir)), function(rownum) {
    jtrow <- job.tbl[rownum, ]
    errfile <- jtrow$errfile
    if(!is.na(errfile)) {
      readLines(file.path(jtrow$jobdir, errfile))
    } else {
      sumfile <- jtrow$sumfile
      if(!is.na(sumfile)) {
        'Success'
      } else {
        'Incomplete run'
      }
    }
  })
  
  # reformat as lists of unique statuses, each of which has a set of jobs+rows+model_names associated with it
  err.uniq <- unique(errs)
  err.tbl <- lapply(err.uniq, function(eu) {
      list(
        error = eu,
        jobs = sort(as.numeric(substring(names(which(sapply(errs, function(err) isTRUE(all.equal(err, eu))))), 5)))
      )
    }) %>%
    lapply(function(e) {
      e$rows = job.tbl$row[match(e$jobs, job.tbl$job)]
      e$model_name = job.tbl$model_name[match(e$jobs, job.tbl$job)]
      e
    })
  outtxt <- unlist(lapply(err.tbl, function(e) {
    c(e$error, 
      if(length(e$jobs) > 0) c(
        # sprintf('%3d  %3d  %s', e$jobs, e$rows, e$sites),
        sprintf('  - %s', dplyr::left_join(data.frame(row=sort(e$rows)), job.tbl, by='row')$model_name)
      ) else '[no jobs]', 
      '')
  }))
  writeLines(outtxt, outfile)
  err.tbl
}
