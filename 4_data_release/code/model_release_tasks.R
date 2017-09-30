#### DIRS ####

#' @param ... named arguments, each giving a directory name; will be returned as
#'   a named list
create_dirs <- function(...) {
  dirs <- list(...)
  lapply(dirs, function(dirname) {
    if(!dir.exists(dirname)) dir.create(dirname, recursive=TRUE)
  })
  return(dirs)
}

#### HELPERS ####

#' @param named_data a named list where the names are the tsv/txt files to write
#'   and the values are the data to write. no file paths, just file names
#' @param zipfile path and filename of the zip file to write
write_zipfile <- function(named_data, zipfile) {
  # go to the temp directory to create intermediate files
  old.dir <- setwd(tempdir())
  on.exit(setwd(old.dir))
  
  # write the data object[s] to file[s]
  data_files <- sapply(names(named_data), function(dataname) {
    file_type <- tools::file_ext(dataname)
    if(file_type == 'txt') {
      # this code block is pretty specialized - only expecting errors.txt and
      # warnings.txt for the model release, and I want double line breaks
      # between messages because some messages already contain '\n'
      data_text <- named_data[[dataname]]
      if(length(data_text) > 0) {
        readr::write_lines(paste(data_text, collapse="\n\n"), path=dataname)
        return(dataname)
      }
    } else if(file_type == 'tsv') {
      data_df <- named_data[[dataname]]
      readr::write_tsv(data_df, path=dataname)
      return(dataname)
    } else {
      stop('unrecognized file extension')
    }
    return(NA)
  })
  data_files <- unname(c(na.omit(data_files)))
  
  # compress
  zip(zipfile = basename(zipfile), files = data_files)
  
  # move the zip files from the temp dir into a more permanent cache location
  setwd(old.dir)
  file.copy(file.path(tempdir(), basename(zipfile)), zipfile)
  
  # clean up the tempdir in case we're running a lot of these
  file.remove(file.path(tempdir(), c(data_files, basename(zipfile))))
  
  return(zipfile)
}

#### MODELS ####

create_model_info_task_plan <- function(metab.config, folders) {

  # define the tasks as unique IDs for each model
  model_titles <- make_metab_run_title(
    format(as.Date(metab.config$date), '%y%m%d'), metab.config$tag, metab.config$strategy)
  model_names <- make_metab_model_name(
    model_titles, metab.config$config.row, metab.config$site)
  
  # temporary truncation for testing
  model_names <- model_names[c(1:6,18:19)]
  
  # define variables to be used by several steps or functions
  download_paths <- mda.streams::make_metab_model_path(
    model_name=model_names, folder=folders$model) %>%
    setNames(model_names)
  model_short_names <- parse_metab_model_name(model_names, out=c('site','strategy')) %>%
    mutate(release_name=paste0(site, '_', substring(strategy, 7))) %>%
    pull(release_name) %>%
    setNames(model_names)
  newline <- "\n      "
  
  download <- create_task_step(
    step_name = 'download',
    target = function(task_name, step_name, ...) {
      sprintf("'%s'", download_paths[[task_name]])
    },
    command = function(task_name, ...) {
      model_path <- download_paths[[task_name]]
      model_name <- task_name
      sprintf(
        "download_model(model_name=I('%s'), model_folder=I('%s'))",
        model_name, folders$model)
    }
  )
  inputs <- create_task_step(
    step_name = 'inputs',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s_input.rds'", folders$prep, model_short_names[[task_name]])
    },
    command = function(task_name, ...) {
      sprintf("extract_model_inputs('%s', target_name)", download_paths[[task_name]])
    }
  )
  dailies <- create_task_step(
    step_name = 'dailies',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s_daily.rds'", folders$prep, model_short_names[[task_name]])
    },
    command = function(task_name, ...) {
      sprintf("extract_model_dailies('%s', target_name)", download_paths[[task_name]])
    }
  )
  fits <- create_task_step(
    step_name = 'fits',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s_fit.zip'", folders$post, model_short_names[[task_name]])
    },
    command = function(task_name, ...) {
      sprintf("extract_model_fits('%s', target_name)", download_paths[[task_name]])
    }
  )
  diagnostics <- create_task_step(
    step_name = 'diagnostics',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s_diagnostics.rds'", folders$prep, model_short_names[[task_name]])
    },
    command = function(task_name, ...) {
      sprintf("extract_model_diagnostics('%s', target_name)", download_paths[[task_name]])
    }
  )

  task_plan <- create_task_plan(
    model_names, list(download, inputs, dailies, fits), #diagnostics
    final_steps=c('inputs','dailies','fits'), indicator_dir=folders$log)
}

create_model_info_makefile <- function(makefile, task_plan, template_file='../lib/task_makefile.mustache') {
  ind_dir <- '../4_data_release/log'
  if(!dir.exists(ind_dir)) dir.create(ind_dir)
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    indicator_dir=dirname(attr(task_plan, 'indicator_dir')),
    include='4_release_models.yml',
    packages=c('mda.streams', 'streamMetabolizer', 'dplyr', 'readr'),
    file_extensions=c('ind','RData'),
    template_file=template_file)
}

#' Build a job target by looping over individual tasks
#'
#' Attempts all steps in a task before moving on to the next task. Especially
#' useful if intermediate files are created and deleted over several steps
#' within a task, and if those files would take up too much space if
#' intermediate files from one step of all tasks were simultaneously present
#'
#' @param job_target overall job target from task_makefile to build
#' @param task_plan task plan as created by `create_task_plan()`
#' @param task_makefile
#' @param num_tries integer number of times to retry looping through all
#'   remaining tasks
#' @param sleep_on_error integer number of seconds to sleep immediately after a
#'   failed task. Especially useful if the error was likely to be inconsistent
#'   (e.g., a temporary network issue) and might not occur again if we wait a
#'   while
loop_model_tasks <- function(job_target='4e_model_info', task_plan, task_makefile='4e_model_info.yml',
                             num_tries=30, sleep_on_error=0) {
  # identify the list of targets that might need to be run
  final_steps <- attr(task_plan, "final_steps")
  task_targets <- gsub("'", "", sapply(unname(task_plan), function(task) {
    sapply(unname(task$steps[final_steps]), `[[`, 'target_name')
  }))
  # run the targets in a loop, with retries, so that we complete (or skip) one
  # task before trying the next. If we just ran remake_smu(job_target,
  # task_makefile) right away, remake would try to build the first step for all
  # tasks before proceeding to the second step for any task
  this_try <- 1
  while(this_try <= num_tries) {
    # identify remaining needs based on the presence or absence of an indicator
    # file. if the file exists, don't bother. this is much quicker than asking
    # remake to rehash every model file to check for changes before we even get
    # started. if we somehow messed up and let an indicator file get out of
    # date, remake will catch it in the final calls of this function when we run
    # remake on the entire job, properly.
    incomplete_targets <- which(!file.exists(task_targets))
    num_targets <- length(incomplete_targets)
    if(num_targets == 0) break
    message(sprintf("\n### Starting loop attempt %s of %s for %s remaining tasks:", this_try, num_tries, num_targets))
    this_try <- this_try + 1
    for(i in seq_len(num_targets)) {
      tryCatch({
        task_number <- incomplete_targets[i]
        task_target <- task_targets[task_number]
        message(sprintf("Building task #%s (%s of %s): %s", task_number, i, num_targets, task_target))
        suppressMessages(remake_smu(task_target, task_makefile, verbose=FALSE)) # verbose=FALSE isn't quiet enough, so also suppressMessages
      }, error=function(e) {
        message(sprintf("  Error in %s : %s", deparse(e$call), e$message))
        message(sprintf("  Skipping task #%s due to error", incomplete_targets[i]))
        # sleep for a while if requested
        if(sleep_on_error > 0) {
          Sys.sleep(sleep_on_error)
        }
      })
    }
  }
  # check the indicator files one last time; if we didn't make it this far, don't try remaking the entire job
  incomplete_targets <- which(!file.exists(task_targets))
  num_targets <- length(incomplete_targets)
  if(num_targets > 0) {
    stop(sprintf("All tries are exhausted, but %s tasks remain", num_targets))
  }
  # if we've made it this far, remake everything to ensure we're done and to
  # write the job indicator file. this will take a few minutes because remake
  # will check the hashes of every file (the big model files take the longest)
  remake_smu(job_target, task_makefile)
}

download_model <- function(model_name, model_folder) {
  mda.streams::login_sb()
  download_metab_model(
    model_name=model_name, folder=model_folder, version='original',
    on_local_exists='skip') #'skip' is a lot faster, safe because any new models would have new names on SB
}

extract_model_inputs <- function(mm_path, inputs_path) {
  # extract the data
  path_vars <- load(mm_path)
  dat <- get_data(mm)
  
  # write the output to rds
  saveRDS(dat, file=inputs_path)
}

extract_model_dailies <- function(mm_path, inputs_path) {
  # extract the data, munge
  path_vars <- load(mm_path)
  site <- get_info(mm)$config$site
  resolution <- substring(get_info(mm)$config$strategy, 7)
  dailies <- get_fit(mm)$daily %>%
    mutate(site_name=site, resolution) %>%
    filter(valid_day) %>%
    select(
      site_name, resolution, date, 
      GPP=GPP_daily_50pct, GPP.lower=GPP_daily_2.5pct, GPP.upper=GPP_daily_97.5pct, GPP.n_eff=GPP_daily_n_eff, GPP.Rhat=GPP_daily_Rhat,
      ER=ER_daily_50pct, ER.lower=ER_daily_2.5pct, ER.upper=ER_daily_97.5pct, ER.n_eff=ER_daily_n_eff, ER.Rhat=ER_daily_Rhat,
      K600=K600_daily_50pct, K600.lower=K600_daily_2.5pct, K600.upper=K600_daily_97.5pct, K600.n_eff=K600_daily_n_eff, K600.Rhat=K600_daily_Rhat,
      warnings)
  
  # write the output to rds
  saveRDS(dailies, file=inputs_path)
}

extract_model_fits <- function(mm_path, fit_path) {
  # extract the data
  path_vars <- load(mm_path)
  site <- get_info(mm)$config$site
  fit <- get_fit(mm)
  
  fit_files <- paste0(names(fit), '.', ifelse(names(fit) %in% c('warnings','errors'), 'txt', 'tsv'))
  fit_data <- setNames(fit, fit_files)
  write_zipfile(fit_data, zipfile=fit_path)
  
  # post
  # append_release_files(parent.id, basename(inputs_path))
}

extract_model_diagnostics <- function(mm_path, inputs_path) {
  # # extract the data
  # path_vars <- load(mm_path)
  # site <- get_info(mm)$config$site
  # fit <- get_fit(mm)$daily
  # #diagnostics <- ...
  # 
  # # write the output to tsv
  # # readr::write_tsv(diagnostics, path=inputs_path)
}

#### SITES ####

create_model_sites_task_plan <- function(metab.config, folders) {
  
  # define the tasks as unique IDs for each model
  sites <- unique(metab.config$site)
  
  # temporary truncation for testing
  sites <- c('nwis_01548303','nwis_03293000','nwis_01473500')
  
  # define model info, named by site
  model_titles <- make_metab_run_title(
    format(as.Date(metab.config$date), '%y%m%d'), metab.config$tag, metab.config$strategy)
  model_names <- make_metab_model_name(
    model_titles, metab.config$config.row, metab.config$site) %>%
    setNames(metab.config$site)
  model_short_names <- parse_metab_model_name(model_names, out=c('site','strategy')) %>%
    mutate(release_name=paste0(site, '_', substring(strategy, 7))) %>%
    pull(release_name) %>%
    setNames(metab.config$site)
  newline <- "\n      "

  # define the steps
  inputs <- create_task_step(
    step_name = 'inputs',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s_input.zip'", folders$post, task_name)
    },
    command = function(task_name, ...) {
      site_models <- model_short_names[names(model_short_names) == task_name]
      site_model_files <- sprintf("I('%s/%s_input.rds')", folders$prep, site_models)
      sprintf("combine_site_inputs(target_name,%s)", paste0(newline, site_model_files, collapse=', '))
    }
  )

  # define the task plan
  task_plan <- create_task_plan(
    sites, list(inputs),
    final_steps=c('inputs'), add_complete=FALSE, indicator_dir=folders$log)
}

create_model_sites_makefile <- function(makefile, task_plan, template_file='../lib/task_makefile.mustache') {
  ind_dir <- '../4_data_release/log'
  if(!dir.exists(ind_dir)) dir.create(ind_dir)
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    indicator_dir=dirname(attr(task_plan, 'indicator_dir')),
    include='4_release_models.yml',
    packages=c('mda.streams', 'streamMetabolizer', 'dplyr', 'readr'),
    file_extensions=c('ind','RData'),
    template_file=template_file)
}

combine_site_inputs <- function(inputs_path, ...) {
  # Read the input files and pare down to just the 7 usual input columns
  input_files <- list(...)
  inputs <- lapply(input_files, function(infi) {
    readRDS(infi) %>%
      select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  })

  # I'm pretty confident that the inputs are the same for all models...but do a
  # check to confirm
  if(!all(sapply(seq_len(length(inputs)-1)+1, function(i) {
    all.equal(inputs[[1]], inputs[[i]])
  }))) {
    stop("all input files are not equal")
  }
  
  # Write out the first input to file. No need to merge because inputs[[1]] is
  # identical to all the others or is the only one
  tsv_path <- gsub('\\.zip$', '.tsv', basename(inputs_path))
  write_zipfile(setNames(list(inputs[[1]]), tsv_path), zipfile=inputs_path)
  
  # post
  # append_release_files(parent.id, inputs_path)
}
