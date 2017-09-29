create_model_release_task_plan <- function(metab.config) {

  # define & create model_folder & indicator_folder as needed. wait to create
  # them until we're within a function because when remake sources this file,
  # our working directory will be set to the directory of this file, and that's
  # confusing
  model_folder <- '../4_data_release/cache/models'
  indicator_folder <- '../4_data_release/log/models'
  if(!dir.exists(model_folder)) dir.create(model_folder)
  if(!dir.exists(indicator_folder)) dir.create(indicator_folder)
  
  # define the tasks as unique IDs for each model
  model_titles <- make_metab_run_title(
    format(as.Date(metab.config$date), '%y%m%d'), metab.config$tag, metab.config$strategy)
  model_names <- make_metab_model_name(
    model_titles, metab.config$config.row, metab.config$site)
  
  # temporary truncation for testing
  model_names <- model_names[1:6]
  
  # define variables to be used by several steps or functions
  download_paths <- mda.streams::make_metab_model_path(
    model_name=model_names, folder=model_folder) %>%
    setNames(model_names)
  
  download <- create_task_step(
    step_name = 'download',
    target = function(task_name, step_name, ...) {
      sprintf("'%s'", download_paths[[task_name]])
    },
    command = function(task_name, ...) {
      model_path <- download_paths[[task_name]]
      model_name <- parse_metab_model_path(model_path, out='model_name')
      sprintf(
        "download_model(model_name=I('%s'), model_folder=I('%s'))",
        model_name, model_folder)
    }
  )
  inputs <- create_task_step(
    step_name = 'inputs',
    target = function(task_name, step_name, ...) {
      inputs_release_name <- parse_metab_model_name(task_name, out=c('site','strategy')) %>%
        mutate(release_name=paste0(site, '_', substring(strategy, 7), '_inputs')) %>%
        pull(release_name)
      sprintf("'%s/%s.tsv'", model_folder, inputs_release_name)
    },
    command = function(task_name, ...) {
      newline <- "\n      "
      sprintf("extract_model_inputs(%s'%s',%starget_name)", newline, download_paths[[task_name]], newline)
    }
  )
  # dailies <- create_task_step(
  #   step_name = 'dailies',
  #   target = target_fun
  # )
  # fits <- create_task_step(
  #   step_name = 'fits',
  #   target = target_fun
  # )
  # diagnostics <- create_task_step(
  #   step_name = 'diagnostics',
  #   target = target_fun
  # )
  
  task_plan <- create_task_plan(
    model_names, list(download, inputs),
    final_steps='inputs', indicator_dir=indicator_folder)
}

create_model_release_makefile <- function(makefile, task_plan, template_file='../lib/task_makefile.mustache') {
  ind_dir <- '../4_data_release/log'
  if(!dir.exists(ind_dir)) dir.create(ind_dir)
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    indicator_dir=dirname(attr(task_plan, 'indicator_dir')),
    include='4_release_models.yml',
    packages=c('mda.streams', 'streamMetabolizer', 'dplyr'),
    file_extensions=c('ind','RData'),
    template_file=template_file)
}

download_model <- function(model_name, model_folder) {
  mda.streams::login_sb()
  download_metab_model(
    model_name=model_name, folder=model_folder, version='original',
    on_local_exists='skip') #'skip' is a lot faster, safe because any new models would have new names on SB
}

extract_model_inputs <- function(mm_path, inputs_path) {
  path_vars <- load(mm_path)
  dat <- get_data(mm)
  write.table(dat, file=inputs_path, sep='\t', row.names=FALSE, quote=TRUE)
}

loop_model_tasks <- function(job_target='4e_model_release', task_plan, task_makefile='4e_model_release.yml') {
  # identify the list of targets that might need to be run
  task_targets <- gsub("'", "", sapply(unname(task_plan), function(task) task$steps$complete$target_name))
  # run the targets in a loop, with retries, so that we complete (or skip) one
  # task before trying the next. If we just ran remake_smu(job_target,
  # task_makefile) right away, remake would try to build the first step for all
  # tasks before proceeding to the second step for any task
  total_tries <- 30
  tries <- 1
  while(tries <= total_tries) {
    # identify remaining needs based on the presence or absence of an indicator
    # file. if the file exists, don't bother. this is much quicker than asking
    # remake to rehash every model file to check for changes before we even get
    # started. if we somehow messed up and let an indicator file get out of
    # date, remake will catch it in the final calls of this function when we run
    # remake on the entire job, properly.
    incomplete_targets <- which(!file.exists(task_targets))
    num_targets <- length(incomplete_targets)
    if(num_targets == 0) break
    message(sprintf("### Starting loop attempt %s of %s for %s remaining tasks:", tries, total_tries, num_targets))
    tries <- tries + 1
    for(i in seq_len(num_targets)) {
      tryCatch({
        task_number <- incomplete_targets[i]
        task_target <- task_targets[task_number]
        message(sprintf("building task #%s (%s of %s): %s", task_number, i, num_targets, task_target))
        suppressMessages(remake_smu(task_target, task_makefile, verbose=FALSE)) # verbose=FALSE isn't quiet enough, so also suppressMessages
      }, error=function(e) {
        message(sprintf("  %s", e$message))
        message(sprintf("  skipping task #%s due to error", incomplete_targets[i]))
        # sleep for a while on the assumption that this was an internet error
        # that will disappear eventually
        Sys.sleep(120)
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
