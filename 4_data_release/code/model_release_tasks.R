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