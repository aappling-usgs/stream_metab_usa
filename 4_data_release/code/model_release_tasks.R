create_model_release_task_plan <- function(metab.config) {
  
  # define the tasks as unique IDs for each model
  model_titles <- make_metab_run_title(
    format(as.Date(metab.config$date), '%y%m%d'), metab.config$tag, metab.config$strategy)
  model_names <- make_metab_model_name(
    model_titles, metab.config$config.row, metab.config$site)
  
  # define variables to be used by several steps or functions
  model_folder <- '../4_data_release/cache/models'
  download_paths <- mda.streams::make_metab_model_path(
    model_name=model_names, folder=model_folder) %>%
    setNames(model_names)
  
  if(!dir.exists(model_folder)) dir.create(model_folder)
  download <- create_task_step(
    step_name = 'download',
    target = function(task_name, step_name, ...) {
      sprintf("'%s'", download_paths[[task_name]])
    },
    command = function(task_name, ...) {
      sprintf(
        "download_model(model_name=I('%s'), folder=I('%s'), version=I('original'))",
        download_paths[[task_name]], model_folder)
    }
  )
  inputs <- create_task_step(
    step_name = 'inputs',
    target = function(task_name, step_name, ...) {
      sprintf("'%s/%s.%s.rds'", model_folder, step_name, task_name)
    },
    command = function(task_name, ...) {
      sprintf("extract_model_inputs('%s', target_name)", download_paths[[task_name]])
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
  
  task_plan <- create_task_plan(model_names, list(download, inputs))
}

create_model_release_makefile <- function(makefile, task_plan) {
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    job_target = 'model_release', include = '4_release_models.yml',
    packages=c('mda.streams', 'streamMetabolizer', 'dplyr'),
    file_extensions=c('RData'))
}

download_model <- function(model_name, folder, version) {
  mda.streams::login_sb()
  download_metab_model(model_name=model_name, folder=folder, version=version)
}

extract_model_inputs <- function(mm_path, inputs_path) {
  path_vars <- load(mm_path)
  dat <- get_data(mm)
  write.table(dat, file=inputs_path, sep='\n', row.names=FALSE)
}