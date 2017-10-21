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
  # model_names <- model_names[c(1:6,18:19)]
  
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
    model_names, list(download, inputs, dailies, fits, diagnostics),
    final_steps=c('inputs','dailies','fits','diagnostics'), indicator_dir=folders$log)
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

download_model <- function(model_name, model_folder) {
  mda.streams::login_sb()
  download_metab_model(
    model_name=model_name, folder=model_folder, version='original',
    on_local_exists='skip') #'skip' is a lot faster, safe because any new models would have new names on SB
  unlink(file.path(tempdir(), '4_data_release'), recursive=TRUE)
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
}

extract_model_diagnostics <- function(mm_path, out_file) {
  # extract the data
  path_vars <- load(mm_path)
  site <- get_info(mm)$config$site
  resolution <- substring(get_info(mm)$config$strategy, 7)
  fit <- get_fit(mm)
  
  diagnostics <- data_frame(
    site = site,
    resolution = resolution,
    K600_daily_sigma_Rhat = fit$KQ_overall$K600_daily_sigma_Rhat,
    err_obs_iid_sigma_Rhat = fit$overall$err_obs_iid_sigma_Rhat,
    err_proc_iid_sigma_Rhat = fit$overall$err_proc_iid_sigma_Rhat,
    K_median = quantile(fit$daily$K600_daily_50pct, 0.5, na.rm = T),
    K_range = 
      quantile(fit$daily$K600_daily_50pct, 0.9, na.rm = T) -
      quantile(fit$daily$K600_daily_50pct, 0.1, na.rm = T),
    neg_GPP = 100 *
      length(which(fit$daily$GPP_daily_50pct < -0.5)) /
      length(which(!is.na(fit$daily$GPP_daily_50pct))),
    pos_ER = 100 *
      length(which(fit$daily$ER_daily_50pct > 0.5)) /
      length(which(!is.na(fit$daily$ER_daily_50pct))))
  
  # write the output to rds
  saveRDS(diagnostics, file=out_file)
}

#### SITES ####

create_model_sites_task_plan <- function(metab.config, folders, inputs_item, fits_item) {
  
  # define the tasks as unique IDs for each model
  sites <- unique(metab.config$site)
  
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
    target_name = function(task_name, step_name, ...) {
      sprintf("'%s/%s_input.zip'", folders$post, task_name)
    },
    command = function(task_name, ...) {
      site_models <- model_short_names[names(model_short_names) == task_name]
      site_model_files <- sprintf("'%s/%s_input.rds'", folders$prep, site_models)
      sprintf("combine_site_inputs(target_name,%s)", paste0(newline, site_model_files, collapse=', '))
    }
  )
  post_inputs <- create_task_step(
    step_name = 'post_inputs',
    # use default target: task_step
    command = function(task_name, step_name, steps, ...) {
      sprintf("create_release_item(%sparent.id=%s,%skey=I('%s'),%s)",
              newline,
              parent.id = inputs_item, newline,
              key = paste0(task_name, "_input"),
              files = paste0(newline, steps$inputs$target_name, collapse=', '))
    }
  )
  post_fits <- create_task_step(
    step_name = 'post_fits',
    # use default target: task_step
    command = function(task_name, step_name, steps, ...) {
      fit_files <- dir(folders$post, pattern=paste0(task_name, '_[[:digit:]]+min_fit.zip'), full.names=TRUE)
      sprintf("create_release_item(%sparent.id=%s,%skey=I('%s'),%s)",
              newline,
              parent.id = fits_item, newline,
              key = paste0(task_name, "_fits"),
              files = paste0(newline, "'", fit_files, "'", collapse=', '))
    }
  )
  
  # define the task plan
  task_plan <- create_task_plan(
    sites, list(inputs, post_inputs, post_fits),
    final_steps=c('post_inputs','post_fits'), add_complete=FALSE, indicator_dir=folders$log)
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

combine_model_dailies <- function(out_file, task_plan) {
  # identify the expected and available daily prediction files to combine
  targets <- gsub("'", "", sapply(unname(task_plan), function(task) {
    task$steps$dailies$target_name
  }))
  target_dir <- unique(sapply(targets, dirname))
  targets <- basename(targets)
  prepped_files <- dir(target_dir)
  
  # pick the targets that also exist as files. make a small stink if expected
  # files don't exist, but not too big because we want to be able to test this
  # function on subsets of the complete set of models
  existing_targets <- intersect(targets, prepped_files)
  missing_targets <- setdiff(targets, prepped_files)
  if(length(missing_targets) > 0) {
    warning(paste("these daily.rds files don't yet exist:", paste0(missing_targets, collapse=', ')))
  }
  
  # combine all the data into one big data.frame
  all_dailies <- bind_rows(lapply(file.path(target_dir, existing_targets), readRDS))
  
  # TODO add in daily average depth, discharge, temperature, light, and velocity (and daylength?)
  
  
  
  # write the combined daily predictions to a file with the same name (other
  # than extension) as the zip file that will contain it. write the zip, too
  tsv_path <- gsub('\\.zip$', '.tsv', basename(out_file))
  write_zipfile(setNames(list(all_dailies), tsv_path), zipfile=out_file)
  
  # post
  # append_release_files(parent.id, out_file)
}

combine_model_diagnostics <- function(out_file, task_plan) {
  # identify the expected and available daily prediction files to combine
  targets <- gsub("'", "", sapply(unname(task_plan), function(task) {
    task$steps$diagnostics$target_name
  }))
  target_dir <- unique(sapply(targets, dirname))
  targets <- basename(targets)
  prepped_files <- dir(target_dir)
  
  # pick the targets that also exist as files. make a small stink if expected
  # files don't exist, but not too big because we want to be able to test this
  # function on subsets of the complete set of models
  existing_targets <- intersect(targets, prepped_files)
  missing_targets <- setdiff(targets, prepped_files)
  if(length(missing_targets) > 0) {
    warning(paste("these diagnostics.rds files don't yet exist:", paste0(missing_targets, collapse=', ')))
  }
  
  # combine all the data into one big data.frame
  all_diagnostics <- bind_rows(lapply(file.path(target_dir, existing_targets), readRDS))
  
  # assess models based on these diagnostics
  all_diagnostics <- all_diagnostics %>%
    mutate(
      model_confidence = ifelse(
        K600_daily_sigma_Rhat > 1.2, "L", ifelse(
          err_obs_iid_sigma_Rhat > 1.2, "L", ifelse(
            err_proc_iid_sigma_Rhat > 1.2, "L", ifelse(
              K_range > 50, "L", ifelse(
                neg_GPP > 50, "L", ifelse(
                  pos_ER > 50, "L", ifelse(
                    K_range > 15, "M", ifelse(
                      neg_GPP > 25, "M", ifelse(
                        pos_ER > 25, "M", "H"
                      ))))))))))
  
  # determine a site-level assessment (possibly combining several models)
  all_diagnostics <- all_diagnostics %>%
    mutate(model_confidence = ordered(model_confidence, levels=c('L','M','H'))) %>%
    group_by(site) %>%
    mutate(
      site_min_confidence = min(model_confidence),
      site_confidence = paste0(sort(unique(model_confidence)), collapse=',')) %>%
    ungroup()
  
  # write and zip the assessment table
  tsv_path <- gsub('\\.zip$', '.tsv', basename(out_file))
  write_zipfile(setNames(list(all_diagnostics), tsv_path), out_file)
}
