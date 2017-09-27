# These functions are a first draft of the scipiper function. Let's see how it
# goes.

#' Create a .yml makefile for a multi-task job
#'
#' Create a .yml makefile (for use with remake or scipiper) for a set of tasks
#' that together form a single job
#'
#' @param task_plan a task plan as produced by `create_task_plan()`
#' @param job_target single character string naming the default target, which
#'   will include all tasks within the job
#' @param include character vector of any remake .yml files to include within
#'   this one. If any files must be quoted in the remake file, quote them with
#'   inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param packages character vector of any packages to load before running steps
#' @param sources character vector of any files that should be sourced before
#'   running steps. If any files must be quoted in the remake file, quote them
#'   with inner single quotes, e.g. `c("unquoted", "'quoted file name.tsv'")`
#' @param makefile character name of the remake file to create, or NULL to
#'   simply return the output as a long character string (can be displayed in
#'   console with `cat()`)
#' @param template_file character name of the mustache template to render into
#'   `makefile`. The default is recommended
#' @return if `makefile==NULL`, the output as a long character string (can be
#'   displayed with `cat()`). If `makefile` is a file name, the file name (can
#'   be displayed with `readLines()`).
#' @export
#' @examples
#' step1 <- create_task_step(
#'   step_name = 'prep',
#'   target = function(task_name, step_name, ...) {
#'     sprintf('%s_%s', task_name, step_name)
#'   },
#'   depends = c('A','B'),
#'   command = "process(target_name, I('C'))"
#' )
#' step2 <- create_task_step(
#'   step_name = 'plot',
#'   command = function(target_name, task_name, ...) {
#'     sprintf('visualize(\'%s\')', task_name)
#'   }
#' )
#' step3 <- create_task_step('report')
#' task_plan <- create_task_plan(c('AZ','CA','CO'), list(step1, step2, step3))
#' create_task_makefile(task_plan, packages='mda.streams')
create_task_makefile <- function(task_plan, job_target = 'all', 
                                 include=c(), packages=c(), sources=c(),
                                 makefile=NULL, template_file='../lib/task_makefile.mustache') {
  
  # prepare the overall job task: list every step of every job as a dependency
  job <- list(
    target_name = job_target,
    depends = unlist(lapply(task_plan, function(task) lapply(task$steps, function(step) step$target_name)), use.names=FALSE)
  )
  
  # prepare the task list: remove names where they'd interfere with whisker.render
  tasks <- unname(task_plan)
  for(task in seq_along(tasks)) {
    tasks[[task]]$steps <- unname(tasks[[task]]$steps)
    for(step in seq_along(tasks[[task]]$steps)) {
      tasks[[task]]$steps[[step]]$has_depends <- length(tasks[[task]]$steps[[step]]$depends) > 0
    }
  }
  
  # prepare the final list of variables to be rendered in the template
  params <- list(
    job = job,
    target_default = overall_target,
    include = include,
    has_include = length(include) > 0,
    packages = packages,
    has_packages = length(packages) > 0,
    sources = sources,
    has_sources = length(sources) > 0,
    tasks = tasks
  )
  
  # read the template
  template <- readLines(template_file)
  
  # render the template
  yml <- whisker::whisker.render(template, data=params)
  yml <- gsub('[\n]{3,}', '\\\n\\\n', yml) # reduce 3+ line breaks to just 2
  
  # write and/or return the results
  if(!is.null(makefile)) {
    cat(yml, file=makefile)
    return(makefile)
  } else {
    return(yml)
  }
}

#' Convert a task_plan into a status table
#'
#' Create a data.frame or .tsv file representing a task_plan and our status
#' within that plan
#' @param task_plan a task plan as produced by `create_task_plan()`
#' @param table_file a file name to write a tab-separated table to (as .tsv), or
#'   NULL to return as a data.frame
#' @export
create_task_table <- function(task_plan, table_file) {
  stop("sorry, not implemented yet")
}

#' Define a set of tasks and steps within a single job
#'
#' Create a structured list that organizes tasks and their sub-tasks (steps) for
#' a large number of near-identical tasks
#'
#' @param task_names character vector of IDs for sets of work coordinates, with
#'   length equal to the number of tasks. You will usually use these character
#'   strings within the names for each step in each task. These names should
#'   therefore describe the aspect/aspects of a task that position it within the
#'   larger parameter space of work to be done. Examples: site IDs, model names,
#'   or character code identifying a unique row in a configuration file or
#'   data.frame.
#' @param task_steps list of definitions steps to perform within each task. Each
#'   step definition should be created by `create_task_step()`
#' @return a structured list that can be passed to `create_task_makefile` or
#'   `create_task_table`
#' @export
#' @examples
#' step1 <- create_task_step(
#'   step_name = 'prep',
#'   target = function(task_name, step_name, ...) {
#'     sprintf('%s_%s', task_name, step_name)
#'   },
#'   depends = c('A','B'),
#'   command = "process(target_name, I('C'))"
#' )
#' step2 <- create_task_step(
#'   step_name = 'plot',
#'   command = function(target_name, task_name, ...) {
#'     sprintf('visualize(\'%s\')', task_name)
#'   }
#' )
#' step3 <- create_task_step('report')
#' task_plan <- create_task_plan(c('AZ','CA','CO'), list(step1, step2, step3))
create_task_plan <- function(task_names, task_steps) {
  
  # munge the task_names into a named list
  task_names <- if(is.data.frame(task_names)) {
    whisker::rowSplit(task_names)
  } else if(is.list(task_names)) {
    task_names
  } else if(is.character(task_names)) {
    setNames(as.list(rep(NA, length(task_names))), task_names)
  }
  
  # check that task_steps is a list of task_steps
  if(!is.list(task_steps)) {
    stop('task_steps must be a list')
  } else {
    if(any(!sapply(task_steps, function(ts) is(ts, 'task_step')))) {
      stop('task_steps must be a list of task_step objects (see ?create_task_step)')
    }
  }
  
  # prepare a list of the tasks and steps
  tasks <- list()
  for(i in seq_along(task_names)) {
    task <- list(
      task_name = names(task_names)[i],
      steps = list()
    )
    for(j in seq_along(task_steps)) {
      # isolate just the definitions for this step
      step_def <- task_steps[[j]]
      
      # every step is a list with step_name as the first element
      step <- list()
      step$step_name = step_def$step_name
      
      # use the user's step definitions to define the recipe
      step$target_name <- evaluate_step_element(step_def, 'target_name', task, step)
      step$depends <- evaluate_step_element(step_def, 'depends', task, step)
      step$command <- evaluate_step_element(step_def, 'command', task, step)
      
      # add a T/F to help whisker avoid empty depends blocks. move this to the makefile function soon
      step$has_depends <- length(step$depends) > 0
      
      # add this step to the task. wait until now to append because easier to
      # type/read 'step' than 'task$steps[[j]]' above
      task$steps[[step$step_name]] <- step
    }
    # append task to full tasks list. wait until now to append because faster
    # when task_names is a long list
    tasks[[task$task_name]] <- task
  }
  
  return(tasks)
}

#' Create an object that defines a step within a task
#'
#' The default values of each parameter are often acceptable, but all parameters
#' may be overridden. When constructing the task makefile or table, the
#' `target_name`, `depends`, and `command` elements are built in that order,
#' with each element optionally depending on the result of the previous
#' elements. They can also depend on the `step_name` (defined in this function
#' call) and/or the `task_name` (to be listed in a call to `create_task_plan()`,
#' where the definitions declared here will ultimately be evaluated)
#'
#' @param step_name a single character string naming this step. The default
#'   `target_name` combines this `step_name` with the `task_name`, and the
#'   default `command` assumes this `step_name` is the function name, but both
#'   defaults may be overridden (see next arguments)
#' @param target_name a character string or vector, or a function that produces
#'   a character string or vector, giving a unique name for the remake target
#'   for a specific application of this step to a specific task. If a function,
#'   should accept `...` and other args optionally including `task_name` and
#'   `step_name`
#' @param depends a character string or vector, or a function that produces a
#'   character string or vector, defining any dependencies that need to be
#'   declared in addition to those implied by `command`. If a function, should
#'   accept `...` and other args optionally including `task_name`, `step_name`,
#'   and `target_name` args optionally including `task_name` and `step_name`
#' @param command a character string or vector, or a function that produces a
#'   character string or vector, defining the command to be run for each
#'   application of this step to a specific task. If a function, should accept
#'   `...` and other args optionally including `task_name`, `step_name`,
#'   `target_name`, and `depends`
#' @md
#' @export
#' @examples
#' create_task_step(
#'   'plot',
#'   target_name=function(task_name, step_name, ...) {
#'     sprintf('~/MyProjects/thisproject/%s_%s.png', task_name, step_name)
#'   },
#'   command='plot_site(target_name)'
#' )
create_task_step <- function(
  step_name,
  target_name = function(task_name, step_name, ...) {
    sprintf('%s_%s', task_name, step_name)
  },
  depends = character(0),
  command = function(task_name, step_name, ...) {
    sprintf("%s('%s')", step_name, task_name)
  }
) {
  
  # create the step definition
  step_def <- list(step_name=step_name, target_name=target_name, depends=depends, command=command)
  class(step_def) <- 'task_step'
  
  # check the inputs for proper formatting
  for(elem_name in names(step_def)) {
    element <- step_def[[elem_name]]
    if(is.character(element)) {
      # this is fine
    } else if(is.function(element)) {
      # check that the arguments are as expected
      given_args <- names(formals(element))
      expected_args <- c('...','task_name','step_name','target_name','depends')
      unknown_args <- setdiff(given_args, expected_args)
      if(length(unknown_args) > 0) {
        stop(paste0("unknown argument name[s] ",paste("'",unknown_args,"'", collapse=", ")," in '", elem_name, "' element"))
      }
      if(!('...' %in% given_args)) {
        stop(paste0("'...' must be included in arguments to ", elem_name))
      }
    } else {
      stop('each element must be either a character string/vector or a function')
    }
  }
  
  return(step_def)
}

#' Evaluate a step element into simple strings
#'
#' Evaluate a step element (target, depends, command) as a character
#' string/vector or as a function of the task and step elements (depending on
#' the class of the element)
#'
#' @param task_step a task_step as created by `create_task_step()`
#' @param element character naming a task_step element ('target_name',
#'   'depends', or 'command') to evaluate
#' @param task a task item: a list with fields for 'task_name' and possibly
#'   other info
#' @param step a step item: a list with fields for 'step_name' and possibly
#'   other info
#' @keywords internal
evaluate_step_element <- function(task_step, element, task, step) {
  # extract the desired task-step element (tse)
  if(!element %in% names(task_step)) {
    stop("task_step does not define the element '", element, "'")
  }
  tse <- task_step[[element]]
  
  # evaluate the tse according to its class
  if(is.character(tse)) {
    out <- tse
  } else if(is.function(tse)) {
    args <- c(task, step) # combine all available task-step info into a single list
    out <- do.call(tse, args) # apply tse as a function of that info
  } else {
    out <- tse # but we'll throw a warning below
  }
  
  if(!is.character(out)) {
    warning("output isn't character - that's probably bad")
  }
  if(any(grepl('"', out))) {
    warning('in task=', task$task_name,
            ', step=', step$step_name,
            ', "',element , '":',
            ' use \' instead of " to avoid html escaping')
  }
  return(out)
}
