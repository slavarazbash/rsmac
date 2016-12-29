library(dplyr)
source('R/runner.R')

#' Launch smac minimization
#' 
#' @objective: The objective function that should be optimized.
#' Designed for objective functions that are:
#'   costly to calculate + don't have a derivative available.
#' @x0: initial guess. xmin < x0 < xmax
#' @xmin: minimum values 
#' @xmax: maximum values
#' @x0_int: initial guess of integer params
#' @xmin_int: minimum values of integer params
#' @xmax_int: maximum values of integer params
#' @x_categorical: dictionary of categorical parameters
#' @custom_args: a dict of custom arguments to the objective function
#' @max_evaluations: the maximum number of evaluations to execute
#' @seed: the seed that SMAC is initialized with
#' @cv_folds: set if you want to use cross-validation. The objective function will 
#' get an new `cv_fold` argument.
#' @update_status_every: the number of num_evaluationss, between status updates
#' @rf_num_trees: number of trees to create in random forest.
#' @rf_full_tree_bootstrap: bootstrap all data points into trees.
#' @intensif_percent: percent of time to spend intensifying versus model learning.
#' 
#' @return: best parameters found
minimize <- function(objective,
                     x0=c(), xmin=c(), xmax=c(),
                     x0_int=c(), xmin_int=c(), xmax_int=c(),
                     x_categorical=c(),
                     custom_args=c(), max_evaluations=100, seed=1,
                     cv_folds=NULL, update_status_every=500,
                     rf_num_trees=100,
                     rf_full_tree_bootstrap=TRUE,
                     intensif_percent=0) {

  stopifnot(length(x0) == length(xmin) && length(x0) == length(xmax))
  stopifnot(length(x0_int) == length(xmin_int) && length(x0_int) == length(xmax_int))
  for (el in x_categorical) stopifnot(is.list(el))
  check_java_version()
  if (!is.null(cv_folds) && cv_folds == 1) cv_folds <- NULL
  
  browser()
  smacRemoteCon <- get_remote_socket_con()
  xs_quoted <- quote(list(x0, xmin, xmax, x0_int, xmin_int, xmax_int, x_categorical))
  xs <- eval(xs_quoted)
  names(xs) <- sapply(2:length(xs_quoted), function(x) as.character(xs_quoted[x]))
  paths <- generate_files(cutoff_time=86400, cv_folds, xs)
  # smacRunner <- SMACRunner
  start_smac(paths, smac_version, port)
  
  current_fmin <- NULL
  num_evaluations <- 0
  stopifnot(!is_finished(smacRunner))
  
  while (!is_finished(smacRunner)) {
    tryCatch(
      param_str <- get_next_smac_input(smacRemoteCon),
      error=function(e) { browser(); next })
    
    params <- parse_smac_param_string(param_str)
    fold <- parse_smac_cv_fold(param_str)
    
    start <- Sys.time()
    stopifnot(all(sapply(names(params), 
                         function(parName) !parName %in% names(custom_args))))
    objective_args <- c(params, custom_args)
    if (!is.null(cv_folds)) objective_args["cv_fold"] <- fold
    
    runRes <- do.call(objective, objective_args)
    num_evaluations <- num_evaluations + 1
    
    if (is.null(run_result) || !is.numeric(run_result)) {
      stop("objective function returned incorrect result [", run_result, "] for pars: ", 
           objective_args)
    }
    
    run_result_is_better <- is.null(current_fmin) || (run_result < current_fmin)
    if (run_result_is_better) current_fmin <- run_result
    
    if (run_result_is_better || (num_evaluations %% update_status_every) == 0) {
      cat("Number of evaluations ", num_evaluations, 
          ", current fmin: ", current_fmin, "\n")
    }
    report_performance(smacRemoteCon, run_result, Sys.time() - start)
    # except KeyboardInterrupt:
    #   logging.warn("received keyboard interrupt ... aborting")
    #   smacRunner$stop()
  
    cat("Total number of evaluations ", num_evaluations, 
        ", current fmin: ", current_fmin, "\n")
  }
  
  close(smacRemoteCon)
  get_best_parameters(paths)
}

