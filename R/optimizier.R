#' Launch smac minimization
#'
#' @param conf smac object configuration. The list of named options:
#' - 't_limit_total_s' the total time budget (in seconds) for the optimization. 
#'     NULL means that no wall clock time constraint is enforced. [float]
#' - 'mem_limit_smac_mb' memory limit for the Java Runtime Environment in which SMAC 
#'     will be executed. NULL means system default. [int]
#' - 'working_directory' directory where SMACs output files are stored. 
#'     NULL means a temporary directory will be created via the tempfile module. [str]
#' - 'persistent_files' whether or note these files persist beyond the runtime 
#'     of the optimization. [bool]
#' - 'debug' set this to TRUE if need debug. [bool]
#' @param func the function to be called
#' @param max_evaluations number of function calls allowed during the optimization 
#' (does not include optional validation)
#' @param parameter_dict parameter configuration space definition
#' @param conditional_clauses list of conditional dependencies between parameters
#' @param forbidden_clauses list of forbidden parameter configurations
#' @param deterministic whether the function to be minimized contains random components
#' @param num_train_instances number of instances used during the configuration/optimizat
#' @param num_test_instances number of instances used for testing/validation
#' @param num_runs number of independent SMAC runs
#' @param num_procs number SMAC runs that can be executed in paralell
#' @param seed seed for SMAC's Random Number generator. 
#' If int, it is used for the first run, additional runs use consecutive numbers. 
#' If list, it specifies a seed for every run
#' @param mem_limit_function_mb sets the memory limit for your function (value in MB). 
#' ``NULL`` means no restriction. 
#' Be aware that this limit is enforced for each SMAC run separately. 
#' So if you have 2 parallel runs, pysmac could use twice that value 
#' (and twice the value of mem_limit_smac_mb) in total. 
#' Note that due to the creation of the subprocess, the amount of memory available to 
#' your function is less than the value specified here. 
#' This option exists mainly to prevent a memory usage of 100% which will at least 
#' slow the system down
#' @param t_limit_function_s cutoff time for a single function call. 
#' ``NULL`` means no restriction. If optimizing run time,
#' SMAC can choose a shorter cutoff than the provided one for individual runs. 
#' If `NULL` was provided, then there is no cutoff ever!
minimize <- function(conf, func, max_evaluations, parameter_dict,
             conditional_clauses = c(), forbidden_clauses=c(),
             deterministic = TRUE,
             num_train_instances = NULL, num_test_instances = NULL,
             train_instance_features = NULL,
             num_runs = 1, num_procs = 1, seed = 0,
             mem_limit_function_mb=NULL, mem_limit_smac_mb=NULL, 
             t_limit_function_s= NULL) {
  
  getSmacWdFilePath <- function(fileName) paste0(getwd(), '/smacwd/', fileName)

  smac_options <- list(
    'algo-exec'= 'echo 0',
    'run-obj'= 'QUALITY',
    'validation'= FALSE,
    'cutoff_time'= 3600,
    'intensification-percentage'= 0.5,
    'numPCA'= 7,
    'rf-full-tree-bootstrap'= FALSE,
    'rf-ignore-conditionality'=FALSE,
    'rf-num-trees'= 10,
    'skip-features'= TRUE,
    'pcs-file'= getSmacWdFilePath('parameters.pcs'),
    'instances'= getSmacWdFilePath('instances.dat'),
    'algo-exec-dir'= self.working_directory,
    'output-dir'= self.__out_dir,
    'console-log-level'= 'OFF',
    'abort-on-first-run-crash'= FALSE,
    'overall_obj'= 'MEAN',
    'scenario_fn'= getSmacWdFilePath('scenario.dat'),  # NOT a SMAC OPTION, but allows to
    # change the standard name
    'java_executable'= 'java',    # NOT a SMAC OPTION; allows to specify a different java 
    # binary and can be abused to pass additional arguments to it 
    'timeout_quality'=2.^127)    # not a SMAC option either customize 
    # the quality reported # to SMAC in case of a timeout
  smac_options[['algo-deterministic']] <- deterministic
  

  # check inputs
  if (!is.null(num_train_instances) && num_train_instances < 1) {
    stop('The number of training instances must be positive!')
  }
  if (!is.null(train_instance_features) && 
      length(train_instance_features) != num_train_instances) {
    stop("You have to provide features for every training instance!")
  }
  for (feature_vector in train_instance_features) {
    if (length(feature_vector) != length(train_instance_features[1])) {
      stop("You have to specify the same number of features for every instance!")
    }
  }
  smac_options[['feature_file']] <- getSmacWdFilePath('features.dat')
  
  source('remote_smac.R')
  ls <- list()
  ls[c('pcs_string', 'parser_dict')] <- process_parameter_definitions(parameter_dict)
  list2env(ls)
  
  seed <- seed:(seed+num_runs - 1)
  
  smac_options[['runcount-limit']] <- max_evaluations
  if (!is.null(t_limit_function_s)) {
    smac_options[['cutoff_time']] <- t_limit_function_s
  }
  
  #create and fill the pcs file
  write(paste(c(ls$pcs_string, conditional_clauses, forbidden_clauses), sep='\n'),
        file=smac_options[['pcs-file']])
  
  #create and fill the instance files
  tmp_num_instances <- if (is.null(num_train_instances)) 1 else num_train_instances
  content <- 0:(tmp_num_instances-1) %>% 
    sapply(function(x) sprintf('id_%s', x)) %>% 
    paste(collapse='\n')
  write(content, file=smac_options[['instances']])
  
  # create and fill the header for the feature file
  if (!is.null(train_instance_features)) {
    # header
    content <- paste(c('instance_name', 
      sapply(0:(length(train_instance_features[1])-1),
             function(x) sprintf('feature%s', x))), collapse=',')
  
    # and then the actual features
    for (i in 0:(length(train_instance_features)-1)) {
      line <- paste(c('id_', i, train_instance_features[i]), collapse=',')
      content <- append(content, line)
    }
    write(content, file=smac_options[['feature_file']])
  }
  
  if (!is.null(num_test_instances)) {
    smac_options[c('validate-only-last-incumbent', 'validation')] <- TRUE
    smac_options[['test-instances']] <- getSmacWdFilePath( 'test_instances.dat')
    content <- sapply(tmp_num_instances:(tmp_num_instances + num_test_instances - 1),
                      function(x) sprintf('id_%s', x))
    write(content, file=smac_options[['test-instances']])
  }
  
  # make sure the java executable is callable and up-to-date
  java_executable <- smac_option[['java_executable']]
  smac_option[['java_executable']] <- NULL
  check_java_version(java_executable)
  
  timeout_quality <- smac_options[['timeout_quality']]
  smac_options[['timeout_quality']] <- NULL
  
  # create and fill the scenario file
  scenario_fn <- getSmacWdFilePath(smac_options[['scenario_fn']])
  smac_options[['scenario_fn']] <- NULL
  
  scenario_options <- c(
    'algo', 'algo-exec', 'algoExec', 'algo-exec-dir', 'exec-dir', 'execDir','execdir',
    'deterministic', 'algo-deterministic', 'paramfile', 'paramFile', 'pcs-file', 
    'param-file', 'run-obj', 'run-objective', 'runObj', 'run_obj', 'intra-obj', 
    'intra-instance-obj', 'overall-obj', 'intraInstanceObj', 'overallObj', 
    'overall_obj', 'intra_instance_obj', 'algo-cutoff-time', 'target-run-cputime-limit', 
    'target_run_cputime_limit', 'cutoff-time', 'cutoffTime', 'cutoff_time',    
    'cputime-limit', 'cputime_limit', 'tunertime-limit', 'tuner-timeout', 'tunerTimeout',
    'wallclock-limit', 'wallclock_limit', 'runtime-limit', 'runtimeLimit', 
    'wallClockLimit', 'output-dir', 'outputDirectory', 'outdir', 'instances', 
    'instance-file', 'instance-dir', 'instanceFile', 'i', 'instance_file', 
    'instance_seed_file', 'test-instances', 'test-instance-file', 'test-instance-dir', 
    'testInstanceFile', 'test_instance_file', 'test_instance_seed_file',          
    'feature-file', 'instanceFeatureFile', 'feature_file')
  
  addit_scenario_fn <- paste0(
    substr(scenario_fn, 1, nchar(scenario_fn) - 4), '.advanced')
  norm_content <- addit_content <- c()
  for (i in 1:length(smac_options)) {
    key <- names(smac_options)[i]
    cur_content_var_name <- paste0(if (!key %in% scenario_options) 'addit_','scenario_fn')
    assign(cur_content_var_name, 
           append(get(cur_content_var_name), paste(key, smac_options[i])))
  }
  write(norm_content, file=scenario_fn)
  write(addit_content, file= addit_scenario_fn)
  
  # the last check before run SMAC
  if (!all(sapply(c(addit_scenario_fn, scenario_fn, 
                    smac_options[['pcs-file']], smac_options[['instances']]),
                  file.exists))) {
    stop("Something went wrong creating files for SMAC! 
         Try to specify a \'working_directory\' and set \'persistent_files=TRUE\'.")
  }
  
  # http://blog.ambodi.com/parallel-loops-in-r/
  argument_lists <- lapply(seed, function(s) {
    c(scenario_fn, addit_scenario_fn, s, func, parser_dict, 
      mem_limit_smac_mb, smac_classpath(),
      num_train_instances, mem_limit_function_mb, t_limit_function_s,
      smac_options[['algo-deterministic']], java_executable, 
      timeout_quality)
  })
  sapply(argument_lists, remote_smac_function)
  
  # find overall incumbent and return it
  scenario_dir <- os.path.join(self.__out_dir,'.'.join(
    scenario_fn.split('/')[-1].split('.')[:-1]))
  run_incumbents <- []
  
  for (s in seed) {
    fn <- os.path.join(scenario_dir, 'traj-run-%i.txt'%s)
    run_incumbents.append(read_trajectory_file(fn)[-1])
  }
  
  run_incumbents.sort(key = operator.itemgetter("Estimated Training Performance"))
  
  param_dict <- run_incumbents[1]['Configuration']
  for (k in param_dict) {
    param_dict[k] <- parser_dict[k](param_dict[k])
  }
  
  return(run_incumbents[1]["Estimated Training Performance"], param_dict)
}
