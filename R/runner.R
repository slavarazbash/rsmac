check_java_version <- function() {
  java_version_res <- tryCatch(system("java -version", intern = T),
                               error=function(e) stop('Java not found'))
  java_version <- as.numeric(substr(gsub('[^0-9\\.]', '', java_version_res[1]), 1, 3))
  stopifnot(java_version > 1.6)
}

generate_files <- function(cutoff_time, cv_folds, xs) {
  paths <- create_working_dir() %>% 
    generate_scenario_file(cutoff_time) %>% 
    generate_instance_file(cv_folds) %>% 
    generate_parameter_file(xs)
  paths
}

create_working_dir <- function() {
  paths <- list(wd=tempdir())
  paths <- append(paths, 
                  sapply(c('exec', 'out', 'result'), 
                         function(dr) paste0(paths$wd, '/', dr), simplify=F))
  paths$result <- gsub('result$', 'out/result', paths$result)
  paths
}

generate_scenario_file <- function(paths, cutoff_time) {
  paths$scenario <- paste0(paths$wd, "/smac-scenario.txt")
  
  fdata <- sprintf(
"\nalgo = echo 0
execdir = %s
outdir = %s
deterministic = 1
rungroup = result
run_obj = quality
overall_obj = mean
cutoff_time = %d
cutoff_length = max
validation = false
paramfile = %s/params.pcs
instance_file = %s/instances.txt", 
  paths$exec, paths$out, cutoff_time, paths$wd, paths$wd)
                   
  writeLines(fdata, paths$scenario)
  paths
}

generate_instance_file <- function(paths, cv_folds) {
  paths$instance <- paste0(paths$wd, "/instances.txt")
  
  if (is.null(cv_folds) || cv_folds < 1) cv_folds <- 1
  content <- paste0(paste0('cvfold-', 0:(cv_folds - 1)), collapse='\n')
  writeLines(content, paths$instance)
  paths
}

generate_parameter_file <- function(paths, xs) {
  paths$param <- paste0(paths$wd, "/params.pcs")
  
  content <- c(
    if (!is.null(xs$x0)) {
      sapply(1:(length(xs$xmin)), function(i)
        sprintf("x%s [%s, %s] [%s]", i, xs$xmin[i], xs$xmax[i], xs$x0[i]), USE.NAMES=F)
    },
    
    if (!is.null(xs$x0_int)) {
      sapply(1:(length(xs$xmin_int)), function(i)
        sprintf("x%s [%s, %s] [%s]", i, xs$xmin_int[i], xs$xmax_int[i], xs$x0_int[i]),
          USE.NAMES=F)
    },
      
    if (!is.null(xs$x_categorical)) {
      sapply(1:(length(xs$x_categorical)), function(i) {
        key <- names(xs$x_categorical)[i]
        vals <- xs$x_categorical[i]
        sprintf("x_categorical_%s {%s} [%s]", key, paste(vals, collapse=', '), vals[0])
      }, USE.NAMES=F)
    }) 
      
  writeLines(paste(content, collapse='\n'), paths$param)
  paths
}
    
smac_classpath <- function(paths, smac_version) {
  # paths$smac$root <- system.file(paste0("smac/", smac_version), package = "rsmac")
  paths$smac$root <- paste0(getwd(), "/smac/", dir('smac')[1])
  paths$smac <- append(paths$smac,
    sapply(c('conf', 'patches', 'lib'), function(df) {
      paste0(paths$smac$root, '/', df)
    })
  )
  
  classPath <- paths$smac[setdiff(names(paths$smac), 'root')]
  
  cat("SMAC lib folder:", paths$smac$root, '\n')
  cat("SMAC classPath:", paste(classPath, collapse=';'), '\n')
  c(classPath, paths)
}

# Start SMAC in IPC mode. SMAC will wait for udp messages to be sent.
start_smac <- function(paths, smac_version, max_evaluations, port, seed,
                       rf_num_trees, rf_full_tree_bootstrap, intensif_percent) {
  res <- smac_classpath(paths, smac_version)
  classPath <- res[[1]]
  paths <- res[[2]]

  cmds <- c(
    "java", "-Xmx1024m", "-cp",
    paste(classPath, collapse=';'),
    "ca.ubc.cs.beta.smac.executors.SMACExecutor",
    "--scenario-file", paths$scenario,
    "--num-run", "1",
    "--totalNumRunsLimit", max_evaluations,
    "--tae", "IPC",
    "--ipc-mechanism", "TCP",
    "--ipc-remote-port", port,
    "--seed", seed,
    "--rf-num-trees", rf_num_trees,
    "--rf-full-tree-bootstrap", rf_full_tree_bootstrap,
    "--intensification-percentage", intensif_percent)
  
  browser()
  readLines(pipe("ls -1"))
  system(paste(cmds, collapse=' '), intern = T)
  # self._smac_process <- Popen(cmds, stdout = sys.stdout, stderr = sys.stdout)
}

get_best_parameters <- function(paths) {
  traj_content <- readLines(paste0(paths$result, '/traj-run-1.txt'))
  parse_smac_trajectory_string(traj_content)
}

is_finished <- function() {
  browser()
  # smac_process$poll()
  # !is.null(smac_process$returncode)
}

stop <- function(paths) {
  browser()
  if (is_finished()) {
    self._smac_process.send_signal(signal.SIGINT)
    self._smac_process.wait()
  }
  
  if (hasattr(self, "_working_dir")) shutil.rmtree(paths$wd)
  if (hasattr(self, "_smac_process")) self._smac_process.poll()
  if (self._smac_process.returncode == NULL) self._smac_process.kill()
}
