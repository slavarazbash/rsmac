# R has rather poor means (atleast in Windows) to work with pipes and fifos.
# So I decided to use inside runner.py script logic to launch another one 
# instance of R using PypeR. I am sure it is a workaround but I have no idea how 
# to achieve the same goals on pure R
# 
# Scheme of work:
# current R -> Python (manage+pysmac)--> Java (smac)
#                                   \
#                                    --> R (for objective)

checkPython <- function() {
  pythonExec <- if (Sys.info()['user'] == 'Gray') 
    'C:/Users/Gray/.conda/envs/py27/python' else 'python'
  pythonVersionFull <- tryCatch(
    system(paste(pythonExec, '--version'), intern=T), 
    error=function(e) stop('Python 2 not found'))
  if (str_extract(pythonVersionFull, '(?i)(?<=python )\\d') != '2') {
    stop('Python version must be equal 2')
  }
  pythonExec
}

checkPythonLibs <- function(pythonExec) {
  needPyper <- suppressWarnings(system(paste(pythonExec, '-c "import pyper"', show=F)))
  if (needPyper) system('pip install PypeR')
  
  needPysmac <- suppressWarnings(system(paste(pythonExec, '-c "import pysmac"', show=F)))
  if (needPysmac) {
    system('pip install pysmac')
    
    if (Sys.info()['sysname'] == 'Windows') {
      libs <- system(paste(pythonExec, '-c "import sys; print sys.path"'), intern=T) %>% 
        strsplit(", ") %>% `[[`(1) %>% grep("site-packages'$", ., value=T, perl=T) %>% 
        head(1) %>% gsub("^'|'$", '',.) %>% gsub('\\\\+', '/',.)
      stop('Please go to', libs, 
           '/pysmac/smacrunner.py and fix [":".join(self._smac_classpath()),] to 
           [";".join(self._smac_classpath()),]')
    }
  }
}

validateSmacArgs <- function(objective, grid) {
  stopifnot(is.function(objective))
  stopifnot(is.list(grid))
  
  if (length(names(formals(objective))) != length(grid)) {
    stop('Count of the objective parameters does not coincide with the grid list length')
  }
  stopifnot(
    all(sapply(grid, `[[`, 'type') %in% c('continuous', 'discrete', 'categorical')))
  
  tryCatch(
    do.call(objective, lapply(grid, `[[`, 'init')),
    error=function(e) stop("Could not call objective function with initial grid values"))
}

#' Smac minimization function
#' 
#' @param objective objective function to minimize
#' @param grid list like list(x1=list(type='continuous', init=0, min=-5, max=10), x2=..)
#' @param ... additional parameters. The description is right after 'x_categorical' here:
#' https://github.com/automl/pysmac/blob/
#' a3452d56aa1f3352c36ec0750be75a1f8fafe509/pysmac/optimize.py#L28
#' @examples
#' if (interactive()) {
#'   rsmacMinimize(objective, grid, max_evaluations=15)
#' }
#' @export
rsmacMinimize <- function(objective, grid, ...) {
  pythonExec <- checkPython()
  checkPythonLibs(pythonExec)
  validateSmacArgs(objective, grid)
  
  smacArgs <- append(list(objective=objective, grid=grid), list(...))
  serializedArgs <- gsub('"', "'", paste(deparse(smacArgs), collapse='[CRLF]'))
  
  smacPipe <- pipe(sprintf('%s -i py/runner.py "%s"', pythonExec, serializedArgs), 'r')
  while (length(line <- readLines(smacPipe, 1))) {
    if (exists('prevLine')) cat(prevLine, fill=T)
    prevLine <- line
  }
  close(smacPipe)
  
  parsedResult <- prevLine %>% 
    gsub('\\[|\\]', '', .) %>% trimws() %>% strsplit(' +') %>% 
    `[[`(1) %>% as.numeric  # prevLine e.g. "[ -2.78657385  11.17500087] 1.06589"
  
  targetMin <- tail(parsedResult, 1)
  optimizedX <- head(parsedResult, -1)
  list(targetMin=targetMin, optimizedX=optimizedX)
}
