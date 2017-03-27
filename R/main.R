# R has rather poor means (atleast in Windows) to work with pipes and fifos.
# So I decided to use inside runner.py script logic to launch another one 
# instance of R using PypeR. I am sure it is a workaround but I have no idea how 
# to achieve the same goals on pure R
# 
# Scheme of work:
# current R -> Python (manage+pysmac)--> Java (smac)
#                                   \
#                                    --> R (for objective)
# TODO: implement categorical

utils::globalVariables(c(".", "%>%"))
isWindows <- Sys.info()['sysname'] == 'Windows'

#' @importFrom stringr str_extract
checkPython <- function() {
  pythonExec <- if (Sys.info()['user'] == 'Gray' ) {
    'C:/Users/Gray/.conda/envs/py27/python'
  } else if (!isWindows) {
    '/Library/Frameworks/Python.framework/Versions/2.7/bin/python' 
  } else {
    'python'
  }
  pythonVersionFull <- tryCatch(
    system2(pythonExec, '--version', stdout=T, stderr=T), 
    error=function(e) stop('Python 2 not found'))
  if (str_extract(pythonVersionFull, '(?i)(?<=python )\\d') != '2') {
    stop('Python version must be equal 2')
  }
  pythonExec
}

checkPyLib <- function(pythonExec, libName) {
  needInstall <- suppressWarnings(if (isWindows) {
    system(sprintf('%s -c "import %s"', pythonExec, tolower(libName), show=F))
  } else {
    length(system2('pip', c('freeze | grep', libName), stdout = T, stderr = T)) == 0
  })
  if (needInstall) system2('pip', c('install', libName))
  needInstall
}

#' @importFrom dplyr "%>%"
#' @importFrom utils head tail
checkPythonLibs <- function(pythonExec) {
  checkPyLib(pythonExec, 'PypeR')
  pysmacWasInstalled <- checkPyLib(pythonExec, 'pysmac')
  
  if (pysmacWasInstalled && isWindows) {
    libs <- system(paste(pythonExec, '-c "import sys; print sys.path"'), intern=T) %>% 
      strsplit(", ") %>% `[[`(1) %>% grep("site-packages'$", ., value=T, perl=T) %>% 
      head(1) %>% gsub("^'|'$", '',.) %>% gsub('\\\\+', '/',.)
    stop('Please go to', libs, 
         '/pysmac/smacrunner.py and fix [":".join(self._smac_classpath()),] to 
         [";".join(self._smac_classpath()),]')
  }
}

validateSmacArgs <- function(grid, objective, pysmac_args, rcode) {
  stopifnot(is.function(objective))
  if (grepl('\\$', paste0(deparse(body(objective)), collapse='')) && !isWindows) {
    stop('Please rewrite objective to make the function free of "$" characters.\n',
         'It is a workaround for one python "pyper" library bug')
  }
  stopifnot(is.list(grid))
  stopifnot(is.null(pysmac_args) || is.list(pysmac_args))
  stopifnot(class(rcode) == '{' || is.null(rcode))
  
  if (!setequal(names(grid), names(formals(objective)))) {
    stop('Objective parameters does not coincide with the grid names')
  }
  stopifnot(
    all(sapply(grid, `[[`, 'type') %in% c('continuous', 'discrete')))#, 'categorical')))
  # tryCatch(
  #   do.call(objective, lapply(grid, `[[`, 'init')),
  #   error=function(e) stop("Could not call objectiv function with initial grid values"))
}

#' Smac minimization function
#' 
#' @param grid list like list(x1=list(type='continuous', init=0, min=-5, max=10), x2=..)
#' @param objective objective function to minimize
#' @param pysmac_args list of pysmac additional parameters. The description is here:
#' \url{
#' https://github.com/automl/pysmac/blob/a3452d56aa1f3352c36ec0750be75a1f8fafe509/
#' pysmac/optimize.py#L32-L38
#' }
#' @param init_rcode r code expression that will be runned once before pysmac
#' @examples
#' \dontrun{
#' objective <- function(x1, x2) {
#'   (x2 - (5.1 / (4*pi^2))*x1^2 + (5 / pi)*x1 - 6)^2 +
#'     10*(1-(1 / (8*pi))) * cos(x1) + 10
#' }
#'
#' grid <- list(
#'   x1=list(type='continuous', init=0, min=-5, max=10),
#'   x2=list(type='continuous', init=0, min=0, max=15))
#'
#' res <- rsmac_minimize(grid, objective, list(max_evaluations=100))
#' 
#' stopifnot(abs(res$target_min - 0.6) < 0.1)
#' stopifnot(abs(res$optimized_x$x1 - 2.937) < 0.1)
#' stopifnot(abs(res$optimized_x$x2 - 2.365) < 0.1)
#' cat('\n')
#' print(res) 
#' }
#' @export
rsmac_minimize <- function(grid, objective, pysmac_args=NULL, init_rcode=NULL, pythonExec=NULL) {

  if(is.null(pythonExec)) {
	  pythonExec <- checkPython()  
  } 
	
  checkPythonLibs(pythonExec)
  subst_init_rcode <- substitute(init_rcode)
  validateSmacArgs(grid, objective, pysmac_args, 
                   if (!is.null(subst_init_rcode)) as.call(subst_init_rcode) else NULL)
  
  smacArgs <- append(list(objective=objective, grid=grid, 
                          init_rcode=subst_init_rcode), 
                     pysmac_args)
  serializedArgs <- gsub('"', "'", paste(deparse(smacArgs), collapse='[CRLF]'))
  
  py_console <- get_py_console(pythonExec, serializedArgs)
  final_line <- track_console(py_console)
  for (stream in py_console) close(stream)
  
  parsed_result <- final_line %>% strsplit("%\\+%") %>% `[[`(1) %>% tail(-1)
  list(target_min  = as.numeric(parsed_result[1]), 
       optimized_x = eval(parse(text=parsed_result[2])))
}

track_console <- function(py_console) {
  if (isWindows) {
    while (!startsWith(line <- readLines(py_console$output, 1), 'pysmac>>')) {
      cat(line, fill=T)
    }
  } else {
    line <- ''
    while (!startsWith(line, 'pysmac>>')) {
      cat(line, fill=T)
      while (!length(line <- readLines(py_console$output, 1))) {
        Sys.sleep(0.1)
      }
    }
  }
  line
}

get_py_console <- function(pythonExec, serializedArgs) {
  if (isWindows) {
    return(list(output=
      pipe(sprintf('%s -i inst/python/runner.py "%s"',  # 2>&1
                        pythonExec, serializedArgs), 'r')))
  }

  stopifnot(capabilities("fifo"))
  fifo_file_name <- '/tmp/Rpython.fifo'
  if (!file.exists(fifo_file_name)) system2('mkfifo', fifo_file_name)
  output <- fifo(fifo_file_name, 'r')
  readLines(output)  # if fifo file was not empty
  input  <- pipe(sprintf('%s -i inst/python/runner.py "%s" > %s',   # 2>&1
                         pythonExec, serializedArgs, fifo_file_name), 'w')
  list(input=input, output=output)
}
