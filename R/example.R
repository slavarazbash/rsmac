# branin <- function(x) {
#   (x[2] - (5.1 / (4*pi^2))*x[1]^2 + (5 / pi)*x[1] - 6)^2 +
#     10*(1-(1 / (8*pi))) * cos(x[1]) + 10
# }
# 
# modified_branin <- function(x) {
#   branin(x) + x[3]
# }
# 
# res <- list()
# res[c('xmin', 'fval')] <- minimize(branin, x0=c(0,0), xmin=c(-5, 0), 
#                                    xmax=c(10, 15), max_evaluations=1500)
# list2env(res)
# length(xmin) == 1
# length(xmin["x"]) == 2
# fval  # ~ 0.4

# Now we have to define the parameters for the function we like to minimize.
# The representation tries to stay close to the SMAC manual, but deviates
# if necessary.
# parameter_dict <- list(
#   x1=list(type='real', span=c(-5, 5), init=1),
#   x2=list(type='real', span=c(-5, 5), init=-1),
#   x3=list(type='integer', span=c(0, 10), init=1))

library(dplyr)

pythonExec <- if (Sys.info()['user'] == 'Gray')
  'C:/Users/Gray/.conda/envs/py27/python' else 'python'
pythonVersionFull <- tryCatch(
  system(paste(pythonExec, '--version'), intern=T), 
  error=function(e) stop('Python 2 not found'))
if (stringr::str_extract(pythonVersionFull, '(?i)(?<=python )\\d') != '2') {
  stop('Wrong python version. It is must be 2')
}

res <- system(paste(pythonExec, 'py/example.py'), intern=T)
if (any(grepl('No module named pysmac', res)) {
  res <- paste(system('pip install pysmac', intern=T), collapse='\n')
  res <- system(paste(pythonExec, 'py/example.py'), intern=T)
}
if (grepl('fmin: %f\" %', res) && grepl('float argument required, not NoneType', res)) {
  libs <- system(paste(pythonExec, '-c "import sys; print sys.path"'), intern=T) %>% 
    strsplit(", ") %>% `[[`(1) %>% grep("site-packages'$", ., value=T, perl=T) %>% 
    head(1) %>% gsub("^'|'$", '',.)
  stop('Please go to',
       libs, 'pysmac/smacrunner.py and fix [":".join(self._smac_classpath()),] to 
       [";".join(self._smac_classpath()),]')
}
print(res)
# get xmin, feval from python script
# find the way to pass R function
