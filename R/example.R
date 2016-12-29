# R has rather poor means (atleast in Windows) to work with pipes and fifos.
# So I decided to use inside runner.py script logic to launch another one 
# instance of R using PypeR. I am sure it is a workaround but I have no idea how 
# to achieve the same goals on pure R
library(dplyr)


#### check python ####
pythonExec <- if (Sys.info()['user'] == 'Gray') 
  'C:/Users/Gray/.conda/envs/py27/python' else 'python'
pythonVersionFull <- tryCatch(
  system(paste(pythonExec, '--version'), intern=T), 
  error=function(e) stop('Python 2 not found'))
if (stringr::str_extract(pythonVersionFull, '(?i)(?<=python )\\d') != '2') {
  stop('Wrong python version. It is must be 2')
}


#### check pyper and pysmac  ####
needPyper <- suppressWarnings(system(paste(pythonExec, '-c "import pyper"', show=F)))
if (needPyper) system('pip install PypeR')

needPysmac <- suppressWarnings(system(paste(pythonExec, '-c "import pysmac"', show=F)))
if (needPysmac) {
  system('pip install pysmac')
  libs <- system(paste(pythonExec, '-c "import sys; print sys.path"'), intern=T) %>% 
    strsplit(", ") %>% `[[`(1) %>% grep("site-packages'$", ., value=T, perl=T) %>% 
    head(1) %>% gsub("^'|'$", '',.)
  stop('Please go to', libs, 
       '/pysmac/smacrunner.py and fix [":".join(self._smac_classpath()),] to 
       [";".join(self._smac_classpath()),]')
}


#### prepare params ####
smacPars <- list(
  objective=function(x) {
    (x[2] - (5.1 / (4*pi^2))*x[1]^2 + (5 / pi)*x[1] - 6)^2 +
      10*(1-(1 / (8*pi))) * cos(x[1]) + 10 },
  x0=c(0, 0), xmin=c(-5, 0), xmax=c(10, 15),
  max_evaluations=100)
# saveRDS(smacPars, NULL)
serial <- gsub('"', "'", paste(deparse(smacPars), collapse='\n'))
# x <- eval(parse(text=serial))


#### run ####
res <- system(paste(pythonExec, 'py/runner.py', sprintf('"%s"', serial)))#, intern=T)
# print(res)
# get xmin, feval from python script
