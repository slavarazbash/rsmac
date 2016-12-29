# https://github.com/sfalkner/pySMAC
# https://docs.python.org/2/library/subprocess.html#popen-constructor
# https://docs.python.org/2/library/socket.html
# http://blog.corynissen.com/2013/05/using-r-to-communicate-via-socket.html
# http://stackoverflow.com/questions/17766391/how-to-get-output-from-a-pipe-
# connection-before-closing-it-in-r

# import os
# print os.getcwd() == getwd()

pOpen <- function(cmd) {
  readFileName <- tempfile()
  inp <- pipe(paste0(cmd, ' >', readFileName), open='w+')
  out <- file(
    description = "", open = "r", blocking = TRUE, encoding = getOption("encoding"),
    raw = FALSE, method = getOption("url.method", "default"))
  c(inp, out)
}

# inp <- pipe('pip freeze | grep ab', open='r')  # activate py27 && 
z <- pipe('python -i >1.txt', open='w')
cat('print(5)\n', file=z)
fl <- file('1.txt', 'r')
readLines(fl, 1)
readLines(fl, 1)
cat('print(3)\n', file=z)
readLines(fl, 1)
closeAllConnections()


branin <- function(x) {
  (x[2] - (5.1 / (4*pi^2))*x[1]^2 + (5 / pi)*x[1] - 6)^2 +
    10*(1-(1 / (8*pi))) * cos(x[1]) + 10
}

modified_branin <- function(x) {
  branin(x) + x[3]
}

res <- list()
res[c('xmin', 'fval')] <- minimize(branin, x0=c(0,0), xmin=c(-5, 0),
                                   xmax=c(10, 15), max_evaluations=1500)
list2env(res)
length(xmin) == 1
length(xmin["x"]) == 2
fval  # ~ 0.4

# Now we have to define the parameters for the function we like to minimize.
# The representation tries to stay close to the SMAC manual, but deviates
# if necessary.
parameter_dict <- list(
  x1=list(type='real', span=c(-5, 5), init=1),
  x2=list(type='real', span=c(-5, 5), init=-1),
  x3=list(type='integer', span=c(0, 10), init=1))