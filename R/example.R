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
# parameter_dict <- list(
#   x1=list(type='real', span=c(-5, 5), init=1),
#   x2=list(type='real', span=c(-5, 5), init=-1),
#   x3=list(type='integer', span=c(0, 10), init=1))

