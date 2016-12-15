# To demonstrate the use, we shall look at a slight modification of the well-
# known branin funtcion. To make things a little bit more interesting, we add
# a third parameter (x3). It has to be an integer, which is usually not
# covered by standard global minimizers.
# SMAC will acutally not impress for this function as SMAC's focus is more on
# high dimensional problems with also categorical parameters, and possible
# dependencies between them. This is where the underlying random forest realy
# shines.
modified_branin <- function(x1, x2, x3) {
  a <- 1
  b <- 5.1 / (4*pi^2)
  c <- 5 / pi
  r <- 6
  s <- 10
  t <- 1 / (8*pi)
  a*(x2 - b*x1^2 + c*x1 - r)^2 +
    s*(1-t) * cos(x1) + s + x3
}

# Now we have to define the parameters for the function we like to minimize.
# The representation tries to stay close to the SMAC manual, but deviates
# if necessary.
parameter_dict <- list(
  x1=list(type='real', span=c(-5, 5), init=1),
  x2=list(type='real', span=c(-5, 5), init=-1),
  x3=list(type='integer', span=c(0, 10), init=1))


smac_config <- list(debug=F)

res <- list()
res[c('val', 'pars')] <- rsmac::minimize(conf=smac_config,
                                         func=modified_branin,
                                         max_evaluations=1000,
                                         parameter_dict=parameter_dict)

print(sprintf('Lowest function value found: %s\nParameters: %s', res$val, res$pars))
