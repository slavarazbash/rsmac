library(rsmac)

grid <- list(
  x1=list(type='continuous', init=0, min=-5, max=10),
  x2=list(type='continuous', init=0, min=0, max=15))

objective <- function(x1, x2) {
  (x2 - (5.1 / (4*pi^2))*x1^2 + (5 / pi)*x1 - 6)^2 +
    10*(1-(1 / (8*pi))) * cos(x1) + 10
}

res <- rsmac_minimize(grid, objective, list(max_evaluations=100))

stopifnot(abs(res$target_min - 0.6) < 0.1)
stopifnot(abs(res$optimized_x$x1 - 2.937) < 0.1)
stopifnot(abs(res$optimized_x$x2 - 2.365) < 0.1)

cat('\n')
print(res)
