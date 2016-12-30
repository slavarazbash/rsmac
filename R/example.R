devtools::load_all()

objective <- function(x1, x2) {
  (x2 - (5.1 / (4*pi^2))*x1^2 + (5 / pi)*x1 - 6)^2 +
    10*(1-(1 / (8*pi))) * cos(x1) + 10
}

grid <- list(
    x1=list(type='continuous', init=0, min=-5, max=10),
    x2=list(type='continuous', init=0, min=0, max=15))

res <- rsmacMinimize(objective, grid, max_evaluations=15)

cat('\n')
print(res)
