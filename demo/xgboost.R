library(rsmac)

grid <- list(
  nrounds     = list(type='discrete', init=5, min=3, max=30),
  max.depth   = list(type='discrete', init=4, min=3, max=10),
  eta         = list(type='continuous', init=0.5, min=0.01, max=0.99),
  subsample   = list(type='continuous', init=1, min=0.5, max=1))

objective <- function(max.depth, eta, subsample, nrounds) {
  params <- list(objective = "binary:logistic",
                 max.depth = max.depth, eta=eta, subsample=subsample)
  cv_history <- xgb.cv(params, xgb.DMatrix(train$data, label = train$label),
                       nrounds = nrounds,  # nthread = max(1, parallel::detectCores() - 2),
                       nfold = 2, verbose=F, prediction=F)
  cv_score <- min(cv_history[, 3, with=F])
  cv_score
}

pysmac_args <- list(max_evaluations=50)

res <- rsmac_minimize(grid, objective, pysmac_args, init_rcode = {
  library(xgboost)
  data(agaricus.train, package='xgboost')
  train <- agaricus.train
})

stopifnot(res$target_min < 0.001)

cat('\n')
print(res)
