closeAllConnections()
library(rsmac)

grid <- list(
  max.depth = list(type='discrete', init=4, min=3, max=10),
  eta       = list(type='continuous', init=0.5, min=0.01, max=0.99))

objective <- function(max.depth, eta) {
  params <- list(objective = "binary:logistic",
                 max.depth=max.depth, eta=eta)
  cv_history <- xgb.cv(params, xgb.DMatrix(train$data, label = train$label),
                       nrounds = 2,  # nthread = max(1, parallel::detectCores() - 2),
                       nfold = 2, verbose=F, prediction=F)
  cv_score <- min(cv_history[, 3, with=F])
  cv_score
}

pysmac_args <- list(max_evaluations=10)

res <- rsmac_minimize(objective, grid, pysmac_args, init_rcode = {
  library(xgboost)
  data(agaricus.train, package='xgboost')
  data(agaricus.test, package='xgboost')
  train <- agaricus.train
  test <- agaricus.test
})

cat('\n')
print(res)

