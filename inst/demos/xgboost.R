library(rsmac)
library(xgboost)
library(pROC)



data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')

summary(agaricus.train$label)
#Min. 	1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.4821  1.0000  1.0000

training.data.final.model <- xgb.DMatrix(agaricus.train[['data']], label = agaricus.train[['label']])
test.data.final.model <- xgb.DMatrix(agaricus.test[['data']], label = agaricus.test[['label']])




grid <- list(
  #Chose some sub-optimal starting values on purpose to show optimiser progress.
  max.depth   = list(type='discrete', init=1, min=1, max=10), 
  eta         = list(type='continuous', init=0.5, min=0.01, max=0.99),
  subsample   = list(type='continuous', init=.1, min=0.1, max=1))

objective <- function(max.depth, eta, subsample) {
  params <- list(objective = "binary:logistic",
                 max.depth = max.depth, eta=eta, subsample=subsample)

  cv_history <- xgb.cv(params, xgb.DMatrix(train[['data']], label = train[['label']]),
                       nrounds = 1000, early_stopping_rounds = 50,  # nthread = max(1, parallel::detectCores() - 2),
                       nfold = 2, verbose=F, prediction=F, metrics = list("auc"))
  cv_score <- -1 * max(cv_history[["evaluation_log"]][cv_history[["best_iteration"]], test_auc_mean])
  cv_score
}

pysmac_args <- list(max_evaluations=50)

res <- rsmac_minimize(grid, objective, pysmac_args, pythonExec="python", init_rcode = {
  library(xgboost)
  data(agaricus.train, package='xgboost')
  train <- agaricus.train
})



best.params <- list(objective = "binary:logistic", max.depth = res$optimized_x$max.depth, eta=res$optimized_x$eta, subsample=res$optimized_x$subsample)

best.iteration <- xgb.cv(best.params, training.data.final.model, nrounds=1000, early_stopping_rounds = 50, nfold = 2, metrics = list("auc"))$best_iteration
bst <- xgb.train(best.params, training.data.final.model, nrounds=best.iteration)
test.preds <- predict(bst, test.data.final.model)
auc(agaricus.test[["label"]], test.preds)
#Area under the curve: 1



