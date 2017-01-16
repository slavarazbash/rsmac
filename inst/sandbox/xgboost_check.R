library(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

params=list(objective = "binary:logistic", #colsample=0.5, subsample=0.7, alpha=0
            max.depth=2, eta=1)

need_pred <- T
cv_history <- xgb.cv(params, xgb.DMatrix(train$data, label = train$label),  # X_sparse
                     nrounds = 10, nthread = max(1, parallel::detectCores() - 2),
                     nfold = 5, verbose=T, prediction=need_pred)  # feval=geomErr
if (need_pred) {
  pred <- cv_history
  cv_history <- cv_history$dt
}
score <- min(cv_history[, 3, with=F])
score_train <- min(cv_history[, 1, with=F])



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

