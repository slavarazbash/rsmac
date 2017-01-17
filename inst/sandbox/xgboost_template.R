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
