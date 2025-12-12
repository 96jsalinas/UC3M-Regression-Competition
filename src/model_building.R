## =========================================================
## Modelling pipeline: Elastic Net on preprocessed data
## Assumes:
##   - train_data: preprocessed, with log1p(SalePrice)
##   - test_data:  preprocessed, no SalePrice
## =========================================================

library(glmnet)
library(dplyr)
library(caret)
library(tictoc)

## 0. Sanity checks
if (!exists("train_data") || !exists("test_data")) {
  stop("train_data and test_data must exist (run preprocessing first).")
}

if (!("SalePrice" %in% names(train_data))) {
  stop("train_data must contain log-transformed SalePrice.")
}

## Keep Ids for later
id_test <- test_data$Id
id_train <- train_data$Id

## Drop Id from modelling features if present
train_mod <- train_data %>%
  select(-Id)
test_mod <- test_data %>%
  select(-Id)

## 1. Create train / validation split from TRAIN ONLY
set.seed(123)

train_idx <- createDataPartition(
  y    = train_mod$SalePrice,
  p    = 0.8,
  list = FALSE
)

train_cv <- train_mod[train_idx, ]
valid_cv <- train_mod[-train_idx, ]


## 2. Build model matrices
# y = log1p(SalePrice)
y_train_cv <- train_cv$SalePrice
y_valid_cv <- valid_cv$SalePrice

X_train_cv <- model.matrix(SalePrice ~ ., data = train_cv)[, -1]
X_valid_cv <- model.matrix(SalePrice ~ ., data = valid_cv)[, -1]


## 3. Elastic Net CV over alpha, select best (alpha, lambda)

alphas <- seq(0, 1, by = 0.01)
cv_fits <- vector("list", length(alphas))
cv_results <- data.frame(
  alpha      = alphas,
  lambda_min = NA_real_,
  cvm_min    = NA_real_
)

tic("Elastic Net alpha grid CV")
for (i in seq_along(alphas)) {
  a <- alphas[i]
  cv_fit <- cv.glmnet(
    x           = X_train_cv,
    y           = y_train_cv,
    alpha       = a,
    nfolds      = 10,
    family      = "gaussian",
    standardize = FALSE # already standardized in preprocessing
  )
  cv_fits[[i]] <- cv_fit
  cv_results$lambda_min[i] <- cv_fit$lambda.min
  cv_results$cvm_min[i] <- min(cv_fit$cvm)

  pct <- round(100 * i / length(alphas), 1)
  cat(sprintf("\rAlpha %d/%d (%.1f%%) complete", i, length(alphas), pct))
  flush.console()
}

cat("\n")
toc()
best_idx <- which.min(cv_results$cvm_min)
best_alpha <- cv_results$alpha[best_idx]
best_lambda <- cv_results$lambda_min[best_idx]

cat("Best alpha:", best_alpha, "  Best lambda:", best_lambda, "\n")

## 4. Fit final model on FULL training data using best (alpha, lambda)

X_full <- model.matrix(SalePrice ~ ., data = train_mod)[, -1]
y_full <- train_mod$SalePrice

final_fit <- glmnet(
  x           = X_full,
  y           = y_full,
  alpha       = best_alpha,
  lambda      = best_lambda,
  family      = "gaussian",
  standardize = FALSE
)

## 5. Evaluate on held-out validation set (using model trained on FULL data)

yhat_valid_log <- as.numeric(
  predict(final_fit, newx = X_valid_cv, s = best_lambda)
)

# root mean square error
rmse <- function(truth, pred) sqrt(mean((truth - pred)^2))

rmse_valid_log <- rmse(y_valid_cv, yhat_valid_log)
cat("Validation RMSE on log1p(SalePrice):", rmse_valid_log, "\n")

## 6. Predict on TEST set and back-transform to original SalePrice

X_test <- model.matrix(~., data = test_mod)[, -1]

yhat_test_log <- as.numeric(predict(final_fit, newx = X_test, s = best_lambda))
SalePrice_pred <- exp(yhat_test_log) - 1 # inverse of log1p

## 7. Build submission dataframe

submission <- data.frame(
  Id        = id_test,
  SalePrice = SalePrice_pred
)

head(submission)

## Optionally write to CSV
# write.csv(submission, "submission.csv", row.names = FALSE)

############################
