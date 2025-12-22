## =============================================================================
## Ridge Model
## =============================================================================

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
y_train_cv_rr <- train_cv$SalePrice
y_valid_cv_rr <- valid_cv$SalePrice

X_train_cv_rr <- model.matrix(SalePrice ~ ., data = train_cv)[, -1]
X_valid_cv_rr <- model.matrix(SalePrice ~ ., data = valid_cv)[, -1]

cv_ridge <- cv.glmnet(
  x           = X_train_cv_rr,
  y           = y_train_cv_rr,
  alpha       = 0,
  family      = "gaussian",
  standardize = FALSE
)

best_lambda_rr <- cv_ridge$lambda.min

################################################################################

X_full_rr <- model.matrix(SalePrice ~ ., data = train_mod)[, -1]
y_full_rr <- train_mod$SalePrice

final_fit_rr <- glmnet(
  x           = X_full_rr,
  y           = y_full_rr,
  alpha       = 0,
  lambda      = best_lambda_rr,
  family      = "gaussian",
  standardize = FALSE
)

## 5. Evaluate on held-out validation set (using model trained on FULL data)

yhat_valid_log_rr <- as.numeric(
  predict(final_fit_rr, newx = X_valid_cv_rr, s = best_lambda_rr)
)

# root mean square error
rmse <- function(truth, pred) sqrt(mean((truth - pred)^2))

rmse_valid_log_rr <- rmse(y_valid_cv_rr, yhat_valid_log_rr)
cat("Validation RMSE on log1p(SalePrice):", rmse_valid_log_rr, "\n")

## In-sample R^2 and adjusted R^2 on FULL training data (log1p scale)
yhat_train_log_rr <- as.numeric(predict(final_fit_rr, newx = X_full_rr, s = best_lambda_rr))

SSE_rr <- sum((y_full_rr - yhat_train_log_rr)^2)
SST_rr <- sum((y_full_rr - mean(y_full_rr))^2)

R2_rr <- 1 - SSE_rr / SST_rr

n_rr <- length(y_full_rr)
p_rr <- ncol(X_full_rr)  # number of predictors (no intercept in glmnet matrix)
adj_R2_rr <- 1 - (1 - R2_rr) * (n_rr - 1) / (n_rr - p_rr - 1)

cat(sprintf("In-sample R^2 (log1p) for Ridge: %.4f\n", R2_rr))
cat(sprintf("In-sample Adjusted R^2 (log1p) for Ridge: %.4f\n", adj_R2_rr))

## 6. Predict on TEST set and back-transform to original SalePrice

X_test_rr <- model.matrix(~., data = test_mod)[, -1]

yhat_test_log_rr <- as.numeric(predict(final_fit_rr, newx = X_test_rr, s = best_lambda_rr))
SalePrice_pred_rr <- exp(yhat_test_log_rr) - 1 # inverse of log1p

## 7. Build submission dataframe

ridge_reg <- data.frame(
  Id        = id_test,
  SalePrice = SalePrice_pred_rr
)

head(ridge_reg)

cat("Validation RMSE on log1p(SalePrice) for Ridge:", rmse_valid_log_rr, "\n")
cat(sprintf("In-sample R^2 (log1p) for Ridge: %.4f\n", R2_rr))
cat(sprintf("In-sample Adjusted R^2 (log1p) for Ridge: %.4f\n", adj_R2_rr))
