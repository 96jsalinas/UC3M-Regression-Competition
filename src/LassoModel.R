## =========================================================
## Lasso Model
## =========================================================

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
y_train_cv_lr <- train_cv$SalePrice
y_valid_cv_lr <- valid_cv$SalePrice

X_train_cv_lr <- model.matrix(SalePrice ~ ., data = train_cv)[, -1]
X_valid_cv_lr <- model.matrix(SalePrice ~ ., data = valid_cv)[, -1]

cv_lasso <- cv.glmnet(
  x           = X_train_cv_lr,
  y           = y_train_cv_lr,
  alpha       = 1, #1 for full Lasso regression
  family      = "gaussian",
  standardize = FALSE
)

best_lambda_lr <- cv_lasso$lambda.min

################################################################################

X_full_lr <- model.matrix(SalePrice ~ ., data = train_mod)[, -1]
y_full_lr <- train_mod$SalePrice

final_fit_lr <- glmnet(
  x           = X_full_lr,
  y           = y_full_lr,
  alpha       = 1, #1 for full Lasso regression
  lambda      = best_lambda_lr,
  family      = "gaussian",
  standardize = FALSE
)

## 5. Evaluate on held-out validation set (using model trained on FULL data)

yhat_valid_log_lr <- as.numeric(
  predict(final_fit_lr, newx = X_valid_cv_lr, s = best_lambda_lr)
)

# root mean square error
rmse <- function(truth, pred) sqrt(mean((truth - pred)^2))

rmse_valid_log_lr <- rmse(y_valid_cv_lr, yhat_valid_log_lr)
cat("Validation RMSE on log1p(SalePrice):", rmse_valid_log_lr, "\n")

## In-sample R^2 and adjusted R^2 on FULL training data (log1p scale)
yhat_train_log_lr <- as.numeric(predict(final_fit_lr, newx = X_full_lr, s = best_lambda_lr))

SSE_lr <- sum((y_full_lr - yhat_train_log_lr)^2)
SST_lr <- sum((y_full_lr - mean(y_full_lr))^2)

R2_lr <- 1 - SSE_lr / SST_lr

n_lr <- length(y_full_lr)
p_lr <- ncol(X_full_lr)  # number of predictors (no intercept in glmnet matrix)
adj_R2_lr <- 1 - (1 - R2_lr) * (n_lr - 1) / (n_lr - p_lr - 1)

cat(sprintf("In-sample R^2 (log1p) for Lasso: %.4f\n", R2_lr))
cat(sprintf("In-sample Adjusted R^2 (log1p) for Lasso: %.4f\n", adj_R2_lr))

## 6. Predict on TEST set and back-transform to original SalePrice

X_test_lr <- model.matrix(~., data = test_mod)[, -1]

yhat_test_log_lr <- as.numeric(predict(final_fit_lr, newx = X_test_lr, s = best_lambda_lr))
SalePrice_pred_lr <- exp(yhat_test_log_lr) - 1 # inverse of log1p

## 7. Build submission dataframe

lasso_reg <- data.frame(
  Id        = id_test,
  SalePrice = SalePrice_pred_lr
)

head(lasso_reg)

cat("Validation RMSE on log1p(SalePrice) for Lasso:", rmse_valid_log_lr, "\n")
cat(sprintf("In-sample R^2 (log1p) for Lasso: %.4f\n", R2_lr))
cat(sprintf("In-sample Adjusted R^2 (log1p) for Lasso: %.4f\n", adj_R2_lr))