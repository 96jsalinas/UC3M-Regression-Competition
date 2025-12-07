# Evaluation Script

# Ensure data is loaded
if (!exists("train_data") || !exists("test_data")) {
  stop("Data not found. Please run main.R.")
}

# Placeholder for evaluation code

#Here, we are calculating the residuals to use with the graphs below. 
res_valid_log <- y_valid_cv - yhat_valid_log

par(mfrow = c(2, 2))  # 2×2 grid (one empty panel)
#QQ plot
qqnorm(res_valid_log, main = "Q–Q Plot of Validation Residuals (log scale)")
qqline(res_valid_log, col = "red")

#Plot that looks at the residuals vs fitted
plot(
  yhat_valid_log, res_valid_log,
  xlab = "Predicted log1p(SalePrice)",
  ylab = "Residuals",
  main = "Residuals vs Fitted"
)
abline(h = 0, col = "red")

#observed vs predicted
plot(
  y_valid_cv, yhat_valid_log,
  xlab = "Observed log1p(SalePrice)",
  ylab = "Predicted",
  main = "Observed vs Predicted"
)
abline(0, 1, col = "red")




