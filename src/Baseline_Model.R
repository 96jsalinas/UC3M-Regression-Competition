## =========================
## Baseline model (R)
## Assumes train_data is preprocessed and SalePrice is log1p already.
## Baseline idea: size + quality + age + garage + baths + location proxy
## =========================

library(dplyr)

# Work on modelling frame (drop Id)
df <- train_data %>% select(-Id)

# Neighborhood is already one-hot, so use the strongest simple proxy:
# include ALL Neighborhood dummy columns automatically (if present)
nbhd_cols <- grep("^Neighborhood", names(df), value = TRUE)

# Pick a small, sensible baseline feature set
base_cols <- c(
  "SalePrice",
  "OverallQual",
  "GrLivArea",
  "TotalSF",
  "HouseAge",
  "RemodAge",
  "GarageCars",
  "TotalBath"
)

# Keep only what exists
use_cols <- intersect(c(base_cols, nbhd_cols), names(df))
df_base <- df[, use_cols]

# Fit baseline linear model on log1p(SalePrice)
baseline_lm <- lm(SalePrice ~ ., data = df_base)

summary(baseline_lm)
