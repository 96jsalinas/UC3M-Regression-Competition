# Comprehensive Preprocessing Pipeline
# Combines data exploration and all transformations from data_transformation.MD

# Ensure data is loaded
if (!exists("train_data") || !exists("test_data")) {
  stop("Data not found in environment. Please run using main.R.")
}

cat("\n========================================\n")
cat("  PREPROCESSING PIPELINE\n")
cat("========================================\n\n")

# Store original row counts for validation
n_train <- nrow(train_data)
n_test <- nrow(test_data)

# =============================================================================
# SECTION 1: DATA QUALITY CHECKS & EXPLORATION
# =============================================================================

cat("[1/7] Data quality checks and exploration...\n")

# Function to generate comprehensive statistics
generate_statistics <- function(df) {
  stats <- data.frame(
    Column = colnames(df),
    Type = sapply(df, function(x) class(x)[1]),
    Missing_Count = colSums(is.na(df)),
    Missing_Pct = round((colSums(is.na(df)) / nrow(df)) * 100, 2),
    Unique_Values = sapply(df, function(x) length(unique(x))),
    stringsAsFactors = FALSE
  )

  # Add numeric stats
  stats$Min <- sapply(df, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA)
  stats$Median <- sapply(df, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA)
  stats$Mean <- sapply(df, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA)
  stats$Max <- sapply(df, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA)

  rownames(stats) <- NULL
  return(stats)
}

# Generate and save statistics
train_stats <- generate_statistics(train_data)
test_stats <- generate_statistics(test_data)

write_csv(train_stats, "train_statistics.csv")
write_csv(test_stats, "test_statistics.csv")

cat("  - Statistics saved to CSV files\n")

# =============================================================================
# SECTION 2: MISSING VALUE HANDLING
# =============================================================================

cat("\n[2/7] Handling missing values...\n")

# Helper function: Impute categorical NA with "None"
impute_categorical_none <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]][is.na(df[[col]])] <- "None"
    }
  }
  return(df)
}

# Helper function: Impute numeric NA with 0
impute_numeric_zero <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]][is.na(df[[col]])] <- 0
    }
  }
  return(df)
}

# 2.1: Categorical NA -> "None"
categorical_none_cols <- c(
  "Alley", "MasVnrType", "BsmtQual", "BsmtCond", "BsmtExposure",
  "BsmtFinType1", "BsmtFinType2", "FireplaceQu", "GarageType",
  "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature"
)

train_data <- impute_categorical_none(train_data, categorical_none_cols)
test_data <- impute_categorical_none(test_data, categorical_none_cols)
cat("  - Categorical NA -> 'None' applied\n")

# 2.2: Numeric NA -> 0
numeric_zero_cols <- c(
  "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF",
  "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "GarageCars", "GarageArea"
)

train_data <- impute_numeric_zero(train_data, numeric_zero_cols)
test_data <- impute_numeric_zero(test_data, numeric_zero_cols)
cat("  - Numeric NA -> 0 applied\n")

# 2.3: LotFrontage - Neighborhood median with fallback
if ("LotFrontage" %in% names(train_data)) {
  # Compute neighborhood medians from training data
  neighborhood_medians <- train_data %>%
    group_by(Neighborhood) %>%
    summarise(LotFrontage_median = median(LotFrontage, na.rm = TRUE), .groups = "drop")

  # Overall median as fallback
  overall_median <- median(train_data$LotFrontage, na.rm = TRUE)

  # Impute train
  train_data <- train_data %>%
    left_join(neighborhood_medians, by = "Neighborhood") %>%
    mutate(LotFrontage = ifelse(is.na(LotFrontage),
      coalesce(LotFrontage_median, overall_median),
      LotFrontage
    )) %>%
    select(-LotFrontage_median)

  # Impute test
  test_data <- test_data %>%
    left_join(neighborhood_medians, by = "Neighborhood") %>%
    mutate(LotFrontage = ifelse(is.na(LotFrontage),
      coalesce(LotFrontage_median, overall_median),
      LotFrontage
    )) %>%
    select(-LotFrontage_median)

  cat("  - LotFrontage imputed by neighborhood\n")
}

# 2.4: Test-specific missing values - impute with mode from training
test_specific_cols <- c(
  "MSZoning", "Utilities", "Exterior1st", "Exterior2nd",
  "KitchenQual", "Functional", "SaleType", "Electrical"
)

for (col in test_specific_cols) {
  if (col %in% names(train_data) && col %in% names(test_data)) {
    # Get mode from training data
    mode_value <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]

    # Impute in both train and test
    train_data[[col]][is.na(train_data[[col]])] <- mode_value
    test_data[[col]][is.na(test_data[[col]])] <- mode_value
  }
}
cat("  - Test-specific missing values imputed\n")

# =============================================================================
# SECTION 3: FEATURE ENGINEERING
# =============================================================================

cat("\n[3/7] Creating engineered features...\n")

# Helper function to create features
create_features <- function(df) {
  df <- df %>%
    mutate(
      # Derived features
      TotalSF = TotalBsmtSF + `1stFlrSF` + `2ndFlrSF`,
      TotalBath = FullBath + 0.5 * HalfBath + BsmtFullBath + 0.5 * BsmtHalfBath,
      TotalPorchSF = OpenPorchSF + EnclosedPorch + `3SsnPorch` + ScreenPorch,

      # Age features
      HouseAge = YrSold - YearBuilt,
      RemodAge = YrSold - YearRemodAdd,
      GarageAge = ifelse(is.na(GarageYrBlt), 0, YrSold - GarageYrBlt),

      # Binary flags
      HasGarage = ifelse(GarageArea > 0, 1, 0),
      HasBasement = ifelse(TotalBsmtSF > 0, 1, 0),
      HasFireplace = ifelse(Fireplaces > 0, 1, 0),
      HasPool = ifelse(PoolArea > 0, 1, 0)
    )

  return(df)
}

train_data <- create_features(train_data)
test_data <- create_features(test_data)
cat("  - 10 new features created\n")

# Drop redundant features (replaced by age features)
redundant_cols <- c("GarageYrBlt", "YearBuilt", "YearRemodAdd")
train_data <- train_data %>% select(-any_of(redundant_cols))
test_data <- test_data %>% select(-any_of(redundant_cols))
cat(paste0("  - ", length(redundant_cols), " redundant year columns dropped\n"))

# =============================================================================
# SECTION 4: TRANSFORMATIONS
# =============================================================================

cat("\n[4/7] Applying transformations...\n")

# 4.1: Log transform skewed features
log_transform_cols <- c(
  "LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2",
  "BsmtUnfSF", "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", "LowQualFinSF",
  "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch",
  "3SsnPorch", "ScreenPorch", "PoolArea", "TotalSF", "TotalPorchSF"
)

for (col in log_transform_cols) {
  if (col %in% names(train_data)) {
    train_data[[col]] <- log1p(train_data[[col]])
    test_data[[col]] <- log1p(test_data[[col]])
  }
}
cat("  - Log transforms applied to 20 features\n")

# 4.2: Transform target variable (TRAIN ONLY)
if ("SalePrice" %in% names(train_data)) {
  train_data$SalePrice <- log1p(train_data$SalePrice)
  cat("  - SalePrice log-transformed\n")
}

# =============================================================================
# SECTION 5: ENCODING
# =============================================================================

cat("\n[5/7] Encoding categorical variables...\n")

# 5.1: Ordinal Encoding
ordinal_encode <- function(df, col, mapping) {
  if (col %in% names(df)) {
    df[[col]] <- mapping[as.character(df[[col]])]
    df[[col]][is.na(df[[col]])] <- 0 # Fallback for unexpected values
  }
  return(df)
}

# Quality/Condition ratings (Ex=5, Gd=4, TA=3, Fa=2, Po=1, None/NA=0)
quality_mapping <- c("Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0)
quality_cols <- c(
  "ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC",
  "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC"
)

for (col in quality_cols) {
  train_data <- ordinal_encode(train_data, col, quality_mapping)
  test_data <- ordinal_encode(test_data, col, quality_mapping)
}

# Basement Exposure
bsmt_exp_mapping <- c("Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "None" = 0)
train_data <- ordinal_encode(train_data, "BsmtExposure", bsmt_exp_mapping)
test_data <- ordinal_encode(test_data, "BsmtExposure", bsmt_exp_mapping)

# Basement Finish Type
bsmt_fin_mapping <- c("GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1, "None" = 0)
train_data <- ordinal_encode(train_data, "BsmtFinType1", bsmt_fin_mapping)
test_data <- ordinal_encode(test_data, "BsmtFinType1", bsmt_fin_mapping)
train_data <- ordinal_encode(train_data, "BsmtFinType2", bsmt_fin_mapping)
test_data <- ordinal_encode(test_data, "BsmtFinType2", bsmt_fin_mapping)

# Functional
functional_mapping <- c(
  "Typ" = 8, "Min1" = 7, "Min2" = 6, "Mod" = 5,
  "Maj1" = 4, "Maj2" = 3, "Sev" = 2, "Sal" = 1
)
train_data <- ordinal_encode(train_data, "Functional", functional_mapping)
test_data <- ordinal_encode(test_data, "Functional", functional_mapping)

# LotShape
lotshape_mapping <- c("Reg" = 4, "IR1" = 3, "IR2" = 2, "IR3" = 1)
train_data <- ordinal_encode(train_data, "LotShape", lotshape_mapping)
test_data <- ordinal_encode(test_data, "LotShape", lotshape_mapping)

# LandSlope
landslope_mapping <- c("Gtl" = 3, "Mod" = 2, "Sev" = 1)
train_data <- ordinal_encode(train_data, "LandSlope", landslope_mapping)
test_data <- ordinal_encode(test_data, "LandSlope", landslope_mapping)

# Utilities
utilities_mapping <- c("AllPub" = 4, "NoSewr" = 3, "NoSeWa" = 2, "ELO" = 1)
train_data <- ordinal_encode(train_data, "Utilities", utilities_mapping)
test_data <- ordinal_encode(test_data, "Utilities", utilities_mapping)

# PavedDrive
paved_mapping <- c("Y" = 3, "P" = 2, "N" = 1)
train_data <- ordinal_encode(train_data, "PavedDrive", paved_mapping)
test_data <- ordinal_encode(test_data, "PavedDrive", paved_mapping)

cat("  - Ordinal encoding applied to 18 features\n")

# 5.2: Convert MSSubClass to factor (already done in original, but ensure it)
if ("MSSubClass" %in% names(train_data)) {
  train_data$MSSubClass <- as.factor(train_data$MSSubClass)
  test_data$MSSubClass <- as.factor(test_data$MSSubClass)
}

# 5.3: One-hot encoding for nominal variables
nominal_cols <- c(
  "MSSubClass", "MSZoning", "Street", "Alley", "LandContour", "LotConfig",
  "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle",
  "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType",
  "Foundation", "Heating", "CentralAir", "Electrical", "GarageType",
  "GarageFinish", "Fence", "MiscFeature", "SaleType", "SaleCondition"
)

# Keep only columns that exist and are still character/factor
nominal_cols_present <- nominal_cols[nominal_cols %in% names(train_data)]
nominal_cols_to_encode <- nominal_cols_present[sapply(
  train_data[nominal_cols_present],
  function(x) is.character(x) || is.factor(x)
)]

if (length(nominal_cols_to_encode) > 0) {
  # Combine train and test to ensure same levels
  train_data$dataset_flag <- "train"
  test_data$dataset_flag <- "test"

  combined <- bind_rows(train_data, test_data)

  # One-hot encode
  dummy_formula <- as.formula(paste("~ ", paste(nominal_cols_to_encode, collapse = " + "), "- 1"))
  dummy_vars <- model.matrix(dummy_formula, data = combined)

  # Split back
  train_dummies <- dummy_vars[combined$dataset_flag == "train", ]
  test_dummies <- dummy_vars[combined$dataset_flag == "test", ]

  # Remove original categorical columns and add dummies
  train_data <- train_data %>%
    select(-all_of(nominal_cols_to_encode), -dataset_flag) %>%
    bind_cols(as.data.frame(train_dummies))

  test_data <- test_data %>%
    select(-all_of(nominal_cols_to_encode), -dataset_flag) %>%
    bind_cols(as.data.frame(test_dummies))

  cat(paste0("  - One-hot encoding applied: ", ncol(train_dummies), " dummy variables created\n"))
}

# =============================================================================
# SECTION 6: STANDARDIZATION & INTERACTION TERMS
# =============================================================================

cat("\n[6/7] Standardization and interaction terms...\n")

# 6.1: Identify numeric columns to standardize
# Exclude Id and SalePrice
numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, c("Id", "SalePrice"))

# Compute scaling parameters from training data
scaling_params <- data.frame(
  column = numeric_cols,
  mean = sapply(train_data[numeric_cols], mean, na.rm = TRUE),
  sd = sapply(train_data[numeric_cols], sd, na.rm = TRUE),
  row.names = NULL
)

# Standardize train
for (col in numeric_cols) {
  col_mean <- scaling_params$mean[scaling_params$column == col]
  col_sd <- scaling_params$sd[scaling_params$column == col]

  if (col_sd > 0) {
    train_data[[col]] <- (train_data[[col]] - col_mean) / col_sd
    test_data[[col]] <- (test_data[[col]] - col_mean) / col_sd
  }
}
cat(paste0("  - Standardized ", length(numeric_cols), " numeric features\n"))

# 6.2: Create polynomial and interaction terms
# Note: Using standardized features to keep scale consistent

# Squared terms
if ("GrLivArea" %in% names(train_data)) {
  train_data$GrLivArea_sq <- train_data$GrLivArea^2
  test_data$GrLivArea_sq <- test_data$GrLivArea^2
}

if ("TotalSF" %in% names(train_data)) {
  train_data$TotalSF_sq <- train_data$TotalSF^2
  test_data$TotalSF_sq <- test_data$TotalSF^2
}

if ("LotArea" %in% names(train_data)) {
  train_data$LotArea_sq <- train_data$LotArea^2
  test_data$LotArea_sq <- test_data$LotArea^2
}

if ("HouseAge" %in% names(train_data)) {
  train_data$HouseAge_sq <- train_data$HouseAge^2
  test_data$HouseAge_sq <- test_data$HouseAge^2
}

if ("RemodAge" %in% names(train_data)) {
  train_data$RemodAge_sq <- train_data$RemodAge^2
  test_data$RemodAge_sq <- test_data$RemodAge^2
}

# Interaction terms
if (all(c("OverallQual", "GrLivArea") %in% names(train_data))) {
  train_data$OverallQual_x_GrLivArea <- train_data$OverallQual * train_data$GrLivArea
  test_data$OverallQual_x_GrLivArea <- test_data$OverallQual * test_data$GrLivArea
}

if (all(c("OverallQual", "TotalSF") %in% names(train_data))) {
  train_data$OverallQual_x_TotalSF <- train_data$OverallQual * train_data$TotalSF
  test_data$OverallQual_x_TotalSF <- test_data$OverallQual * test_data$TotalSF
}

if (all(c("GarageCars", "GarageArea") %in% names(train_data))) {
  train_data$GarageCars_x_GarageArea <- train_data$GarageCars * train_data$GarageArea
  test_data$GarageCars_x_GarageArea <- test_data$GarageCars * test_data$GarageArea
}

if (all(c("TotalBath", "GrLivArea") %in% names(train_data))) {
  train_data$TotalBath_x_GrLivArea <- train_data$TotalBath * train_data$GrLivArea
  test_data$TotalBath_x_GrLivArea <- test_data$TotalBath * test_data$GrLivArea
}

if (all(c("HouseAge", "OverallQual") %in% names(train_data))) {
  train_data$HouseAge_x_OverallQual <- train_data$HouseAge * train_data$OverallQual
  test_data$HouseAge_x_OverallQual <- test_data$HouseAge * test_data$OverallQual
}

cat("  - 10 polynomial/interaction terms created\n")

# =============================================================================
# SECTION 7: FINAL VALIDATION
# =============================================================================

cat("\n[7/7] Final validation checks...\n")

# Check for remaining NAs
train_na_count <- sum(is.na(train_data))
test_na_count <- sum(is.na(test_data))

if (train_na_count > 0) {
  cat(paste0("  WARNING: ", train_na_count, " NAs remaining in training data\n"))

  # Detailed NA report for train
  train_na_cols <- colSums(is.na(train_data))
  train_na_cols <- train_na_cols[train_na_cols > 0]

  if (length(train_na_cols) > 0) {
    cat("\n  Columns with NAs in TRAIN:\n")
    for (col_name in names(train_na_cols)) {
      cat(paste0("    - ", col_name, ": ", train_na_cols[col_name], " NAs\n"))
    }
  }
}

if (test_na_count > 0) {
  cat(paste0("  WARNING: ", test_na_count, " NAs remaining in test data\n"))

  # Detailed NA report for test
  test_na_cols <- colSums(is.na(test_data))
  test_na_cols <- test_na_cols[test_na_cols > 0]

  if (length(test_na_cols) > 0) {
    cat("\n  Columns with NAs in TEST:\n")
    for (col_name in names(test_na_cols)) {
      cat(paste0("    - ", col_name, ": ", test_na_cols[col_name], " NAs\n"))
    }
  }
}

if (train_na_count == 0 && test_na_count == 0) {
  cat("  - No NAs remaining in datasets ✓\n")
}

# Check column alignment
train_cols <- setdiff(names(train_data), "SalePrice")
test_cols <- names(test_data)
common_cols <- intersect(train_cols, test_cols)

if (length(setdiff(test_cols, train_cols)) > 0) {
  cat("  WARNING: Test has columns not in train\n")
}

cat(paste0("  - Train dimensions: ", nrow(train_data), " x ", ncol(train_data), "\n"))
cat(paste0("  - Test dimensions: ", nrow(test_data), " x ", ncol(test_data), "\n"))
cat(paste0("  - Common features: ", length(common_cols), "\n"))

# Verify row counts unchanged
if (nrow(train_data) != n_train) {
  cat("  ERROR: Training data row count changed!\n")
}
if (nrow(test_data) != n_test) {
  cat("  ERROR: Test data row count changed!\n")
}

cat("\n========================================\n")
cat("  PREPROCESSING COMPLETE\n")
cat("========================================\n\n")
