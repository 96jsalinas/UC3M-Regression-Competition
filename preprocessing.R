# Preprocessing Script

# Ensure data is loaded
if (!exists("train_data") || !exists("test_data")) {
  stop("Data not found in environment. Please run main.R.")
}

# Dummy check: Print list of columns

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

  # Reorder columns to put row names (Column) as a proper column if needed,
  # but here 'Column' is already the first column.
  rownames(stats) <- NULL
  return(stats)
}

if (exists("train_data")) {
  cat("Columns in Train Data:\n")
  print(colnames(train_data))

  # 1. Missing Value Analysis
  cat("\n--- Missing Value Analysis (Train) ---\n")
  missing_train <- colSums(is.na(train_data))
  missing_train_pct <- (missing_train / nrow(train_data)) * 100
  print(missing_train[missing_train > 0])
  print(missing_train_pct[missing_train > 0])

  # 2. Duplicate Detection
  cat("\n--- Duplicate Detection (Train) ---\n")
  duplicates_train <- sum(duplicated(train_data))
  cat("Number of duplicate rows in Train:", duplicates_train, "\n")

  # 3. Anomaly Detection / Summary Statistics
  cat("\n--- Summary Statistics (Train) ---\n")
  print(summary(train_data))

  # Save Statistics to CSV
  train_stats <- generate_statistics(train_data)
  write_csv(train_stats, "train_statistics.csv")
  cat("\n[INFO] Train statistics saved to 'train_statistics.csv'\n")
}

if (exists("test_data")) {
  cat("\nColumns in Test Data:\n")
  print(colnames(test_data))

  # 1. Missing Value Analysis
  cat("\n--- Missing Value Analysis (Test) ---\n")
  missing_test <- colSums(is.na(test_data))
  missing_test_pct <- (missing_test / nrow(test_data)) * 100
  print(missing_test[missing_test > 0])
  print(missing_test_pct[missing_test > 0])

  # 2. Duplicate Detection
  cat("\n--- Duplicate Detection (Test) ---\n")
  duplicates_test <- sum(duplicated(test_data))
  cat("Number of duplicate rows in Test:", duplicates_test, "\n")

  # Save Statistics to CSV
  test_stats <- generate_statistics(test_data)
  write_csv(test_stats, "test_statistics.csv")
  cat("\n[INFO] Test statistics saved to 'test_statistics.csv'\n")

  # 3. Column Consistency Check
  cat("\n--- Column Consistency Check ---\n")
  if (exists("train_data")) {
    train_cols <- colnames(train_data)
    test_cols <- colnames(test_data)

    # Check for columns in Test that are not in Train
    extra_cols <- setdiff(test_cols, train_cols)
    if (length(extra_cols) > 0) {
      cat("Warning: Columns in Test but not in Train:", paste(extra_cols, collapse = ", "), "\n")
    } else {
      cat("All Test columns are present in Train.\n")
    }

    # Check for columns in Train that are not in Test (excluding potential target)
    missing_cols <- setdiff(train_cols, test_cols)
    if (length(missing_cols) > 0) {
      cat("Columns in Train but not in Test (Potential Target):", paste(missing_cols, collapse = ", "), "\n")
    }

    # Check for mismatched types
    common_cols <- intersect(train_cols, test_cols)
    mismatched_types <- c()
    for (col in common_cols) {
      if (class(train_data[[col]]) != class(test_data[[col]])) {
        mismatched_types <- c(mismatched_types, col)
      }
    }
    if (length(mismatched_types) > 0) {
      cat("Warning: Mismatched column types:", paste(mismatched_types, collapse = ", "), "\n")
    } else {
      cat("Column types match for common columns.\n")
    }

    # Check for new levels in categorical variables in Test
    cat("\n--- Categorical Levels Check ---\n")
    char_cols <- common_cols[sapply(train_data[common_cols], is.character)]
    for (col in char_cols) {
      train_levels <- unique(train_data[[col]])
      test_levels <- unique(test_data[[col]])
      new_levels <- setdiff(test_levels, train_levels)
      # Remove NA from consideration if present
      new_levels <- new_levels[!is.na(new_levels)]

      if (length(new_levels) > 0) {
        cat("Warning: New levels in Test for column", col, ":", paste(new_levels, collapse = ", "), "\n")
      }
    }
  }
}
