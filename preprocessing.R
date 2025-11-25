# Preprocessing Script

# Ensure data is loaded
if (!exists("train_data") || !exists("test_data")) {
  stop("Data not found in environment. Please run main.R.")
}

# Dummy check: Print list of columns
if (exists("train_data")) {
  cat("Columns in Train Data:\n")
  print(colnames(train_data))
}

if (exists("test_data")) {
  cat("\nColumns in Test Data:\n")
  print(colnames(test_data))
}
