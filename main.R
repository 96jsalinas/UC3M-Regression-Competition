library(readr)
library(dplyr)
# Setting seed for reproducibility
set.seed(1000)

# Define file paths
# Assuming the script is run from the root of the project directory
train_path <- "./Provided files/train.csv"
test_path <- "./Provided files/test.csv"

# Load the data
if (file.exists(train_path)) {
  train_data <- read_csv(train_path, show_col_types = FALSE)
} else {
  stop("Error: Train file not found at ", train_path)
}

if (file.exists(test_path)) {
  test_data <- read_csv(test_path, show_col_types = FALSE)
} else {
  stop("Error: Test file not found at ", test_path)
}

# Execute Pipeline
source("src/preprocessing.R")
source("src/model_building.R")
source("src/evaluation.R")
