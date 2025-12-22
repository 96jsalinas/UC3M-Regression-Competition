# UC3M Regression Competition

## Project Description

This project builds a regression model for the UC3M Regression Models course. The goal is to predict house sale prices using the Ames Housing dataset. The pipeline includes comprehensive data preprocessing (handling missing values, feature engineering, transformations, and encoding), a baseline linear model for reference, and an Elastic Net regression model with cross-validated hyperparameter tuning for final predictions.

## How to Run the Project

### Prerequisites

Make sure you have R installed along with the following packages:
- `readr`
- `dplyr`
- `openxlsx`
- `glmnet`
- `caret`
- `tictoc`

You can install missing packages with:
```r
install.packages(c("readr", "dplyr", "openxlsx", "glmnet", "caret", "tictoc"))
```

### Running the Pipeline

1. Open the project in RStudio using `UC3M-Regression-Competition.Rproj`
2. Run the main script:
   ```r
   source("main.R")
   ```

This will execute the full pipeline:
1. Load the training and test datasets from `Provided files/`
2. Preprocess the data (missing value imputation, feature engineering, encoding, standardization)
3. Fit a baseline linear model for reference
4. Train an Elastic Net model with cross-validation over alpha and lambda
5. Generate predictions and save them to `Provided files/predicted_prices.xlsx`
6. Produce evaluation plots (Q-Q plot, residuals vs fitted, observed vs predicted)

## Project Structure

```
UC3M-Regression-Competition/
│
├── main.R                      # Main entry point - orchestrates the full pipeline
│
├── src/                        # Source code modules
│   ├── preprocessing.R         # Data cleaning, imputation, feature engineering,
│   │                           # transformations, encoding, and standardization
│   ├── Baseline_Model.R        # Simple linear model for baseline comparison
│   ├── model_building.R        # Elastic Net model with cross-validation
│   ├── evaluation.R            # Model evaluation and diagnostic plots
│   ├── LassoModel.R            # Model evaluation using base Lasso regression
│   └── RidgeModel.R            # Model evaluation using base Ridge regression 
│
├── Provided files/             # Input data and competition materials
│   ├── train.csv               # Training dataset
│   ├── test.csv                # Test dataset (no target)
│   ├── data_description.txt    # Feature descriptions
│   ├── competition.pdf         # Competition guidelines
│   └── predicted_prices.xlsx   # Model predictions (output)
│
├── output/                     # Generated statistics and logs
│   ├── train_statistics.csv    # Summary statistics for training data
│   ├── test_statistics.csv     # Summary statistics for test data
│   ├── train_statistics_processed.csv  # Statistics after preprocessing
│   └── test_statistics_processed.csv   # Statistics after preprocessing
│
├── figures/                    # Generated plots and visualizations
│
├── UC3M-Regression-Competition.Rproj  # RStudio project file
├── .gitignore                  # Git ignore rules
├── .gitattributes              # Git attributes
└── .lintr                      # R linting configuration
```
