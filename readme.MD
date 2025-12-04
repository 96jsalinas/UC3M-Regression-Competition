# UC3M Regression Competition

## Project Overview
The goal of this project is to analyze the provided dataset and create a predictive model for house prices. The dataset contains various features describing aspects of residential homes in Ames, Iowa.

## Data Description
The data is split into `train.csv` and `test.csv`.

(See `Provided files/data_description.txt` for full details).

## Project Structure

```
UC3M-Regression-Competition/
├── main.R                    # Entry point - loads data and runs pipeline
├── src/                      # R source files
│   ├── preprocessing.R       # Data transformations and feature engineering
│   ├── model_building.R      # Model training logic
│   └── evaluation.R          # Model evaluation
├── output/                   # Generated files (see Output Files below)
├── figures/                  # Plots and visualizations
├── Provided files/           # Original competition data
│   ├── train.csv
│   ├── test.csv
│   └── data_description.txt
├── data_analysis.MD          # Summary of data analysis and preprocessing
├── data_transformation.MD    # Detailed transformation checklist
└── pending_tasks.md          # Task tracking
```

## Output Files

The `output/` directory contains the following generated files:

| File | Description |
|------|-------------|
| `train_statistics.csv` | Descriptive statistics for the **raw** training data (before preprocessing). Includes column types, missing counts, min/max/mean/median, skewness, kurtosis, and correlation with SalePrice. |
| `test_statistics.csv` | Descriptive statistics for the **raw** test data (before preprocessing). Same structure as above, without SalePrice correlation. |
| `train_statistics_processed.csv` | Descriptive statistics for the **processed** training data (after all transformations). Used to verify skewness reduction and normalization. |
| `test_statistics_processed.csv` | Descriptive statistics for the **processed** test data (after all transformations). |
| `transformation_log.csv` | Complete audit trail of every transformation applied. Columns: `Column_Name`, `Transformation_Type`, `Action_Taken`, `Reasoning`. Includes skewness values as proof for log transforms.
```

## Usage

Run the full pipeline from the project root:
```r
source("main.R")
```
