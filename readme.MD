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
├── output/                   # Generated files (CSVs, logs)
├── figures/                  # Plots and visualizations
├── Provided files/           # Original competition data
│   ├── train.csv
│   ├── test.csv
│   └── data_description.txt
├── data_analysis.MD          # Exploratory data analysis notes
├── data_transformation.MD    # Transformation documentation
└── pending_tasks.md          # Task tracking
```

## Usage

Run the full pipeline from the project root:
```r
source("main.R")
```
