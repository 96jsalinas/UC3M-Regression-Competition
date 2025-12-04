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

---

## Preprocessing Pipeline

The preprocessing pipeline (`src/preprocessing.R`) transforms raw data into model-ready features. All transformations are computed from **training data only** and applied to both train and test sets to prevent data leakage.

### Pipeline Overview

```
Raw Data → Missing Values → Feature Engineering → Transformations → Encoding → Standardization → Interactions
```

### Step 1: Missing Value Handling

| Type | Strategy | Example |
|------|----------|---------|
| **Structural NA** (categorical) | Replace with "None" | `PoolQC = NA` → "None" (no pool) |
| **Structural NA** (numeric) | Replace with 0 | `GarageArea = NA` → 0 (no garage) |
| **True missing** | Neighborhood median | `LotFrontage` imputed by neighborhood |
| **Test-only missing** | Mode from training | `MSZoning`, `KitchenQual`, etc. |

### Step 2: Feature Engineering

**Created Features (11 total):**
- `TotalSF` = Basement + 1st Floor + 2nd Floor area
- `TotalBath` = Full + 0.5×Half bathrooms (above + basement)
- `TotalPorchSF` = All porch types combined
- `HouseAge`, `RemodAge`, `GarageAge` = Years since built/remodeled
- `HasGarage`, `HasBasement`, `HasFireplace`, `HasPool`, `HasMultipleKitchens` = Binary flags

**Dropped Features (3):**
- `YearBuilt`, `YearRemodAdd`, `GarageYrBlt` → Replaced by age features

### Step 3: Log Transformations

Applied `log1p(x)` to right-skewed features to improve linearity:

| Feature | Before Skew | After Skew | Why |
|---------|-------------|------------|-----|
| `SalePrice` | 1.88 | 0.12 | Normalize target |
| `LotArea` | 12.18 | -0.14 | Extreme right skew |
| `GrLivArea` | 1.36 | -0.01 | Moderate skew |

**Not transformed** (zero-inflation issues): `GarageArea`, `TotalBsmtSF`, `BsmtUnfSF`

### Step 4: Encoding

**Ordinal Encoding (20 features):**
Quality/condition features with inherent order (Ex > Gd > TA > Fa > Po > None).
Examples: `ExterQual`, `KitchenQual`, `GarageFinish`, `Fence`

**One-Hot Encoding (24 features → ~180 dummies):**
Nominal categories with no inherent order.
Examples: `Neighborhood`, `MSZoning`, `RoofStyle`, `SaleCondition`

### Step 5: Standardization

All numeric features scaled to mean=0, sd=1 using **training parameters only**.
- Includes ordinal-encoded features and one-hot dummies
- Required for regularized regression (LASSO/Ridge) to penalize features equally

### Step 6: Polynomial & Interaction Terms

**Squared terms (5):** Capture non-linear relationships
- `GrLivArea²`, `TotalSF²`, `LotArea²`, `HouseAge²`, `RemodAge²`

**Interactions (5):** Capture synergistic effects
- `OverallQual × GrLivArea` — Quality premium scales with size
- `OverallQual × TotalSF` — Quality amplifies total space value
- `HouseAge × OverallQual` — Quality mitigates depreciation
- `GarageCars × GarageArea` — Garage consistency
- `TotalBath × GrLivArea` — Bathrooms more valuable in larger homes

---

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Log transform target | Reduces heteroscedasticity; common for price prediction |
| Ordinal vs One-Hot | Features with quality hierarchy get ordinal; nominal categories get one-hot |
| Standardize dummies | Required for LASSO/Ridge to penalize all features equally |
| Neighborhood median for LotFrontage | Preserves local lot characteristics better than global median |
