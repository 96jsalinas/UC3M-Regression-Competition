# Data Analysis & Preprocessing Summary

> **Status**: This document reflects the current state of the preprocessing pipeline implemented in `src/preprocessing.R`.

## Dataset Overview

| Metric | Training Set | Test Set |
|--------|-------------|----------|
| **Observations** | 1,460 | 1,459 |
| **Original Features** | 81 | 80 |
| **Processed Features** | 242 | 241 |
| **Target Variable** | SalePrice | N/A |

---

## 1. Missing Value Handling

### Structural Missing Values (NA = Absence of Feature)

These columns had NAs that indicate the *absence* of a feature, not truly missing data. They were imputed as follows:

| Category | Columns | Train NA% | Action |
|----------|---------|-----------|--------|
| **Garage** | `GarageType`, `GarageFinish`, `GarageQual`, `GarageCond` | 5.55% | Categorical → "None" |
| **Basement** | `BsmtQual`, `BsmtCond`, `BsmtExposure`, `BsmtFinType1`, `BsmtFinType2` | 2.5-2.6% | Categorical → "None" |
| **Fireplace** | `FireplaceQu` | 47.26% | Categorical → "None" |
| **Pool** | `PoolQC` | 99.52% | Categorical → "None" |
| **Other** | `Alley`, `Fence`, `MiscFeature`, `MasVnrType` | 0.5-96% | Categorical → "None" |
| **Numeric** | `MasVnrArea`, `BsmtFinSF1/2`, `BsmtUnfSF`, `TotalBsmtSF`, `BsmtFullBath`, `BsmtHalfBath`, `GarageCars`, `GarageArea` | 0-5.5% | Numeric → 0 |

### True Missing Values

| Column | Train NA% | Imputation Method |
|--------|-----------|-------------------|
| `LotFrontage` | 17.74% | Neighborhood median (fallback: overall median) |
| `Electrical` | 0.07% | Mode from training data |

### Test-Set-Specific Missing Values

These columns were complete in training but had NAs in test. Imputed with the **mode** from training data:
- `MSZoning`, `Utilities`, `Exterior1st`, `Exterior2nd`, `KitchenQual`, `Functional`, `SaleType`

---

## 2. Feature Engineering

### Created Features (10 new)

| New Feature | Formula | Rationale |
|-------------|---------|-----------|
| `TotalSF` | `TotalBsmtSF + 1stFlrSF + 2ndFlrSF` | Total living space |
| `TotalBath` | `FullBath + 0.5*HalfBath + BsmtFullBath + 0.5*BsmtHalfBath` | Weighted bathroom count |
| `TotalPorchSF` | `OpenPorchSF + EnclosedPorch + 3SsnPorch + ScreenPorch` | Total porch space |
| `HouseAge` | `YrSold - YearBuilt` | More interpretable than year |
| `RemodAge` | `YrSold - YearRemodAdd` | Recency of remodeling |
| `GarageAge` | `YrSold - GarageYrBlt` (0 if NA) | Age of garage |
| `HasGarage` | `1 if GarageArea > 0` | Binary flag |
| `HasBasement` | `1 if TotalBsmtSF > 0` | Binary flag |
| `HasFireplace` | `1 if Fireplaces > 0` | Binary flag |
| `HasPool` | `1 if PoolArea > 0` | Binary flag |

### Dropped Features (3)

| Dropped | Reason |
|---------|--------|
| `YearBuilt` | Replaced by `HouseAge` |
| `YearRemodAdd` | Replaced by `RemodAge` |
| `GarageYrBlt` | Replaced by `GarageAge` |

---

## 3. Transformations

### Log Transformations (18 features + target)

Applied `log1p(x)` to normalize right-skewed distributions:

| Feature | Original Skew | Post-Transform Skew | Improvement |
|---------|---------------|---------------------|-------------|
| `SalePrice` | 1.88 | 0.12 | ✅ Excellent |
| `LotArea` | 12.18 | -0.14 | ✅ Excellent |
| `GrLivArea` | 1.36 | -0.01 | ✅ Excellent |
| `1stFlrSF` | 1.37 | 0.08 | ✅ Excellent |
| `LotFrontage` | 2.16 | -0.91 | ✅ Good |
| `MasVnrArea` | 2.66 | 0.50 | ✅ Good |
| `BsmtFinSF1` | 1.68 | -0.62 | ✅ Good |
| `WoodDeckSF` | 1.54 | 0.15 | ✅ Good |
| `OpenPorchSF` | 2.36 | -0.02 | ✅ Good |
| `2ndFlrSF` | 0.81 | 0.29 | ✅ Good |
| `EnclosedPorch` | 3.08 | 2.11 | ⚠️ Zero-inflated |
| `BsmtFinSF2` | 4.25 | 2.52 | ⚠️ Zero-inflated |
| `ScreenPorch` | 4.11 | 3.14 | ⚠️ Zero-inflated |
| `MiscVal` | 24.43 | 5.16 | ⚠️ Zero-inflated |
| `LowQualFinSF` | 8.99 | 7.45 | ⚠️ Zero-inflated |
| `3SsnPorch` | 10.28 | 7.72 | ⚠️ Zero-inflated |
| `PoolArea` | 14.80 | 14.33 | ⚠️ Zero-inflated (99% zeros) |

**Note**: Features marked ⚠️ are "zero-inflated" (mostly zeros). The log transform helps dampen outliers but cannot fix the fundamental bimodal distribution.

### Features NOT Log-Transformed (by design)

| Feature | Original Skew | Reason |
|---------|---------------|--------|
| `GarageArea` | 0.18 | Already nearly normal |
| `TotalBsmtSF` | 1.52 | Zero-inflation caused negative skew (-5.14) when transformed |
| `BsmtUnfSF` | 0.92 | Zero-inflation caused negative skew (-2.18) when transformed |

---

## 4. Encoding

### Ordinal Encoding (18 features)

Mapped to numeric values preserving inherent order:

| Category | Features | Mapping |
|----------|----------|---------|
| **Quality/Condition** | `ExterQual`, `ExterCond`, `BsmtQual`, `BsmtCond`, `HeatingQC`, `KitchenQual`, `FireplaceQu`, `GarageQual`, `GarageCond`, `PoolQC` | Ex=5, Gd=4, TA=3, Fa=2, Po=1, None=0 |
| **Basement Exposure** | `BsmtExposure` | Gd=4, Av=3, Mn=2, No=1, None=0 |
| **Basement Finish** | `BsmtFinType1`, `BsmtFinType2` | GLQ=6, ALQ=5, BLQ=4, Rec=3, LwQ=2, Unf=1, None=0 |
| **Functional** | `Functional` | Typ=8 → Sal=1 |
| **Other** | `LotShape`, `LandSlope`, `Utilities`, `PavedDrive` | Custom ordinal mappings |

### One-Hot Encoding (26 features)

Nominal categorical variables converted to dummy variables:
- `MSSubClass`, `MSZoning`, `Street`, `Alley`, `LandContour`, `LotConfig`, `Neighborhood`, `Condition1`, `Condition2`, `BldgType`, `HouseStyle`, `RoofStyle`, `RoofMatl`, `Exterior1st`, `Exterior2nd`, `MasVnrType`, `Foundation`, `Heating`, `CentralAir`, `Electrical`, `GarageType`, `GarageFinish`, `Fence`, `MiscFeature`, `SaleType`, `SaleCondition`

---

## 5. Standardization & Interactions

### Standardization
All numeric features (except `Id` and `SalePrice`) scaled to mean=0, SD=1 using **training data parameters only** (no data leakage).

### Polynomial Terms (5)
- `GrLivArea_sq`, `TotalSF_sq`, `LotArea_sq`, `HouseAge_sq`, `RemodAge_sq`

### Interaction Terms (5)
- `OverallQual_x_GrLivArea`, `OverallQual_x_TotalSF`, `GarageCars_x_GarageArea`, `TotalBath_x_GrLivArea`, `HouseAge_x_OverallQual`

---

## 6. Key Features by Correlation with SalePrice

| Feature | Correlation | Notes |
|---------|-------------|-------|
| `OverallQual` | 0.79 | Strongest predictor |
| `GrLivArea` | 0.71 | Above-ground living area |
| `GarageCars` | 0.64 | Garage capacity |
| `GarageArea` | 0.62 | Garage size |
| `TotalBsmtSF` | 0.61 | Basement size |
| `1stFlrSF` | 0.61 | First floor size |
| `FullBath` | 0.56 | Full bathrooms |
| `YearBuilt` | 0.52 | (now `HouseAge`) |
| `YearRemodAdd` | 0.51 | (now `RemodAge`) |
| `Fireplaces` | 0.47 | Number of fireplaces |

---

## 7. Pending Analysis

- [ ] **Outlier Detection**: Identify influential points using Cook's Distance, Studentized Residuals, and Leverage (Hat values)
- [ ] **Multicollinearity Check**: VIF analysis on final feature set
- [ ] **Feature Selection**: LASSO/Ridge regularization path analysis
