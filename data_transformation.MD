# Data Transformation Log

The following transformations have been applied to both the training and testing datasets in `preprocessing.R`.

## 1. Structural Missing Value Imputation
Missing values in the following columns were treated as "Structural Missing" (indicating the absence of the feature) rather than missing data.

### Imputed with "None"
*   **Garage Features**: `GarageType`, `GarageFinish`, `GarageQual`, `GarageCond`
    *   *Note: `GarageYrBlt` was excluded from this step as per instructions.*
*   **Basement Features**: `BsmtQual`, `BsmtCond`, `BsmtExposure`, `BsmtFinType1`, `BsmtFinType2`
*   **Fireplace**: `FireplaceQu`
*   **Masonry**: `MasVnrType`

### Imputed with 0
*   **Masonry**: `MasVnrArea`

## 2. Type Conversions
*   **MSSubClass**: Converted from numeric to categorical (factor).
