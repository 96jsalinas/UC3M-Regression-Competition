# Data Transformation Log

The following transformations have been applied to both the training and testing datasets in `preprocessing.R`.

## 1. Structural Missing Value Imputation
Missing values in the following columns were treated as "Structural Missing" (indicating the absence of the feature) rather than missing data.

### Imputed with "None"
*   **Garage Features**: `GarageType`, `GarageFinish`, `GarageQual`, `GarageCond`
    *   *Note: `GarageYrBlt` still needs checking, it has empty values but putting 0 will skew the data*
*   **Basement Features**: `BsmtQual`, `BsmtCond`, `BsmtExposure`, `BsmtFinType1`, `BsmtFinType2`
*   **Fireplace**: `FireplaceQu`
*   **Masonry**: `MasVnrType`

### Imputed with 0
*   **Masonry**: `MasVnrArea`

## 2. Type Conversions
*   **MSSubClass**: Converted from numeric to categorical (factor).

# Eoin's preprocess things
- We treat inconsistencies in data as having NA/None/0 in them. 
## Categorical NA -> None
[ ] Alley
[ ] BsmtQual
[ ] BsmtCond
[ ] BsmtExposure
[ ] BsmtFinType1
[ ] BsmtFinType2
[ ] FireplaceQu
[ ] GarageType
[ ] GarageFinish
[ ] GarageQual
[ ] GarageCond
[ ] PoolQC
[ ] Fence
[ ] MiscFeature

## Numeric NA -> 0
[ ] MasVnrArea       – no masonry veneer → 0
[ ] BsmtFinSF1       – no basement → 0
[ ] BsmtFinSF2       – no basement → 0
[ ] BsmtUnfSF        – no basement → 0
[ ] TotalBsmtSF      – no basement → 0
[ ] BsmtFullBath     – no basement → 0
[ ] BsmtHalfBath     – no basement → 0
[ ] GarageYrBlt      – no garage → 0 (or very rarely median year)
[ ] GarageCars       – no garage → 0
[ ] GarageArea       – no garage → 0
[ ] MasVnrArea       – no masonry → 0

## Special case: LotFrontage
- NA does not mean no frontage, it means it was not recorded. Correct imputation is 
[ ] LotFrontage  – impute using the *median LotFrontage within the same Neighborhood*
- Houses in each neighbourhood share typical lot shapes/ front widths. Imputing by global median gives worse RMSE. Imputing by 0 destroys linearity and scale. Imputing by neighbourhood keeps distribution intact.



# Transformations on data to do
## (A) Fix skewness of predictors
Log-transform strongly right-skewed variables. This helps linearity and homoscedasticity. (log(x + 1))
-Extremely skewed features contain:
[ ] LotFrontage
[ ] LotArea
[ ] MasVnrArea
[ ] BsmtFinSF1
[ ] BsmtFinSF2
[ ] BsmtUnfSF
[ ] TotalBsmtSF
[ ] 1stFlrSF
[ ] 2ndFlrSF
[ ] LowQualFinSF
[ ] GrLivArea
[ ] GarageArea
[ ] WoodDeckSF
[ ] OpenPorchSF
[ ] EnclosedPorch
[ ] 3SsnPorch
[ ] ScreenPorch
[ ] PoolArea
[ ] TotalSF
[ ] TotalPorchSF


##(B) Fix non-linear relationships
Many house-price relationships are curved:
[ ] GrLivArea^2            – add squared term
[ ] TotalSF^2              – add squared term
[ ] LotArea^2              – add squared term (after log)
[ ] HouseAge^2             – add squared term
[ ] RemodAge^2             – add squared term
[ ] OverallQual * GrLivArea – add interaction
[ ] OverallQual * TotalSF   – add interaction
[ ] OverallQual * Neighborhood – interactions with all dummies
[ ] GarageCars * GarageArea – add interaction
[ ] TotalBath * GrLivArea   – add interaction
[ ] HouseAge * OverallQual  – add interaction


##(C) Make Categorical strusture representable
Categoricals must be:
[ ] MSZoning              – one-hot
[ ] Street                – one-hot
[ ] Alley                 – one-hot
[ ] LotShape              – one-hot
[ ] LandContour           – one-hot
[ ] Utilities             – one-hot
[ ] LotConfig             – one-hot
[ ] LandSlope             – one-hot
[ ] Neighborhood          – one-hot
[ ] Condition1            – one-hot
[ ] Condition2            – one-hot
[ ] BldgType              – one-hot
[ ] HouseStyle            – one-hot
[ ] RoofStyle             – one-hot
[ ] RoofMatl              – one-hot
[ ] Exterior1st           – one-hot
[ ] Exterior2nd           – one-hot
[ ] MasVnrType            – one-hot
[ ] ExterQual             – one-hot
[ ] ExterCond             – one-hot
[ ] Foundation            – one-hot
[ ] BsmtQual              – one-hot
[ ] BsmtCond              – one-hot
[ ] BsmtExposure          – one-hot
[ ] BsmtFinType1          – one-hot
[ ] BsmtFinType2          – one-hot
[ ] Heating               – one-hot
[ ] HeatingQC             – one-hot
[ ] CentralAir            – one-hot
[ ] Electrical            – one-hot
[ ] KitchenQual           – one-hot
[ ] Functional            – one-hot
[ ] FireplaceQu           – one-hot
[ ] GarageType            – one-hot
[ ] GarageFinish          – one-hot
[ ] GarageQual            – one-hot
[ ] GarageCond            – one-hot
[ ] PavedDrive            – one-hot
[ ] PoolQC                – one-hot
[ ] Fence                 – one-hot
[ ] MiscFeature           – one-hot
[ ] SaleType              – one-hot
[ ] SaleCondition         – one-hot

## (D) Make these variables
[ ] TotalSF
      – TotalBsmtSF + 1stFlrSF + 2ndFlrSF

[ ] TotalBath
      – FullBath + 0.5*HalfBath + BsmtFullBath + 0.5*BsmtHalfBath

[ ] TotalPorchSF
      – OpenPorchSF + EnclosedPorch + 3SsnPorch + ScreenPorch

[ ] HouseAge
      – YrSold - YearBuilt

[ ] RemodAge
      – YrSold - YearRemodAdd

[ ] HasGarage
      – 1 if GarageArea > 0 else 0

[ ] HasBasement
      – 1 if TotalBsmtSF > 0 else 0

[ ] HasFireplace
      – 1 if Fireplaces > 0 else 0

[ ] HasPool
      – 1 if PoolArea > 0 else 0

## (E) Improve model stability
Standardise all numerical predictors (mean = 0, sd = 1).
[ ] LotFrontage
[ ] LotArea
[ ] MasVnrArea
[ ] BsmtFinSF1
[ ] BsmtFinSF2
[ ] BsmtUnfSF
[ ] TotalBsmtSF
[ ] 1stFlrSF
[ ] 2ndFlrSF
[ ] LowQualFinSF
[ ] GrLivArea
[ ] GarageArea
[ ] WoodDeckSF
[ ] OpenPorchSF
[ ] EnclosedPorch
[ ] 3SsnPorch
[ ] ScreenPorch
[ ] PoolArea

[ ] TotalSF
[ ] TotalPorchSF
[ ] TotalBath
[ ] HouseAge
[ ] RemodAge

[ ] BedroomAbvGr
[ ] TotRmsAbvGrd
[ ] KitchenAbvGr
[ ] Fireplaces
[ ] FullBath
[ ] HalfBath
[ ] BsmtFullBath
[ ] BsmtHalfBath
[ ] GarageCars
[ ] GarageYrBlt
[ ] OverallQual
[ ] OverallCond

[ ] MoSold
[ ] YrSold
