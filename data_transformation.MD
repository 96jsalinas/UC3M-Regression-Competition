# Data Transformation Log

The following transformations have been applied to both the training and testing datasets in `preprocessing.R`.
### Data Inconsistencies: PoolQC, GarageQual, BsmtQual
We treat inconsistencies in data as having NA/None/0 in them. This means that if we have NA in PoolQC we will treat is as having no pool, and so we will set PoolArea = 0. (And later when treating NAs we will set it to None)
### Categorical NA -> None
These columns have missing values that indicate absence of a feature, not truly missing data:
- [ ] Alley
- [ ] MasVnrType
- [ ] BsmtQual
- [ ] BsmtCond
- [ ] BsmtExposure
- [ ] BsmtFinType1
- [ ] BsmtFinType2
- [ ] FireplaceQu
- [ ] GarageType
- [ ] GarageFinish
- [ ] GarageQual
- [ ] GarageCond
- [ ] PoolQC
- [ ] Fence
- [ ] MiscFeature

### Numeric NA -> 0
These numeric NAs indicate absence of a feature (structural missing):
- [ ] MasVnrArea - no masonry veneer → 0
- [ ] BsmtFinSF1 - no basement → 0
- [ ] BsmtFinSF2 - no basement → 0
- [ ] BsmtUnfSF - no basement → 0
- [ ] TotalBsmtSF - no basement → 0
- [ ] BsmtFullBath - no basement → 0
- [ ] BsmtHalfBath - no basement → 0
- [ ] GarageCars - no garage → 0
- [ ] GarageArea - no garage → 0

### Special case: LotFrontage
NA does not mean no frontage, it means it was not recorded. Correct imputation is:
- [ ] LotFrontage - impute using the *median LotFrontage within the same Neighborhood*, with fallback to overall median if neighborhood median is unavailable

Houses in each neighbourhood share typical lot shapes/ front widths. Imputing by global median gives worse RMSE. Imputing by 0 destroys linearity and scale. Imputing by neighbourhood keeps distribution intact.

### Test-specific missing values
The test set has missing values in columns that are complete in training. Impute with mode from training set:
- [ ] MSZoning - impute with mode from training
- [ ] Utilities - impute with mode from training
- [ ] Exterior1st - impute with mode from training
- [ ] Exterior2nd - impute with mode from training
- [ ] KitchenQual - impute with mode from training
- [ ] Functional - impute with mode from training
- [ ] SaleType - impute with mode from training
- [ ] Electrical - impute with mode from training (also has 1 missing in train)

### Fix skewness of predictors
Log-transform strongly right-skewed variables. This helps linearity and homoscedasticity. (log(x + 1))
- [ ] LotFrontage
- [ ] LotArea
- [ ] MasVnrArea
- [ ] BsmtFinSF1
- [ ] BsmtFinSF2
- [ ] BsmtUnfSF
- [ ] TotalBsmtSF
- [ ] 1stFlrSF
- [ ] 2ndFlrSF
- [ ] LowQualFinSF
- [ ] GrLivArea
- [ ] GarageArea
- [ ] WoodDeckSF
- [ ] OpenPorchSF
- [ ] EnclosedPorch
- [ ] 3SsnPorch
- [ ] ScreenPorch
- [ ] PoolArea
- [ ] TotalSF
- [ ] TotalPorchSF
- [ ] MiscVal


### Fix non-linear relationships
Many house-price relationships are curved:
- [ ] GrLivArea^2 - add squared term
- [ ] TotalSF^2 - add squared term
- [ ] LotArea^2 - add squared term (after log)
- [ ] HouseAge^2 - add squared term
- [ ] RemodAge^2 - add squared term
- [ ] OverallQual * GrLivArea - add interaction
- [ ] OverallQual * TotalSF - add interaction
- [ ] OverallQual * Neighborhood - interactions with all dummies
- [ ] GarageCars * GarageArea - add interaction
- [ ] TotalBath * GrLivArea - add interaction
- [ ] HouseAge * OverallQual - add interaction


### Ordinal Encoding
These categorical variables have inherent order and should be mapped to numeric values. See `data_description.txt` for specific category codes.

**Quality/Condition ratings** (Ex=5, Gd=4, TA=3, Fa=2, Po=1, None/NA=0):
- [ ] ExterQual - exterior material quality
- [ ] ExterCond - exterior material condition
- [ ] BsmtQual - basement height quality
- [ ] BsmtCond - basement condition
- [ ] HeatingQC - heating quality and condition
- [ ] KitchenQual - kitchen quality
- [ ] FireplaceQu - fireplace quality
- [ ] GarageQual - garage quality
- [ ] GarageCond - garage condition
- [ ] PoolQC - pool quality

**Basement features** (see data_description.txt for codes):
- [ ] BsmtExposure - walkout/garden level walls (Gd=4, Av=3, Mn=2, No=1, NA=0)
- [ ] BsmtFinType1 - basement finished area rating (GLQ=6, ALQ=5, BLQ=4, Rec=3, LwQ=2, Unf=1, NA=0)
- [ ] BsmtFinType2 - basement finished area rating (if multiple types) (same as BsmtFinType1)

**Functionality and shape**:
- [ ] Functional - home functionality (Typ=8, Min1=7, Min2=6, Mod=5, Maj1=4, Maj2=3, Sev=2, Sal=1)
- [ ] LotShape - general shape of property (Reg=4, IR1=3, IR2=2, IR3=1)
- [ ] LandSlope - slope of property (Gtl=3, Mod=2, Sev=1)

**Utilities and access**:
- [ ] Utilities - type of utilities available (AllPub=4, NoSewr=3, NoSeWa=2, ELO=1)
- [ ] PavedDrive - paved driveway (Y=3, P=2, N=1)

### Nominal Encoding (One-Hot)
These categorical variables have no inherent order and should be one-hot encoded:
- [ ] MSSubClass - one-hot (already converted to factor)
- [ ] MSZoning - one-hot
- [ ] Street - one-hot
- [ ] Alley - one-hot
- [ ] LandContour - one-hot
- [ ] LotConfig - one-hot
- [ ] Neighborhood - one-hot
- [ ] Condition1 - one-hot
- [ ] Condition2 - one-hot
- [ ] BldgType - one-hot
- [ ] HouseStyle - one-hot
- [ ] RoofStyle - one-hot
- [ ] RoofMatl - one-hot
- [ ] Exterior1st - one-hot
- [ ] Exterior2nd - one-hot
- [ ] MasVnrType - one-hot
- [ ] Foundation - one-hot
- [ ] Heating - one-hot
- [ ] CentralAir - one-hot
- [ ] Electrical - one-hot
- [ ] GarageType - one-hot
- [ ] GarageFinish - one-hot
- [ ] Fence - one-hot
- [ ] MiscFeature - one-hot
- [ ] SaleType - one-hot
- [ ] SaleCondition - one-hot

### Create these new variables
- [ ] TotalSF: TotalBsmtSF + 1stFlrSF + 2ndFlrSF
- [ ] TotalBath: FullBath + 0.5*HalfBath + BsmtFullBath + 0.5*BsmtHalfBath
- [ ] TotalPorchSF: OpenPorchSF + EnclosedPorch + 3SsnPorch + ScreenPorch
- [ ] HouseAge: YrSold - YearBuilt
- [ ] RemodAge: YrSold - YearRemodAdd
- [ ] GarageAge: YrSold - GarageYrBlt (set to 0 for missing GarageYrBlt)
- [ ] HasGarage: 1 if GarageArea > 0 else 0
- [ ] HasBasement: 1 if TotalBsmtSF > 0 else 0
- [ ] HasFireplace: 1 if Fireplaces > 0 else 0
- [ ] HasPool: 1 if PoolArea > 0 else 0

### Drop redundant features
After creating age features, drop the original year columns to avoid multicollinearity and improve interpretability:
- [ ] GarageYrBlt - replaced by GarageAge
- [ ] YearBuilt - replaced by HouseAge
- [ ] YearRemodAdd - replaced by RemodAge

**Keep:** YrSold and MoSold (capture market timing and seasonal effects)

### Transform target variable (TRAIN ONLY)
- [ ] SalePrice - log1p transformation to normalize distribution

## Improve model stability

### Standardization order (CRITICAL)
Standardize in this specific order to ensure proper scaling:

**Step 1: Standardize base numerical features** (after log transforms, before interactions)
- [ ] LotFrontage (after log transform)
- [ ] LotArea (after log transform)
- [ ] MasVnrArea (after log transform)
- [ ] BsmtFinSF1 (after log transform)
- [ ] BsmtFinSF2 (after log transform)
- [ ] BsmtUnfSF (after log transform)
- [ ] TotalBsmtSF (after log transform)
- [ ] 1stFlrSF (after log transform)
- [ ] 2ndFlrSF (after log transform)
- [ ] LowQualFinSF (after log transform)
- [ ] GrLivArea (after log transform)
- [ ] GarageArea (after log transform)
- [ ] WoodDeckSF (after log transform)
- [ ] OpenPorchSF (after log transform)
- [ ] EnclosedPorch (after log transform)
- [ ] 3SsnPorch (after log transform)
- [ ] ScreenPorch (after log transform)
- [ ] PoolArea (after log transform)
- [ ] TotalSF (after log transform)
- [ ] TotalPorchSF (after log transform)
- [ ] TotalBath
- [ ] HouseAge
- [ ] RemodAge
- [ ] GarageAge
- [ ] BedroomAbvGr
- [ ] TotRmsAbvGrd
- [ ] KitchenAbvGr
- [ ] Fireplaces
- [ ] FullBath
- [ ] HalfBath
- [ ] BsmtFullBath
- [ ] BsmtHalfBath
- [ ] GarageCars
- [ ] OverallQual (already numeric 1-10, standardize as-is)
- [ ] OverallCond (already numeric 1-10, standardize as-is)
- [ ] MoSold
- [ ] YrSold

**Step 2: Create polynomial and interaction terms** (from standardized features)
Interactions of standardized features are already on compatible scale
- [ ] GrLivArea_squared
- [ ] TotalSF_squared
- [ ] LotArea_squared
- [ ] HouseAge_squared
- [ ] RemodAge_squared
- [ ] OverallQual_x_GrLivArea
- [ ] OverallQual_x_TotalSF
- [ ] OverallQual_x_Neighborhood (interactions with all dummies)
- [ ] GarageCars_x_GarageArea
- [ ] TotalBath_x_GrLivArea
- [ ] HouseAge_x_OverallQual
