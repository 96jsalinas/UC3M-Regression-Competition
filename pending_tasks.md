# Task List

## Data Cleaning & Preprocessing
- [x] **Structural Missing Values (Part 1)**
    - [x] Impute "None" for Garage features (Type, Finish, Qual, Cond)
    - [x] Impute "None" for Basement features (Qual, Cond, Exposure, FinType1, FinType2)
    - [x] Impute "None" for FireplaceQu
    - [x] Impute "None" for MasVnrType
    - [x] Impute 0 for MasVnrArea
- [x] **Type Conversion**
    - [x] Convert MSSubClass to categorical
- [ ] **High Missing Value Columns**
    - [ ] Decide on `PoolQC`, `MiscFeature`, `Alley`, `Fence`, `GarageYrBlt`
    - [ ] For MiscFeature we could create binary flags from it (it includes info such as if they have elevator, 2nd garage, shed, tennis court or others, see data_description.txt)
    - [ ] For deciding on Alley, Fence or LotFrontage, without looking at the data it feels like they may have high correlation with other variables that may indicate expensive houses, or even the value in related variables such as MiscVal or LotArea, we could check by looking at a correlation matrix.
    - [ ] Resolve `PoolQC` inconsistency (NA but PoolArea > 0)
    - [ ] Resolve `Garage` inconsistency (1 row in Test: NA but GarageArea=360)
    - [ ] Resolve `Basement` inconsistency (2 rows in Test: NA but TotalBsmtSF > 0)
- [ ] **True Missing Values**
    - [ ] `Electrical` (Mode imputation?)
- [ ] **Test Set Specific Missing Values**
    - [ ] Categorical cols (`MSZoning`, `Utilities`, etc.) -> Mode?
    - [ ] Numeric cols (`BsmtFinSF1`, `GarageArea`, etc.) -> 0?
- [ ] **Feature Engineering**
    - [ ] Create Age features (YearBuilt, YearRemodAdd, YrSold)
    - [ ] Create binary flags (HasPool, HasGarage, HasAlley, etc.)
- [ ] **Transformations**
    - [ ] Log-transform `SalePrice`
    - [ ] Log-transform skewed numeric predictors
- [ ] **Encoding**
    - [ ] Ordinal Encoding for quality variables
    - [ ] One-Hot Encoding for nominal variables
- [ ] **Outliers**
    - [ ] Analyze and remove `GrLivArea` outliers

## Modeling
- [ ] Decide on techniques for variable selection based on the ones seen in class.
- [ ] Decide on model based on the ones seen in class.
- [ ] Baseline Model
- [ ] Model Iteration
## Reporting
- [ ] Create overleaf project and invite all participants
- [ ] Chapter on Preprocessing
    - [ ] Exploratory analysis
    - [ ] Data cleaning & preprocessing
    - [ ] Feature engineering
- [ ] Chapter on Modeling
    - [ ] Baseline Model
    - [ ] Feature selection
    - [ ] Model Iteration
- [ ] Chapter on Results
    - [ ] Goodness of fit of the model 
    - [ ] Used variables in the model
    - [ ] Other questions (see competition.pdf for details) 