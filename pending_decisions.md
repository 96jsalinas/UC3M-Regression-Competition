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
- [ ] **High Missing Value Columns** <!-- id: 0 -->
    - [ ] Decide on `PoolQC`, `MiscFeature`, `Alley`, `Fence` (Drop vs Binary Flag)
    - [ ] Resolve `PoolQC` inconsistency (NA but PoolArea > 0)
    - [ ] Resolve `Garage` inconsistency (1 row in Test: NA but GarageArea=360)
    - [ ] Resolve `Basement` inconsistency (2 rows in Test: NA but TotalBsmtSF > 0)
- [ ] **GarageYrBlt** <!-- id: 1 -->
    - [ ] Decide handling (Drop vs GarageAge vs Impute)
- [ ] **True Missing Values** <!-- id: 2 -->
    - [ ] `LotFrontage` (Median imputation?)
    - [ ] `Electrical` (Mode imputation?)
- [ ] **Test Set Specific Missing Values** <!-- id: 3 -->
    - [ ] Categorical cols (`MSZoning`, `Utilities`, etc.) -> Mode?
    - [ ] Numeric cols (`BsmtFinSF1`, `GarageArea`, etc.) -> 0?
- [ ] **Feature Engineering** <!-- id: 4 -->
    - [ ] Create Age features (YearBuilt, YearRemodAdd, YrSold)
    - [ ] Create binary flags (HasPool, HasGarage, HasAlley, etc.)
- [ ] **Transformations** <!-- id: 5 -->
    - [ ] Log-transform `SalePrice`
    - [ ] Log-transform skewed numeric predictors
- [ ] **Encoding** <!-- id: 6 -->
    - [ ] Ordinal Encoding for quality variables
    - [ ] One-Hot Encoding for nominal variables
- [ ] **Outliers** <!-- id: 7 -->
    - [ ] Analyze and remove `GrLivArea` outliers

## Modeling
- [ ] Baseline Model
- [ ] Model Iteration
