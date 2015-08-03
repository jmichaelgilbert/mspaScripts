**********************************************************************;
*	410-DL | Assignment 05;
*	Last updated: 2015-07-26 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data AHD;
	set mydata.ames_housing_data;
run; quit;

**********************************************************************;
**********************************************************************;
*	PROC CONTENTS;
*	Goal here is to save a copy of PROC CONTENTS of the AHD, then
	add in flags for each variable type based on the data dictionary;
*	We do this AFTER we define our sample population, because we're
	creating new variables. Otherwise, we'd have to do this twice;
**********************************************************************;
**********************************************************************;

*	Run PROC CONTENTS, save as data set;
proc contents data = AHD
	out = AHD_Contents noprint;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data AHD_Contents;
	set AHD_Contents(keep = name type length varnum format formatl informat 
		informl just npos nobs);
run; quit;

*	Add in variable types;
data AHD_Contents;
	set AHD_Contents;
*	Continuous;
	if (Name in ('BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'EnclosedPorch',
		'FirstFlrSF', 'GarageArea', 'GrLivArea', 'LotArea', 'LotFrontage',
		'LowQualFinSF', 'MasVnrArea', 'MiscVal', 'OpenPorchSF', 'PoolArea',
		'SalePrice', 'ScreenPorch', 'SecondFlrSF', 'ThreeSsnPorch',
		'TotalBsmtSF', 'WoodDeckSF')) 
		then Continuous = 1; else Continuous = 0;
*	Discrete;
	if (Name in ('BedroomAbvGr', 'BsmtFullBath', 'BsmtHalfBath', 'Fireplaces',
		'FullBath', 'GarageCars', 'GarageYrBlt', 'HalfBath', 'KitchenAbvGr',
		'MoSold', 'SID', 'TotRmsAbvGrd', 'YearBuilt', 'YearRemodel', 
		'YrSold'))
		then Discrete = 1; else Discrete = 0;
*	Nominal;
	if (Name in ('Alley', 'BldgType', 'CentralAir', 'Condition1', 
		'Condition2', 'Exterior1', 'Exterior2', 'Foundation', 'GarageType',
		'Heating', 'HouseStyle', 'LandContour', 'LotConfig', 'MasVnrType',
		'MiscFeature', 'Neighborhood', 'PID', 'RoofMat', 'RoofStyle',
		'SaleCondition', 'SaleType', 'Street', 'SubClass', 'Zoning'))
		then Nominal = 1; else Nominal = 0;
*	Ordinal;
	if (Name in ('BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
		'BsmtQual', 'Electrical', 'ExterCond', 'ExterQual', 'Fence',
		'FireplaceQu', 'Functional', 'GarageCond', 'GarageFinish',
		'GarageQual', 'HeatingQC', 'KitchenQual', 'LandSlope', 'LotShape',
		'OverallCond', 'OverallQual', 'PavedDrive', 'PoolQC', 'Utilities'))
		then Ordinal = 1; else Ordinal = 0;
run; quit;

proc sort data = AHD_Contents;
	by ordinal nominal discrete continuous name;
run; quit;

*	Print the output, give it a quick visual inspection;
proc print data = AHD_Contents;
	var name nobs continuous discrete nominal ordinal;
run; quit;

**********************************************************************;
**********************************************************************;
*	Define the Sample Population through an initial waterfall;
**********************************************************************;
**********************************************************************;

*	Our goal is to define a sample population representative of TYPICAL
	homes in Ames, Iowa;
*	Create a new variable, drop_condition to track drops;
*	Note: GT 4000 SQFT included as suggested in Data Dictionary;
data AHD_Initial;
	set AHD;
	format drop_condition $45.;
	if (SaleCondition ne 'Normal') then drop_condition = '01: Not Normal Condition of Sale';
	else if (BldgType ne '1Fam') then drop_condition = '02: Not Single-family Detached';
	else if (Zoning not in ('RH', 'RL', 'RP', 'RM')) then drop_condition = '03: Non-Residential Zoning';
	else if (Street ne 'Pave') then drop_condition = '04: Non-paved Road to Home';
	else if (Utilities ne 'AllPub') then drop_condition = '05: Not All Public Utilities';
	else if (GrLivArea > 4000) then drop_condition = '06: GT 4000 SQFT';
	else drop_condition = '07: Sample Population';
run; quit;

*	View table with observations remaining;
proc freq data = AHD_Initial;
	tables drop_condition;
	title 'Sample Waterfall';
run; quit;

*	Looks good, now let's save that data set;
data AHD_Sample;
	set AHD_Initial;
	if (drop_condition = '07: Sample Population');
run; quit;

**********************************************************************;
**********************************************************************;
*	Explore missing values for possible subsequent analysis;
**********************************************************************;
**********************************************************************;

*	Set the new data set;
data AHD_Sample_NoMiss;
	set AHD_Sample;
run; quit;

*	Parse all variables and identify missing values.
	Note: just because values are not missing does not mean remaining
	values are correct - typos happen;
*	Note: CMISS should be used but did not work, WHY? CMISS works on
	both character AND numeric values, whereas NMISS only works on
	numeric values;
proc means data = AHD_Sample_NoMiss NMISS N;
run; quit;

*	Another way to identify missing and non-missing variables
	This will cover both character and numeric values;
proc format;
	value $missfmt ' ' = 'Missing' other = 'Not Missing';
	value missfmt . = 'Missing' other = 'Not Missing';
run; quit;

*	PROC FREQ to explore missing data from Sample Waterfall;
proc freq data = AHD_Sample_NoMiss;
	format _CHAR_ $missfmt.;
	tables _CHAR_ / missing missprint nocum nopercent;
	format _NUMERIC_ missfmt.;
	tables _NUMERIC_ / missing missprint nocum nopercent;
run; quit;

**********************************************************************;
**********************************************************************;
*	Create new variables and indicator variables as needed ;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Create new variables where appropriate (for totals or otherwise);
**********************************************************************;

data AHD_Sample;
	set AHD_Sample;
	
*	Natural log of SalePrice;	
	logSalePrice = log(SalePrice);
	
*	Actual total baths;
	total_baths = max(FullBath, 0) + max(BsmtFullBath, 0);
	total_halfbaths = max(HalfBath, 0) + max(BsmtHalfBath, 0);
	total_baths_calc = total_baths + total_halfbaths;

*	Total SF on first and second floors;
	TotalFloorSF = FirstFlrSF + SecondFlrSF;
	
run; quit;

**********************************************************************;
*	Prescribed Indicator Variables;
**********************************************************************;

data AHD_Sample;
	set AHD_Sample;
	
*	Central Air Indicator;
	if (CentralAir = 'Y') then central_air = 1; else central_air = 0;

*	Fence Indicator;
	if (Fence ne 'NA') then fence_ind = 1; else fence_ind = 0;

*	Fireplace Indicator;
	if (Fireplaces > 0) then fireplace_ind = 1; else fireplace_ind = 0;

*	Many Fireplaces Indicator;
	if (Fireplaces > 2) then fireplaceMany_ind = 1; else fireplaceMany_ind = 0;

*	Garage Indicator;
	if (GarageCars > 0) then garage_ind = 1; else garage_ind = 0;

*	Large Garages Indicator;
	if (GarageCars > 3) then garageLg_ind = 1; else garageLg_ind = 0;

*	Multiple Garages Indicator;
	if (MiscFeature = 'Gar2') then garageMult_ind = 1; else garageMult_ind = 0;

*	Multiple Kitchens Indicator;
	if (KitchenAbvGr > 1) then kitchenMult_ind = 1; else kitchenMult_ind = 0;

*	Tennis Courts Indicator;
	if (MiscFeature = 'TenC') then tennis_ind = 1; else tennis_ind = 0;

*	Pool Indicator;
	if (PoolArea > 0) then pool_ind = 1; else pool_ind = 0;

*	Land Contour Indicator;
*	Excludes: Banked (Bnk), Hillside (HLS), and Depression (Low);
	if (LandContour = 'Lvl') then landcontourLvl_ind = 1; else landcontourLvl_ind = 0;

*	Land Slope Indicator;
*	Excludes: Moderate (Mod) and Severe (Sev);
	if (LandSlope = 'Gtl') then landslopeGtl_ind = 1; else landslopeGtl_ind = 0;

*	Brick Exterior;
	if (Exterior1 in ('BrkComm', 'BrkFace')) or (Exterior2 in ('BrkComm', 'BrkFace'))
		then brick_exterior = 1; else brick_exterior = 0;

*	Tile Roof;
	if (RoofMat = 'ClyTile') then tile_roof = 1; else tile_roof = 0;

*	Lot Shape;
	if (LotShape in ('Reg', 'IR1')) then regular_lot = 1; else regular_lot = 0;

*	Lot Configuration;
	if (LotConfig = 'Inside') then lot_inside = 1; else lot_inside = 0;
	if (LotConfig = 'Corner') then lot_corner = 1; else lot_corner = 0;
	if (LotConfig = 'CulDSac') then lot_culdsac = 1; else lot_culdsac = 0;
	if (LotConfig in ('FR2', 'FR3')) then lot_frontage = 1; else lot_frontage = 0;

run; quit;

**********************************************************************;
*	Condition & Quality Indicator Variables;
**********************************************************************;

data AHD_Sample;
	set AHD_Sample;

*	Note: most "patterned" indicator variables here excludes 
	Fair or Poor quality or conditions;
*	Be careful with these, as some properites have "NA"

*	Basement Condition Indicator;
	if (BsmtCond in ('Ex', 'Gd', 'TA')) then bsmtCond_ind = 1;
		 else bsmtCond_ind = 0;

*	Basement Quality Indicator;
	if (BsmtQual in ('Ex', 'Gd', 'TA')) then bsmtQual_ind = 1;
		else bsmtQual_ind = 0;

*	Basement NA Indicator;
	if ((BsmtCond = 'NA') or (BsmtQual = 'NA')) then bsmtNA_ind = 1;
		else bsmtNA_ind = 0;

*	Electrical Condition Indicator;
*	Excludes Fair (FuseF) and Poor (FuseP);
	if (Electrical in ('SBrkr', 'FuseA', 'Mix')) then electrical_ind = 1;
		else electrical_ind = 0;

*	Fireplace Quality Indicator (Ben Franklin Indicator);
	if (FireplaceQu = 'Po') then fireplaceQual_ind = 1;
		else fireplaceQual_ind = 0;

*	Fireplace NA Indicator;
	if (FireplaceQu = 'NA') then fireplaceNA_ind = 1; else fireplaceNA_ind = 0;

*	Garage Condition Indicator;
	if (GarageCond in ('Ex', 'Gd', 'TA')) then garageCond_ind = 1;
		else garageCond_ind = 0;

*	Garage Quality Indicator;
	if (GarageQual in ('Ex', 'Gd', 'TA')) then garageQual_ind = 1;
		else garageQual_ind = 0;

*	Garage NA Indicator;
	if ((GarageQual = 'NA') or (GarageCond = 'NA')) then garageNA_ind = 1;
		 else garageNA_ind = 0;

*	Exterior Condition Indicator;
	if (ExterCond in ('Ex', 'Gd', 'TA')) then exterCond_ind = 1;
		else exterCond_ind = 0;

*	Exterior Quality Indicator;
	if (ExterQual in ('Ex', 'Gd', 'TA')) then exterQual_ind = 1;
		else exterQual_ind = 0;

*	Functional Indicator;
*	Note: Typical and Minor deductions included here
	Excluded are Moderate, Major, Severe, and Salvage;
	if (Functional in ('Typ', 'Min1', 'Min2')) then functional_ind = 1;
		else functional_ind = 0;

run; quit;

**********************************************************************;
*	Family Indicator Variables;
**********************************************************************;

data AHD_Sample;
	set AHD_Sample;

*	Basement Condition - Family of Indicator Variables;
	if (BsmtCond = 'Ex') then BsmtCond_Ex = 1; else BsmtCond_Ex = 0;
	if (BsmtCond = 'Gd') then BsmtCond_Gd = 1; else BsmtCond_Gd = 0;
	if (BsmtCond = 'TA') then BsmtCond_TA = 1; else BsmtCond_TA = 0;
	if (BsmtCond = 'Fa') then BsmtCond_Fa = 1; else BsmtCond_Fa = 0;
	if (BsmtCond = 'Po') then BsmtCond_Po = 1; else BsmtCond_Po = 0;
	if (BsmtCond = 'NA') then BsmtCond_NA = 1; else BsmtCond_NA = 0;
	
*	Basement Quality - Family of Indicator Variables;
	if (BsmtQual = 'Ex') then BsmtQual_Ex = 1; else BsmtQual_Ex = 0;
	if (BsmtQual = 'Gd') then BsmtQual_Gd = 1; else BsmtQual_Gd = 0;
	if (BsmtQual = 'TA') then BsmtQual_TA = 1; else BsmtQual_TA = 0;
	if (BsmtQual = 'Fa') then BsmtQual_Fa = 1; else BsmtQual_Fa = 0;
	if (BsmtQual = 'Po') then BsmtQual_Po = 1; else BsmtQual_Po = 0;
	if (BsmtQual = 'NA') then BsmtQual_NA = 1; else BsmtQual_NA = 0;

*	Exterior Material Condition - Family of Indicator Variables;
	if (ExterCond = 'Ex') then ExterCond_Ex = 1; else ExterCond_Ex = 0;
	if (ExterCond = 'Gd') then ExterCond_Gd = 1; else ExterCond_Gd = 0;
	if (ExterCond = 'TA') then ExterCond_TA = 1; else ExterCond_TA = 0;
	if (ExterCond = 'Fa') then ExterCond_Fa = 1; else ExterCond_Fa = 0;
	if (ExterCond = 'Po') then ExterCond_Po = 1; else ExterCond_Po = 0;

*	Exterior Material Quality - Family of Indicator Variables;
	if (ExterQual = 'Ex') then ExterQual_Ex = 1; else ExterQual_Ex = 0;
	if (ExterQual = 'Gd') then ExterQual_Gd = 1; else ExterQual_Gd = 0;
	if (ExterQual = 'TA') then ExterQual_TA = 1; else ExterQual_TA = 0;
	if (ExterQual = 'Fa') then ExterQual_Fa = 1; else ExterQual_Fa = 0;
	if (ExterQual = 'Po') then ExterQual_Po = 1; else ExterQual_Po = 0;

*	Fireplace Quality - Family of Indicator Variables;
	if (FireplaceQu = 'Ex') then FireplaceQu_Ex = 1; else FireplaceQu_Ex = 0;
	if (FireplaceQu = 'Gd') then FireplaceQu_Gd = 1; else FireplaceQu_Gd = 0;
	if (FireplaceQu = 'TA') then FireplaceQu_TA = 1; else FireplaceQu_TA = 0;
	if (FireplaceQu = 'Fa') then FireplaceQu_Fa = 1; else FireplaceQu_Fa = 0;
	if (FireplaceQu = 'Po') then FireplaceQu_Po = 1; else FireplaceQu_Po = 0;
	if (FireplaceQu = 'NA') then FireplaceQu_NA = 1; else FireplaceQu_NA = 0;

*	Note on Garages: Do Garages and their Quality or Condition matter?
	PROC FREQ shows only 71 observations of 1939 do not have a garage,
	so they probably do. Only 8 observations of 1939 have a garage for four
	cars or more;

*	Garage Condition - Family of Indicator Variables;
	if (GarageCond = 'Ex') then GarageCond_Ex = 1; else GarageCond_Ex = 0;
	if (GarageCond = 'Gd') then GarageCond_Gd = 1; else GarageCond_Gd = 0;
	if (GarageCond = 'TA') then GarageCond_TA = 1; else GarageCond_TA = 0;
	if (GarageCond = 'Fa') then GarageCond_Fa = 1; else GarageCond_Fa = 0;
	if (GarageCond = 'Po') then GarageCond_Po = 1; else GarageCond_Po = 0;
	if (GarageCond = 'NA') then GarageCond_NA = 1; else GarageCond_NA = 0;
	
*	Garage Quality - Family of Indicator Variables;
	if (GarageQual = 'Ex') then GarageQual_Ex = 1; else GarageQual_Ex = 0;
	if (GarageQual = 'Gd') then GarageQual_Gd = 1; else GarageQual_Gd = 0;
	if (GarageQual = 'TA') then GarageQual_TA = 1; else GarageQual_TA = 0;
	if (GarageQual = 'Fa') then GarageQual_Fa = 1; else GarageQual_Fa = 0;
	if (GarageQual = 'Po') then GarageQual_Po = 1; else GarageQual_Po = 0;
	if (GarageQual = 'NA') then GarageQual_NA = 1; else GarageQual_NA = 0;

*	Kitchen Condition - Family of Indicator Variables;
	if (KitchenQual = 'Ex') then KitchenQual_Ex = 1; else KitchenQual_Ex = 0;
	if (KitchenQual = 'Gd') then KitchenQual_Gd = 1; else KitchenQual_Gd = 0;
	if (KitchenQual = 'TA') then KitchenQual_TA = 1; else KitchenQual_TA = 0;
	if (KitchenQual = 'Fa') then KitchenQual_Fa = 1; else KitchenQual_Fa = 0;
	if (KitchenQual = 'Po') then KitchenQual_Po = 1; else KitchenQual_Po = 0;

*	Heating Condition - Family of Indicator Variables;
	if (HeatingQC = 'Ex') then HeatingQC_Ex = 1; else HeatingQC_Ex = 0;
	if (HeatingQC = 'Gd') then HeatingQC_Gd = 1; else HeatingQC_Gd = 0;
	if (HeatingQC = 'TA') then HeatingQC_TA = 1; else HeatingQC_TA = 0;
	if (HeatingQC = 'Fa') then HeatingQC_Fa = 1; else HeatingQC_Fa = 0;
	if (HeatingQC = 'Po') then HeatingQC_Po = 1; else HeatingQC_Po = 0;

run; quit;

proc freq data = AHD_Sample;
	tables ScreenPorch ThreeSsnPorch EnclosedPorch OpenPorchSF;
run; quit;

**********************************************************************;
*	Neighborhood Indicator;
**********************************************************************;

*	Proc Freq for Neighborhood names (strings, so spelling and caps matter);
*	Can also be used as generic PROC FREQ to test values of variables;
proc freq data = AHD_Sample;
	tables Neighborhood;
run; quit;

data AHD_Sample;
	set AHD_Sample;
	
	if (Neighborhood = 'Blmngtn') then nb_Blmngtn = 1; else nb_Blmngtn = 0;
	if (Neighborhood = 'Blueste') then nb_Blueste = 1; else nb_Blueste = 0;
	if (Neighborhood = 'BrDale') then nb_BrDale = 1; else nb_BrDale = 0;
	if (Neighborhood = 'BrkSide') then nb_BrkSide = 1; else nb_BrkSide = 0;
	if (Neighborhood = 'ClearCr') then nb_ClearCr = 1; else nb_ClearCr = 0;
	if (Neighborhood = 'CollgCr') then nb_CollgCr = 1; else nb_CollgCr = 0;
	if (Neighborhood = 'Crawfor') then nb_Crawfor = 1; else nb_Crawfor = 0;
	if (Neighborhood = 'Edwards') then nb_Edwards = 1; else nb_Edwards = 0;
	if (Neighborhood = 'Gilbert') then nb_Gilbert = 1; else nb_Gilbert = 0;
	if (Neighborhood = 'Greens') then nb_Greens = 1; else nb_Greens = 0;
	if (Neighborhood = 'GrnHill') then nb_GrnHill = 1; else nb_GrnHill = 0;
	if (Neighborhood = 'IDOTRR') then nb_IDOTRR = 1; else nb_IDOTRR = 0;
	if (Neighborhood = 'Landmrk') then nb_Landmrk = 1; else nb_Landmrk = 0;
	if (Neighborhood = 'MeadowV') then nb_MeadowV = 1; else nb_MeadowV = 0;
	if (Neighborhood = 'Mitchel') then nb_Mitchel = 1; else nb_Mitchel = 0;
	if (Neighborhood = 'NAmes') then nb_NAmes = 1; else nb_NAmes = 0;
	if (Neighborhood = 'NoRidge') then nb_NoRidge = 1; else nb_NoRidge = 0;
	if (Neighborhood = 'NPkVill') then nb_NPkVill = 1; else nb_NPkVill = 0;
	if (Neighborhood = 'NridgHt') then nb_NridgHt = 1; else nb_NridgHt = 0;
	if (Neighborhood = 'NWAmes') then nb_NWAmes = 1; else nb_NWAmes = 0;
	if (Neighborhood = 'OldTown') then nb_OldTown = 1; else nb_OldTown = 0;
	if (Neighborhood = 'SWISU') then nb_SWISU = 1; else nb_SWISU = 0;
	if (Neighborhood = 'Sawyer') then nb_Sawyer = 1; else nb_Sawyer = 0;
	if (Neighborhood = 'SawyerW') then nb_SawyerW = 1; else nb_SawyerW = 0;
	if (Neighborhood = 'Somerst') then nb_Somerst = 1; else nb_Somerst = 0;
	if (Neighborhood = 'StoneBr') then nb_StoneBr = 1; else nb_StoneBr = 0;
	if (Neighborhood = 'Timber') then nb_Timber = 1; else nb_Timber = 0;
	if (Neighborhood = 'Veenker') then nb_Veenker = 1; else nb_Veenker = 0;

run; quit;

*	Quality Composite Indicies;
data AHD_Sample;
	set AHD_Sample;
	overallQual_index = OverallCond * OverallQual;
	sqftQual_index = GrLivArea * overallQual_index;
run; quit;

*	Should have 199 variables across 1939 observations;
*	Take a look at the first five;
proc print data = AHD_Sample(obs = 5);
run; quit;

**********************************************************************;
**********************************************************************;
*	Explore data for promising predictor variables;
*	PROC CORR to see correlation to SalePrice;
**********************************************************************;
**********************************************************************;

*	Examine continuous variables;
proc print data = AHD_Contents;
	where continuous = 1;
	var name nobs continuous discrete nominal ordinal;
run; quit;

*	PROC CORR for continuous variables;
*	Note: using created variable 'TotalFloorSF' to see how it compares
	to GrLivArea (answer: pretty close);
proc corr data = AHD_Sample;
	var SalePrice BsmtFinSF1 BsmtFinSF2 BsmtUnfSF EnclosedPorch
		FirstFlrSF GarageArea GrLivArea LotArea LotFrontage LowQualFinSF
		MasVnrArea MiscVal OpenPorchSF PoolArea ScreenPorch SecondFlrSF
		ThreeSsnPorch TotalBsmtSF TotalFloorSF WoodDeckSF;
run; quit;

**********************************************************************;
**********************************************************************;
*	Create Random Train/Test Split;
**********************************************************************;
**********************************************************************;

*	Actually going to create two sets here;
*	First (AHD_TT1) will be to manipulate as necessary;
*	Second (AHD_TT2) will be used to force a waterfall and test possible
	drop conditions (e.g. remove observations with specific variables);

*	First Set;
data AHD_TT1;
	set AHD_Sample;
	u = uniform(123);
	if (u < 0.70) then train = 1;
		else train = 0;
	if (u > 0.70) then test = 1;
		else test = 0;
	if (train = 1) then train_response = SalePrice;
		else train_response = .;
	if (test = 1) then test_response = SalePrice;
		else test_response = .;
	log_train_response = log(train_response);
	log_test_response = log(test_response);
run; quit;

*	Now we'll see how close we are to the 70/30 split;

*	Using PROC SQL;
proc sql;
	select sum(train)/count(train) as TT_Train
	from AHD_TT1;
run; quit;

proc sql;
	select sum(test)/count(test) as TT_Test
	from AHD_TT1;
run; quit;

*	Using PROC FREQ;
proc freq data = AHD_TT1;
	tables train test;
run; quit;

*	And a PROC CONTENTS to round it off and inform variable choices;
proc contents data = AHD_TT1;
run; quit;

/*
*	This set is to use a bastard-rigged waterfall and remove variables
	as necessary, e.g. specific neighborhoods or features (via indicators);
*	Want to apply the drop conditions FIRST, then do the split, otherwise 
	could distort which data are removed from train or test set (e.g. if many
	of a given drop appears in one set, then are removed, can have a negative
	effect - low probability but not worth the risk;
data AHD_TT2;
	set AHD_Sample;
	u = uniform(123);
	if (u < 0.70) then train = 1;
		else train = 0;
	if (u > 0.70) then test = 1;
		else test = 0;
	if (train = 1) then train_response = SalePrice;
		else train_response = .;
	if (test = 1) then test_response = SalePrice;
		else test_response = .;
	log_train_response = log(train_response);
	log_test_response = log(test_response);
run; quit;

*	Now we'll see how close we are to the 70/30 split;
proc sql;
	select sum(train)/count(train) as TT_Train
	from AHD_TT1;
run; quit;
*	So, we're at 68.28%, pretty close;

proc sql;
	select sum(test)/count(test) as TT_Test
	from AHD_TT1;
run; quit;

*	And a PROC CONTENTS to round it off and inform variable choices;
proc contents data = AHD_TT2;
run; quit;
*/

**********************************************************************;
**********************************************************************;
*	Model Identification by Automated Variable Selection
*	and Predictive Accuracy;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	NO AVS!! Playing with regression;
**********************************************************************;

*	Goal is to stuff the model like a woodchipper, understand WHY and
	HOW overfit happens;
*	Do it with both SalePrice and logSalePrice overall, in train,
	and in test;

*	SalePrice;
proc reg data = AHD_TT1;
	model SalePrice = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Reg predicted = yhat residual = res_a;
run; quit;

*	logSalePrice;
proc reg data = AHD_TT1;
	model logSalePrice = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Log predicted = yhat residual = res_a;
run; quit;

*	Now repeat on train;

*	train_response;
proc reg data = AHD_TT1;
	model train_response = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Reg_Train predicted = yhat residual = res_a;
run; quit;

*	log_train_response;
proc reg data = AHD_TT1;
	model log_train_response = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Log_Train predicted = yhat residual = res_a;
run; quit;

*	Now repeat on test;

*	test_response;
proc reg data = AHD_TT1;
	model test_response = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Reg_Test predicted = yhat residual = res_a;
run; quit;

*	log_test_response;
proc reg data = AHD_TT1;
	model log_test_response = 
/* Normal variables */
	GarageArea GrLivArea MasVnrArea OpenPorchSF OverallCond 
	OverallQual TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index;
	output out = TT1_Log_Test predicted = yhat residual = res_a;
run; quit;

*	Now compare MAE and MSE;
*	For this example, using log response variable transformation(s);
*	Will create frame of reference for evaluating AVS selection;
*	This will also verify calculations of MAE and MSE from SAS output
	versus user-created variables;

data TT1_Log_Train_Res;
	set TT1_Log_Train;
	res_c = (log_train_response - yhat);
	where res is not missing;
	abs_res_c = abs(res_c);
	square_res_c = (res_c**2);
	abs_res_a = abs(res_a);
	square_res_a = (res_a**2);
run; quit;

data TT1_Log_Test_Res;
	set TT1_Log_Test;
	res_c = (log_test_response - yhat);
	abs_res_c = abs(res_c);
	square_res_c = (res_c**2);
	abs_res_a = abs(res_a);
	square_res_a = (res_a**2);
run; quit;

proc means data = TT1_Log_Train_Res mean nway nmiss;
	class train;
	var abs_res_c square_res_c abs_res_a square_res_a;
	output out = EM_Log_Train
	mean(abs_res_c) = MAE_c
	mean(square_res_c) = MSE_c
	mean(abs_res_a) = MAE_a
	mean(square_res_a) = MSE_a;
run; quit;

proc means data = TT1_Log_Test_Res mean nway nmiss;
	class test;
	var abs_res_c square_res_c abs_res_a square_res_a;
	output out = EM_Log_Test
	mean(abs_res_c) = MAE_c
	mean(square_res_c) = MSE_c
	mean(abs_res_a) = MAE_a
	mean(square_res_a) = MSE_a;
run; quit;

proc print data = EM_Log_Train;
run; quit;

proc print data = EM_Log_Test;
run; quit;

*	Ok, what are our results?;

*	With sqftQual_index;
*	MAE train 	= 0.073201
*	MAE test 	= 0.073446
*		Percent change = 0.3347%;
*	MSE train	= 0.009568708
*	MSE test 	= 0.009615054
*		Percent change = 0.00%;

*	Without sqftQual_index;
*	MAE train 	= 0.073321
*	MAE test 	= 0.073445
*		Percent change = 0.1691%;
*	MSE train	= 0.009575421
*	MSE test 	= 0.009615055
*		Percent change = 0.00%;

*	That gives us a frame of reference to compare subsequent AVS
	methods and models;

**********************************************************************;
*	Using AVS to find "Best" Models;
**********************************************************************;

*	Model_AdjR2;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = adjrsq best = 5;
run; quit;

*	Model_MaxR;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = maxr;
run; quit;

*	Model_MCp;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = cp best = 5;
run; quit;

*	Model_F;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = forward slentry = 0.11 include = 3;
run; quit;

*	Model_B;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = backward slstay = 0.11 include = 3;
run; quit;

*	Model_S;
proc reg data = AHD_TT1;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Normal variables */
	GarageArea MasVnrArea OpenPorchSF TotRmsAbvGrd
	TotalBsmtSF TotalFloorSF YearBuilt
/* Created Variables */
	central_air lot_corner lot_culdsac lot_frontage
	regular_lot total_baths_calc
/* Family Predictor Variables */
	BsmtCond_Gd BsmtCond_TA BsmtCond_Fa
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa
	ExterCond_Ex ExterCond_Gd ExterCond_TA
	ExterQual_Ex ExterQual_Gd ExterQual_TA
	FireplaceQu_Ex FireplaceQu_Gd FireplaceQu_TA
	GarageCond_Gd GarageCond_TA GarageCond_Fa 
	GarageQual_Gd GarageQual_TA GarageQual_Fa 
	HeatingQC_Ex HeatingQC_Gd HeatingQC_TA
	KitchenQual_Ex KitchenQual_Gd KitchenQual_TA
/* Indicators */
	electrical_ind fence_ind fireplaceMany_ind fireplace_ind
	functional_ind garageLg_ind garageMult_ind garage_ind
	kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind
/* Indicies */
	overallQual_index sqftQual_index
/* Options */
	/ selection = stepwise slentry = 0.11 slstay = 0.11 include = 3;
run; quit;

**********************************************************************;
*	Evaluation using selected variables;
**********************************************************************;

*	For each of the six, compute the adjusted R-squared, AIC, BIC,
	MSE, MAE in the training sample;
*	For each of the six, compute MSE and MAE in the test sample;

***********************************;
*	Model_AdjR2;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = AdjR2_Train_Sum;
	model train_response = 
/*	Variables */
	GrLivArea OverallCond OverallQual GarageArea MasVnrArea OpenPorchSF
	TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt central_air 
	lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd BsmtQual_TA 
	BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd FireplaceQu_Gd 
	GarageCond_Fa GarageQual_Gd GarageQual_TA HeatingQC_Ex HeatingQC_Gd 
	HeatingQC_TA KitchenQual_Ex KitchenQual_TA fireplace_ind functional_ind
	garageMult_ind kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 38 stop = 38 mse adjrsq aic bic cp vif;
	output out = AdjR2_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = AdjR2_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data AdjR2_Train_Res;
	set AdjR2_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = AdjR2_Train_Res mean nway nmiss;
	class train;
	var abs_res square_res;
	output out = EM_AdjR2_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_AdjR2_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = AdjR2_Test_Sum;
	model test_response = 
	GrLivArea OverallCond OverallQual GarageArea MasVnrArea OpenPorchSF
	TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt central_air 
	lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd BsmtQual_TA 
	BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd FireplaceQu_Gd 
	GarageCond_Fa GarageQual_Gd GarageQual_TA HeatingQC_Ex HeatingQC_Gd 
	HeatingQC_TA KitchenQual_Ex KitchenQual_TA fireplace_ind functional_ind
	garageMult_ind kitchenMult_ind landcontourLvl_ind landslopeGtl_ind
	pool_ind overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 38 stop = 38 mse adjrsq aic bic cp vif;
	output out = AdjR2_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = AdjR2_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data AdjR2_Test_Res;
	set AdjR2_Test;
	res = (test_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = AdjR2_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_AdjR2_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_AdjR2_Test;
run; quit;

***********************************;
*	Model_MaxR;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = MaxR_Train_Sum;
	model train_response = 
	OverallCond OverallQual GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF
	YearBuilt central_air lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd
	BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd 
	FireplaceQu_Gd GarageCond_Fa HeatingQC_TA KitchenQual_Ex KitchenQual_TA 
	fireplace_ind functional_ind landcontourLvl_ind landslopeGtl_ind 
	overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 28 stop = 28 mse adjrsq aic bic cp vif;
	output out = MaxR_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = MaxR_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data MaxR_Train_Res;
	set MaxR_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = MaxR_Train_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = EM_MaxR_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_MaxR_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = MaxR_Test_Sum;
	model test_response = 
	OverallCond OverallQual GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF
	YearBuilt central_air lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd
	BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd 
	FireplaceQu_Gd GarageCond_Fa HeatingQC_TA KitchenQual_Ex KitchenQual_TA 
	fireplace_ind functional_ind landcontourLvl_ind landslopeGtl_ind 
	overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 28 stop = 28 mse adjrsq aic bic cp vif;
	output out = MaxR_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = MaxR_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data MaxR_Test_Res;
	set MaxR_Test;
	res = (test_response - yhat);
	where res is not missing;	
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = MaxR_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_MaxR_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_MaxR_Test;
run; quit;

***********************************;
*	Model_MCp;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = MCp_Train_Sum;
	model train_response = 
	OverallCond OverallQual GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF
	TotalFloorSF YearBuilt lot_culdsac lot_frontage total_baths_calc 
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd
	FireplaceQu_Gd GarageCond_Fa HeatingQC_Ex HeatingQC_Gd HeatingQC_TA 
	KitchenQual_Ex fireplace_ind functional_ind kitchenMult_ind 
	landcontourLvl_ind landslopeGtl_ind overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 30 stop = 30 mse adjrsq aic bic cp vif;
	output out = MCp_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = MCp_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data MCp_Train_Res;
	set MCp_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = MCp_Train_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = EM_MCp_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_MCp_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = MCp_Test_Sum;
	model test_response = 
	OverallCond OverallQual GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF
	TotalFloorSF YearBuilt lot_culdsac lot_frontage total_baths_calc 
	BsmtQual_Gd BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd
	FireplaceQu_Gd GarageCond_Fa HeatingQC_Ex HeatingQC_Gd HeatingQC_TA 
	KitchenQual_Ex fireplace_ind functional_ind kitchenMult_ind 
	landcontourLvl_ind landslopeGtl_ind overallQual_index sqftQual_index
/*	Options */
	/ selection = rsquare start = 30 stop = 30 mse adjrsq aic bic cp vif;
	output out = MCp_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = MCp_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data MCp_Test_Res;
	set MCp_Test;
	res = (test_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = MCp_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_MCp_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_MCp_Test;
run; quit;

***********************************;
*	Model_F;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = F_Train_Sum;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	TotalBsmtSF YearBuilt ExterQual_Ex sqftQual_index overallQual_index
	landslopeGtl_ind GarageArea MasVnrArea total_baths_calc KitchenQual_Ex
	landcontourLvl_ind FireplaceQu_Gd BsmtQual_TA BsmtQual_Gd BsmtQual_Fa
	lot_culdsac TotRmsAbvGrd ExterQual_Gd fireplace_ind functional_ind
	central_air KitchenQual_TA TotalFloorSF lot_frontage HeatingQC_TA
	ExterCond_Ex
/*	Options */
	/ selection = rsquare start = 29 stop = 29 mse adjrsq aic bic cp vif;
	output out = F_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = F_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data F_Train_Res;
	set F_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = F_Train_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = EM_F_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_F_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = F_Test_Sum;
	model test_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	TotalBsmtSF YearBuilt ExterQual_Ex sqftQual_index overallQual_index
	landslopeGtl_ind GarageArea MasVnrArea total_baths_calc KitchenQual_Ex
	landcontourLvl_ind FireplaceQu_Gd BsmtQual_TA BsmtQual_Gd BsmtQual_Fa
	lot_culdsac TotRmsAbvGrd ExterQual_Gd fireplace_ind functional_ind
	central_air KitchenQual_TA TotalFloorSF lot_frontage HeatingQC_TA
	ExterCond_Ex
/*	Options */
	/ selection = rsquare start = 29 stop = 29 mse adjrsq aic bic cp vif;
	output out = F_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = F_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data F_Test_Res;
	set F_Test;
	res = (test_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = F_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_F_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_F_Test;
run; quit;

***********************************;
*	Model_B;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = B_Train_Sum;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
	central_air lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd
	BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd FireplaceQu_Gd
	HeatingQC_Ex HeatingQC_Gd KitchenQual_Ex KitchenQual_TA fireplace_ind
	functional_ind landcontourLvl_ind landslopeGtl_ind overallQual_index
	sqftQual_index
/*	Options */
	/ selection = rsquare start = 30 stop = 30 mse adjrsq aic bic cp vif;
	output out = B_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = B_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data B_Train_Res;
	set B_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = B_Train_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = EM_B_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_B_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = B_Test_Sum;
	model test_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	GarageArea MasVnrArea TotRmsAbvGrd TotalBsmtSF TotalFloorSF YearBuilt
	central_air lot_culdsac lot_frontage total_baths_calc BsmtQual_Gd
	BsmtQual_TA BsmtQual_Fa ExterCond_Ex ExterQual_Ex ExterQual_Gd FireplaceQu_Gd
	HeatingQC_Ex HeatingQC_Gd KitchenQual_Ex KitchenQual_TA fireplace_ind
	functional_ind landcontourLvl_ind landslopeGtl_ind overallQual_index
	sqftQual_index
/*	Options */
	/ selection = rsquare start = 30 stop = 30 mse adjrsq aic bic cp vif;
	output out = B_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = B_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data B_Test_Res;
	set B_Test;
	res = (test_response - yhat);
	where res is not missing;	
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = B_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_B_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_B_Test;
run; quit;

***********************************;
*	Model_S;
***********************************;
*	TRAIN MODEL;
proc reg data = AHD_TT1
	outest = S_Train_Sum;
	model train_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	TotalBsmtSF YearBuilt ExterQual_Ex sqftQual_index overallQual_index
	landslopeGtl_ind GarageArea MasVnrArea total_baths_calc KitchenQual_Ex
	landcontourLvl_ind FireplaceQu_Gd BsmtQual_TA BsmtQual_Gd BsmtQual_Fa
	lot_culdsac TotRmsAbvGrd ExterQual_Gd fireplace_ind functional_ind
	central_air KitchenQual_TA TotalFloorSF lot_frontage HeatingQC_TA
	ExterCond_Ex
/*	Options */
	/ selection = rsquare start = 29 stop = 29 mse adjrsq aic bic cp vif;
	output out = S_Train predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = S_Train_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data S_Train_Res;
	set S_Train;
	res = (train_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = S_Train_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = EM_S_Train
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_S_Train;
run; quit;

*	TEST MODEL;
proc reg data = AHD_TT1
	outest = S_Test_Sum;
	model test_response = 
/* Forced Variables */
	GrLivArea OverallCond OverallQual
/* Selected Variables */
	TotalBsmtSF YearBuilt ExterQual_Ex sqftQual_index overallQual_index
	landslopeGtl_ind GarageArea MasVnrArea total_baths_calc KitchenQual_Ex
	landcontourLvl_ind FireplaceQu_Gd BsmtQual_TA BsmtQual_Gd BsmtQual_Fa
	lot_culdsac TotRmsAbvGrd ExterQual_Gd fireplace_ind functional_ind
	central_air KitchenQual_TA TotalFloorSF lot_frontage HeatingQC_TA
	ExterCond_Ex
/*	Options */
	/ selection = rsquare start = 29 stop = 29 mse adjrsq aic bic cp vif;
	output out = S_Test predicted = yhat residual = res;
run; quit;

*	Another way to get an output table;
proc print data = S_Test_Sum;
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;

*	Still need to calculate MAE, might as well to MSE to verify match;
data S_Test_Res;
	set S_Test;
	res = (test_response - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	MSE does not match, likely due to rounding (slightly off, then squaring it);
proc means data = S_Test_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = EM_S_Test
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = EM_S_Test;
run; quit;

**********************************************************************;
*	Operational Validation;
**********************************************************************;

*	Note: THERE ARE NINE MISSING VARIABLES
	4 in TRAINING
	5 IN TEST;

*	Create Format;
proc format;
	value $Prediction_Grade (default = 7)
	. = 'Missing'
	0.0 - 0.10 = 'Grade 1'
	0.10 <- 0.15 = 'Grade 2'
	0.15 <- high = 'Grade 3'
	;
run;

*	Model_AdjR2;
data AdjR2_OV;
	set AdjR2_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = AdjR2_OV;
	tables Prediction_Grade;
run; quit;

*	Model_MaxR;
data MaxR_OV;
	set MaxR_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = MaxR_OV;
	tables Prediction_Grade;
run; quit;

*	Model_MCp;
data MCp_OV;
	set MCp_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = MCp_OV;
	tables Prediction_Grade;
run; quit;

*	Model_F;
data F_OV;
	set F_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = F_OV;
	tables Prediction_Grade;
run; quit;

*	Model_B;
data B_OV;
	set B_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = B_OV;
	tables Prediction_Grade;
run; quit;

*	Model_S;
data S_OV;
	set S_Test;
	OV = abs(((yhat-test_response)/test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = S_OV;
	tables Prediction_Grade;
run; quit;

**********************************************************************;
*	Operational Validation;
*	FOR FUN TO COMPARE TO ORIGINAL WOODCHIPPER TEST MODEL;
**********************************************************************;

proc format;
	value $Prediction_Grade (default = 7)
	. = 'Missing'
	0.0 - 0.05 = 'Grade 0'
	0.05 <- 0.10 = 'Grade 1'
	0.10 <- 0.15 = 'Grade 2'
	0.15 <- high = 'Grade 3'
	;
run;

data TT1_Log_OV;
	set TT1_Log_Test;
	OV = abs(((yhat-log_test_response)/log_test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = TT1_Log_OV;
	tables Prediction_Grade;
run; quit;

*	HOT DAMN NOT BAD;

**********************************************************************;
*	FIN;
**********************************************************************;
