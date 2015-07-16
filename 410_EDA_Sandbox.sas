**********************************************************************;
*	410-DL | EDA for Continuous Variables;
*	Last updated: 2015-07-13 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data AHD;
	set mydata.ames_housing_data;
run; quit;

**********************************************************************;
*	SAS Macros;
**********************************************************************;

*	Macro for scatterplots;
%macro AHD_Scatter(varname);
	ods graphics on;
	proc sgscatter data = AHD;
		compare x = &varname. y = SalePrice / loess reg;
		title "Scatter Plot of SalePrice by &varname.";
		title2 "with LOESS smoother";
	run; quit;
	ods graphics off;
%mend;

*	Macro for histograms;
%macro AHD_Histogram(varname);
	proc sgplot data = AHD;
		histogram &varname. / transparency = 0.5;
		density &varname. / type = normal;
		density &varname. / type = kernel;
		title "Histogram of &varname.";
		title2 "with normal and kernel density estimates";
	run; quit;
%mend;

*	Macro for boxplots;
%macro AHD_Boxplot(varname);
	proc sgplot data = AHD;
		vbox &varname.;
		title "Boxplot of &varname.";
	run; quit;
%mend;

**********************************************************************;
*	PROC CONTENTS;
*	Goal here is to save a copy of PROC CONTENTS of the AHD, then
	add in flags for each variable type based on the data dictionary;
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
		'WoodDeckSF')) 
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

proc print data = AHD_Contents;
	var name nobs continuous discrete nominal ordinal;
run; quit;

**********************************************************************;
*	EDA on each continuous variable;
**********************************************************************;

data _null_;
	do i = 1 to num;
		set AHD_Contents nobs = num;
			if name = 'SalePrice' then delete;
			where continuous = 1;
			call execute('%AHD_Scatter('||name||')');
			call execute('%AHD_Histogram('||name||')');
			call execute('%AHD_Boxplot('||name||')');
	end;
run; quit;

proc univariate data = AHD
	outtable AHD_Univariate noprint;
	do i = to num
		set AHD_Contents nobs = num;
			if name = 'SalePrice' then delete;
			where continuous = 1;
			proc univariate data = AHD
				outtable = AHD_Univariate noprint;
	end;
run; quit;


proc univariate data = AHD
	outtable = AHD_Univariate noprint;
run; quit;

proc print data = AHD_Univariate;
run; quit;







