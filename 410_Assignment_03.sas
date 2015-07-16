**********************************************************************;
*	410-DL | Assignment 03;
*	Last updated: 2015-07-12 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data AHD;
	set mydata.ames_housing_data;
run; quit;

*	Take a look at the head of the data - first ten observations;
proc print data =  AHD (obs = 10);
run; quit;

**********************************************************************;
*	SAS Macros;
**********************************************************************;

*	Macro for scatterplots;
%macro AHD_Scatter(varname);
	ods graphics on;
	proc sgscatter data = AHD_Sample;
		compare x = &varname. y = SalePrice / loess reg;
		title "Scatter Plot of SalePrice by &varname.";
		title2 "with LOESS smoother";
	run; quit;
	ods graphics off;
%mend;

*	Macro for histograms;
%macro AHD_Histogram(varname);
	proc sgplot data = AHD_Sample;
		histogram &varname. / transparency = 0.5;
		density &varname. / type = normal;
		density &varname. / type = kernel;
		title "Histogram of &varname.";
		title2 "with normal and kernel density estimates";
	run; quit;
%mend;

*	Macro for boxplots;
%macro AHD_Boxplot(varname);
	proc sgplot data = AHD_Sample;
		vbox &varname.;
		title "Boxplot of &varname.";
	run; quit;
%mend;

**********************************************************************;
*	PROC CONTENTS;
*	Goal here is to save a copy of PROC CONTENTS of the AHD, then
	add in flags for each variable type based on the data dictionary;
*	We do this AFTER we define our sample population, because we're
	creating new variables. Otherwise, we'd have to do this twice;
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

proc print data = AHD_Contents;
	var name nobs continuous discrete nominal ordinal;
run; quit;

**********************************************************************;
*	Define the Sample Population through an initial waterfall;
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
*	Explore missing values for possible subsequent comparative analysis;
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
run;

proc freq data = AHD_Sample_NoMiss;
	format _CHAR_ $missfmt.;
	tables _CHAR_ / missing missprint nocum nopercent;
	format _NUMERIC_ missfmt.;
	tables _NUMERIC_ / missing missprint nocum nopercent;
run;

**********************************************************************;
*	Create new variables and indicator variables as needed ;
**********************************************************************;

*	Create new variables where appropriate (for totals or otherwise);
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

*	Create indicator variables where appropriate;
data AHD_Sample;
	set AHD_Sample;
	
*	Central Air Indicator;
	if (CentralAir = 'Y') then central_air = 1; else central_air = 0;

*	Fireplace Indicator;
	if (Fireplaces > 0) then fireplace_ind = 1; else fireplace_ind = 0;

*	Garage Indicator;
	if (GarageCars > 0) then garage_ind = 1; else garage_ind = 0;

*	Good Basement Indicator;
	if (BsmtQual in ('Ex', 'Gd')) or (BsmtCond in ('Ex', 'Gd'))
		then good_basement_ind = 1; else good_basement_ind = 0;

*	Exterior Material Quality - Family of Indicator Variables;
	if (ExterQual='Ex') then ExterQual_Ex = 1; else ExterQual_Ex = 0;
	if (ExterQual='Gd') then ExterQual_Gd = 1; else ExterQual_Gd = 0;
	if (ExterQual='TA') then ExterQual_TA = 1; else ExterQual_TA = 0;
	if (ExterQual='Fa') then ExterQual_Fa = 1; else ExterQual_Fa = 0;
	if (ExterQual='Po') then ExterQual_Po = 1; else ExterQual_Po = 0;

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
	
*	Construct a composite quality index;
	quality_index = OverallCond * OverallQual;
	
run; quit;

**********************************************************************;
*	Explore data for promising predictor variables;
*	PROC CORR to see correlation to SalePrice;
**********************************************************************;

*	Examine continuous variables;
proc print data = AHD_Contents;
	where continuous = 1;
	var name nobs continuous discrete nominal ordinal;
run; quit;

*	PROC CORR;
*	Note: using created variable 'TotalFloorSF' to see how it compares
	to GrLivArea (answer: pretty close);
proc corr data = AHD_Sample;
	var SalePrice BsmtFinSF1 BsmtFinSF2 BsmtUnfSF EnclosedPorch
		FirstFlrSF GarageArea GrLivArea LotArea LotFrontage LowQualFinSF
		MasVnrArea MiscVal OpenPorchSF PoolArea ScreenPorch SecondFlrSF
		ThreeSsnPorch TotalBsmtSF TotalFloorSF WoodDeckSF;
run; quit;

*	Post-corr candidates include: FirstFlrSF, GarageArea, GrLivArea,
	MasVnrArea, TotalBsmtSF, and TotalFloorSF;

*	FirstFlrSF;
%AHD_Scatter(FirstFlrSF);
%AHD_Histogram(FirstFlrSF);
%AHD_Boxplot(FirstFlrSF);

*	GarageArea;
%AHD_Scatter(GarageArea);
%AHD_Histogram(GarageArea);
%AHD_Boxplot(GarageArea);

*	GrLivArea;
%AHD_Scatter(GrLivArea);
%AHD_Histogram(GrLivArea);
%AHD_Boxplot(GrLivArea);

*	MasVnrArea;
%AHD_Scatter(MasVnrArea);
%AHD_Histogram(MasVnrArea);
%AHD_Boxplot(MasVnrArea);

*	TotalFloorSF;
%AHD_Scatter(TotalFloorSF);
%AHD_Histogram(TotalFloorSF);
%AHD_Boxplot(TotalFloorSF);

*	TotalBsmtSF;
%AHD_Scatter(TotalBsmtSF);
%AHD_Histogram(TotalBsmtSF);
%AHD_Boxplot(TotalBsmtSF);

*	Of those which look good? 
		FirstFlrSF, GrLivArea, and TotalBsmtSF;
*	Picking GrLivArea;
*	Between FirstFlrSF & TotalBsmtSF, picking TotalBsmtSF
		Although FirstFlrSF looks slightly better, it likely is part
		of GrLivArea (think about it). And while a 'typical' home likely
		has a first floor but may have a basement, want to be careful
		with variable selection. Don't mix parts and wholes. Remember
		GrLivArea is above ground living area, so basement space not
		included.;
		
**********************************************************************;
*	Simple Linear Regression Models;
*	No transformation on SalePrice
**********************************************************************;

*	Two simple linear regression models, each using a promising
	continuous variable;

*	Simple Linear Regression Model #1;
proc reg data = AHD_Sample 
	outest = SLR1
	plots = (residuals(smooth));
	SLR1: model SalePrice = GrLivArea / alpha = 0.05 clb vif;
	title 'Simple Linear Regression Model 1';
run; quit;

proc print data = SLR1;
run; quit;

*	Simple Linear Regression Model #2;
proc reg data = AHD_Sample
	plots = (residuals(smooth));
	model SalePrice = TotalBsmtSF / alpha = 0.05 clb;
run; quit;

*	Simple Linear Regression Model #2;
proc reg data = AHD_Sample
	plots = (residuals(smooth));
	model SalePrice = TotalBsmtSF / alpha = 0.05 clb;
run; quit;

**********************************************************************;
*	Multiple Linear Regression Model;
*	No transformation on SalePrice
**********************************************************************;

*	Multiple Linear Regression Model;
*	Output results to AHD_MLR to prep for outlier identification;
proc reg data = AHD_Sample
	plots = (residuals(smooth));
	model SalePrice = GrLivArea TotalBsmtSF / alpha = 0.05 stb clb;
	output out = AHD_MLR cookd = cookd h = leverage student = residual
run; quit;

*	Examine correlations among variables in the multiple model;
proc corr data = AHD_Sample;
	var SalePrice GrLivArea TotalBsmtSF;
run; quit;

**********************************************************************;
*	Outliers Identification;
**********************************************************************;

*	Let's take a closer look at outliers from the MLR model;
*	Does anything stand out as a candidate for outliers?;

data AHD_Test;
	set AHD_MLR;
	residual_sq = residual*residual;
	cook_lev = cookd*leverage;
run; quit;

*	First scatterplot: examine leverage versus squared residuals;
proc sgplot data = AHD_Test;
	scatter y = leverage x = residual_sq / datalabel = SID;
run; quit;

*	Second scatterplot: examine Cook's D versus squared residuals;
proc sgplot data = AHD_Test;
	scatter y = cookd x = residual_sq / datalabel = SID;
run; quit;

*	From our scatterplots, observations with these properities look
	to be good candidates for outliers. What properties (if any) do
	they have in common?;
data AHD_Test_Narrow;
	set AHD_Test;
	where ((cookd > 4/1939) or (leverage > 0.015) or (residual_sq > 30));
run; quit;

*	Sort data by (Cook's D * Leverage) score descending;
proc sort data = AHD_Test_Narrow;
	by descending cook_lev;
run; quit;

*	Location, location, location? 
	What about a PROC FREQ by neighborhood?
	NridgHt looks pretty hiflautin, but what does that mean?;
proc freq data = AHD_Test_Narrow;
	tables Neighborhood;
run; quit;

*	Turn back to our sample data;

*	Take a closer look at Fireplaces;
%AHD_Histogram(Fireplaces);
%AHD_Boxplot(Fireplaces);

proc univariate data = AHD_Sample;
	var fireplaces;
run; quit;

*	Take a closer look at GarageCars;
%AHD_Histogram(GarageCars);
%AHD_Boxplot(GarageCars);

proc univariate data = AHD_Sample;
	var GarageCars;
run; quit;

*	Take a closer look at MiscFeature;
proc freq data = AHD_Sample;
	tables MiscFeature;
run; quit;

*	Take a closer look at PoolArea;
%AHD_Histogram(PoolArea);
%AHD_Boxplot(PoolArea);

proc univariate data = AHD_Sample;
	var PoolArea;
run; quit;

*	Take a closer look at GrLivArea;
%AHD_Histogram(GrLivArea);
%AHD_Boxplot(GrLivArea);

proc univariate data = AHD_Sample;
	var GrLivArea;
run; quit;

proc print data = AHD_Sample;
	where GrLivArea > 2840;
run; quit;

*	Take a closer look at TotalBsmtSF;
%AHD_Histogram(TotalBsmtSF);
%AHD_Boxplot(TotalBsmtSF);

proc univariate data = AHD_Sample;
	var TotalBsmtSF;
run; quit;

proc print data = AHD_Sample;
	where TotalBsmtSF > 2153;
run; quit;

*	Take a closer look at TotRmsAbvGrd;
%AHD_Histogram(TotRmsAbvGrd);
%AHD_Boxplot(TotRmsAbvGrd);

proc univariate data = AHD_Sample;
	var TotRmsAbvGrd;
run; quit;

proc print data = AHD_Sample;
	where TotRmsAbvGrd >= 10;
run; quit;

*	Look at candidate outliers in AHD_Sample in totality;
data AHD_Test_Outliers;
	set AHD_Sample;
	where ((Fireplaces >= 3) or (GarageCars > 3)
		or (MiscFeature = 'Gar2') or (PoolArea > 0)
		or (GrLivArea > 2840) or (TotalBsmtSF > 2153)
		or (TotRmsAbvGrd >= 10));
run; quit;

proc sort data = AHD_Test_Outliers;
	by descending SalePrice;
run; quit;

proc print data = AHD_Test_Outliers;
run; quit;

proc freq data = AHD_Sample;
	tables Fireplaces GarageCars MiscFeature PoolArea TotRmsAbvGrd;
run; quit;

*	Outliers Waterfall;
data AHD_Outliers;
	set AHD_Sample;
	format outlier_def $45.;
	if (Fireplaces >= 3) then do;
		outlier_def = '01: GT 2 Fireplaces';
		outlier_code = 1;
	end;
	else if (GarageCars > 3) then do;
		outlier_def = '02: GT 3-Car Garage';
		outlier_code = 2;
	end;
	else if (MiscFeature = 'Gar2') then do;
		outlier_def = '03: Second Garage';
		outlier_code = 3;
	end;
	else if (PoolArea > 0) then do;
		outlier_def = '04: Has Pool';
		outlier_code = 4;
	end;
	else if (GrLivArea > 2840) then do;
		outlier_def = '05: GrLivArea > 2840';
		outlier_code = 5;
	end;
	else if (TotalBsmtSF > 2153) then do;
		outlier_def = '06: TotalBsmtSF > 2153';
		outlier_code = 6;
	end;
	else if (TotRmsAbvGrd >= 10) then do;
		outlier_def = '07: TotRmsAbvGrd >= 10';
		outlier_code = 7;
	end;
	else do;
		outlier_def = '08: Not An Outlier';
		outlier_code = 8;
	end;
run; quit;

proc freq data = AHD_Outliers;
	tables outlier_def;
run; quit;

**********************************************************************;
*	Multiple Linear Regression Model;
*	Refit without Outlier Observations
**********************************************************************;

*	Multiple Linear Regression Model;
*	No outliers;
proc reg data = AHD_Outliers 
	plots(unpack) plots = residuals(smooth);
	model SalePrice = GrLivArea TotalBsmtSF / alpha = 0.05 stb clb;
	where (outlier_code = 8);
run; quit;

*	Examine correlations among variables in the multiple model;
proc corr data = AHD_Outliers;
	var SalePrice GrLivArea TotalBsmtSF;
run; quit;

*	Multiple Linear Regression Model;
*	Including outliers;
proc reg data = AHD_Outliers 
	plots(unpack) plots = residuals(smooth);
	model SalePrice = GrLivArea TotalBsmtSF / alpha = 0.05 stb clb;
run; quit;

*	Examine correlations among variables in the multiple model;
proc corr data = AHD_Outliers;
	var SalePrice GrLivArea TotalBsmtSF;
run; quit;

**********************************************************************;
*	Model Comparison of SalePrice & logSalePrice;
**********************************************************************;

*	Multiple Linear Regression Model;
*	No transformation on SalePrice;
proc reg data = AHD_Sample
	plots(unpack) plots = residuals(smooth);
	model SalePrice = GarageArea MasVnrArea TotalBsmtSF TotalFloorSF
	/ alpha = 0.05 stb clb;
run; quit;

*	Examine correlations among variables in the multiple model;
proc corr data = AHD_Sample;
	var SalePrice GarageArea MasVnrArea TotalBsmtSF TotalFloorSF;
run; quit;

*	Multiple Linear Regression Model;
*	Natural log transformation on SalePrice;
proc reg data = AHD_Sample
	plots(unpack) plots = residuals(smooth);
	model logSalePrice = GarageArea MasVnrArea TotalBsmtSF TotalFloorSF
	/ alpha = 0.05 stb clb;
run; quit;

*	Examine correlations among variables in the multiple model;
proc corr data = AHD_Outliers;
	var logSalePrice GarageArea MasVnrArea TotalBsmtSF TotalFloorSF;
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;
