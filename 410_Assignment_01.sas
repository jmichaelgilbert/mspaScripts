**********************************************************************;
*	410-DL | Assignment 01;
*	Last updated: 2015-06-28 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data AHD;
	set mydata.ames_housing_data;
run; quit;

*	List out the column names and data types for the data set;
proc contents data = AHD;
run; quit;

*	Take a look at the head of the data - first ten observations;
proc print data =  AHD (obs = 10);
run; quit;

**********************************************************************;
*	Data Survey;
**********************************************************************;

*	Survey data to understand frequencies and appearances,
	Will be used in waterfall as part of defining 'Typical';
*	First be SURE to consult data dictionary for variable types,
	possible values, and so on.;

*	Take a look at GrLivArea, Above Grade Living Area = what is 'typical'?;
proc univariate data = AHD;
	var GrLivArea;
run; quit;

*	Take a look at TotRmsAbvGrd - how many are 'normal'?;
proc univariate data = AHD;
	var TotRmsAbvGrd;
run; quit;

*	Take a look at LotShape - just how irregular is irregular?;
proc freq data = AHD;
	tables LotShape;
run; quit;

*	Data dictionary suggests plotting SalePrice vs. GrLivArea.;
*	Playing with scatter plot options and LOESS smoother.;
ods graphics on;
proc sgscatter data = AHD;
	title "Scatter Plot of Sale Price by Above Grade Living Area";
	compare x = GrLivArea y = SalePrice / loess reg;
run; quit;
ods graphics off;

**********************************************************************;
*	Define the Sample Population through an initial waterfall;
**********************************************************************;

*	Begin waterfall;
data AHD_WF;
	set AHD;
	*	Create a new variable, drop_condition to track drops;
	format drop_condition $45.;
	if (Functional ne 'Typ') then drop_condition = '01: Non-Typical Home Functionality';	
	else if ((Zoning ne 'RH') and (Zoning ne 'RL') and (Zoning ne 'RP') 
		and (Zoning ne 'RM')) then drop_condition = '02: Non-Residential Zoning';
	else if ((OverallCond = 1) or (OverallCond = 2) or (OverallCond = 3)
		or (OverallCond = 4)) then drop_condition = '03: LT Average Condition';
	else if ((OverallQual = 1) or (OverallQual = 2) or (OverallQual = 3)
		or (OverallQual = 4)) then drop_condition = '04: LT Average Quality';
	else if (YearBuilt < 1946) then drop_condition = '05: Built 1945 & Older';
	else if ((LotShape eq 'IR2') or (LotShape eq 'IR3')) then
		drop_condition = '06: Irregular Lot Shape (2 or 3)';	
	else if (TotRmsAbvGrd < 5) then drop_condition = '07: LT 5 Rooms Above Grade';
	else if (GrLivArea < 800) then drop_condition = '08: LT 800 SQFT';
	else if (GrLivArea > 4000) then drop_condition = '09: GT 4000 SQFT';
	else drop_condition = '10: Sample Population';
run;
*	Note: Appears that SAS wants you to "pair" NE with AND, EQ with OR
	on multiple conditions in single if / else-if statement.
*	Note: GT 4000 SQFT included as referenced in Data Dictionary.;

*	View table with observations remaining;
proc freq data = AHD_WF;
	tables drop_condition;
	title 'Sample Waterfall';
run; quit;

**********************************************************************;
*	Initial Data Quality (QA/QC) check;
**********************************************************************;

*	Parse all variables and identify missing values.
	Note: just because values are not missing does not mean remaining
	values are correct! Typos happen!;

proc means data = AHD_WF NMISS N;
run; quit;

*	Another way to identify missing and non-missing variables
	This will cover both character and numeric values;
proc format;
	value $missfmt ' ' = 'Missing' other = 'Not Missing';
	value missfmt . = 'Missing' other = 'Not Missing';
run;

proc freq data = AHD;
	format _CHAR_ $missfmt.;
	tables _CHAR_ / missing missprint nocum nopercent;
	format _NUMERIC_ missfmt.;
	tables _NUMERIC_ / missing missprint nocum nopercent;
run;

*	Select twenty variables for a data quality check. Split these
	by continuous and discrete. For these purposes I have chosen an
	even mix of the two.;

*	Discrete Variables;
*	Doing both proc means and proc univariate.
	Proc univariate will help with tail observations.;
proc means data = AHD;
	var BedroomAbvGr Fireplaces FullBath GarageYrBlt GarageCars 
		TotRmsAbvGrd MoSold YrSold YearBuilt YearRemodel;
run; quit;

proc univariate data = AHD;
	var BedroomAbvGr Fireplaces FullBath GarageYrBlt GarageCars 
		TotRmsAbvGrd MoSold YrSold YearBuilt YearRemodel;
run; quit;

*	Continuous Variables;
*	Doing both proc means and proc univariate.
	Proc univariate will help with tail observations.;
proc means data = AHD;
	var LotArea GrLivArea FirstFlrSF SecondFlrSF TotalBsmtSF 
		GarageArea PoolArea WoodDeckSF MiscVal ScreenPorch;
run; quit;
	
proc univariate data = AHD;
	var LotArea GrLivArea FirstFlrSF SecondFlrSF TotalBsmtSF 
		GarageArea PoolArea WoodDeckSF MiscVal ScreenPorch;
run; quit;

*	Now we can create a "clone" data set with missing values removed.
	If we wish, we could start with this data set then apply the
	waterfall conditions.;
data AHD_NoMiss;
	set AHD;
	if cmiss(of _all_) then delete;
run;

**********************************************************************;
*	Initial Exploratory Data Analysis;
**********************************************************************;

*	DISCRETE VARIABLES;
*	Mix of proc freq and Bar Charts;
proc freq data = AHD;
	tables BedroomAbvGr / plots = FreqPlot(scale = Percent) out = Freq1Out;
run; quit;

proc freq data = AHD;
	tables FullBath / plots = FreqPlot(scale = Percent) out = Freq1Out;
run; quit;

proc freq data = AHD;
	tables TotRmsAbvGrd / plots = FreqPlot(scale = Percent) out = Freq1Out;
run; quit;

proc freq data = AHD;
	tables YearBuilt / plots = FreqPlot(scale = Percent) out = Freq1Out;
run; quit;

*	CONTINUOUS VARIABLES;
proc means data = AHD;
	var LotArea FirstFlrSF SecondFlrSF TotalBsmtSF GarageArea;
run; quit;

*	Histograms are used for continuous variables;
proc sgplot data = AHD;
	histogram LotArea / transparency = 0.5;
	density LotArea / type = normal;
	density LotArea / type = kernel;
	title 'Histogram of variable LotArea';
run; quit;

proc sgplot data = AHD;
	histogram GrLivArea / transparency = 0.5;
	density GrLivArea / type = normal;
	density GrLivArea / type = kernel;
	title 'Histogram of variable GrLivArea';
run; quit;

proc sgplot data = AHD;
	histogram FirstFlrSF / transparency = 0.5;
	density FirstFlrSF / type = normal;
	density FirstFlrSF / type = kernel;
	title 'Histogram of variable FirstFlrSF';
run; quit;

proc sgplot data = AHD;
	histogram SecondFlrSF / transparency = 0.5;
	density SecondFlrSF / type = normal;
	density SecondFlrSF / type = kernel;
	title 'Histogram of variable SecondFlrSF';
run; quit;

proc sgplot data = AHD;
	histogram TotalBsmtSF / transparency = 0.5;
	density TotalBsmtSF / type = normal;
	density TotalBsmtSF / type = kernel;
	title 'Histogram of variable TotalBsmtSF';
run; quit;

proc sgplot data = AHD;
	histogram GarageArea / transparency = 0.5;
	density GarageArea / type = normal;
	density GarageArea / type = kernel;
	title 'Histogram of variable GarageArea';
run; quit;

*	We can also do Box Plots;
proc sgplot data = AHD;
	vbox LotArea;
	title 'Boxplot of variable LotArea';
run; quit;

proc sgplot data = AHD;
	vbox GrLivArea;
	title 'Boxplot of variable GrLivArea';
run; quit;

proc sgplot data = AHD;
	vbox FirstFlrSF;
	title 'Boxplot of variable FirstFlrSF';
run; quit;

proc sgplot data = AHD;
	vbox SecondFlrSF;
	title 'Boxplot of variable SecondFlrSF';
run; quit;

proc sgplot data = AHD;
	vbox TotalBsmtSF;
	title 'Boxplot of variable TotalBsmtFSF';
run; quit;

proc sgplot data = AHD;
	vbox GarageArea;
	title 'Boxplot of variable GarageArea';
run; quit;

**********************************************************************;
*	Initial Exploratory Data Analysis for Modeling;
**********************************************************************;

*	Now take the ten predictor or explanatory variables and model them
	against the response variable.;
*	Use LOESS smoother here, be comfortable. Add titles beyond 
	"variable X".;

*	DISCRETE VARIABLES;
*	Scatter Plots, using 95% ci for regression line
ods graphics on;
proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Above Grade Bedrooms';
	title2 'Regression bands at 95% Confidence Level';
	compare x = BedroomAbvGr y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Above Grade Full Bathrooms';
	title2 'Regression bands at 95% Confidence Level';
	compare x = FullBath y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Rooms Above Grade';
	title2 'Regression bands at 95% Confidence Level';
	compare x = TotRmsAbvGrd y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Year Built';
	title2 'Regression bands at 95% Confidence Level';
	compare x = YearBuilt y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

*	CONTINUOUS VARIABLES;
*	Scatter Plots, using 95% ci for regression line;
proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Lot Size in SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = LotArea y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Above Grade Living Area in SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = GrLivArea y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by First Floor SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = FirstFlrSF y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Second Floor SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = SecondFlrSF y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Basement SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = TotalBsmtSF y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;

proc sgscatter data = AHD;
	title1 'Scatter Plot of Sale Price by Garage Area SF';
	title2 'Regression bands at 95% Confidence Level';
	compare x = GarageArea y = SalePrice / loess reg = (alpha = 0.05 cli clm);
run; quit;
ods graphics off;







