**********************************************************************;
*	410-DL | Assignment 02;
*	Last updated: 2015-07-05 by MJG;
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

*	Data dictionary suggests plotting SalePrice vs. GrLivArea;
*	Playing with scatter plot options and LOESS smoother;
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
data AHD_Sample;
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
	on multiple conditions in single if / else-if statement;
*	Note: GT 4000 SQFT included as referenced in Data Dictionary;

*	View table with observations remaining;
proc freq data = AHD_Sample;
	tables drop_condition;
	title 'Sample Waterfall';
run; quit;

*	Looks good, now let's save that data set;
data AHD_Sample;
	set AHD_Sample;
	if (drop_condition = '10: Sample Population');
run; quit;

*	Verify we successfully saved the trimmed data set;
proc contents data = AHD_Sample;
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
*	Linear Regression Models;
*	No transformation on SalePrice
**********************************************************************;

*	First, create two simple linear regression models using a promising
	continuous variable;
*	Second, create multiple linear regression model that uses both
	variables from the simple linear regression model;
	
*	Simple Linear Regression Model #1
*		First, the raw data;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = GrLivArea / alpha = 0.05;
run; quit;

*		Second, additional graphics;
*			Histogram;
proc sgplot data = AHD_Sample;
	histogram GrLivArea / transparency = 0.5;
	density GrLivArea / type = normal;
	density GrLivArea / type = kernel;
	title 'Histogram of variable GrLivArea';
run; quit;
*			Boxplot;
proc sgplot data = AHD_Sample;
	vbox GrLivArea;
	title 'Boxplot of variable GrLivArea';
run; quit;

*	Simple Linear Regression Model #2
*		First, the raw data;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = LotArea / alpha = 0.05;
run; quit;

*		Second, additional graphics;
*			Histogram;
proc sgplot data = AHD_Sample;
	histogram LotArea / transparency = 0.5;
	density LotArea / type = normal;
	density LotArea / type = kernel;
	title 'Histogram of variable LotArea';
run; quit;
*			Boxplot;
proc sgplot data = AHD_Sample;
	vbox LotArea;
	title 'Boxplot of variable LotArea';
run; quit;

*	Multiple Linear Regression Model;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = GrLivArea LotArea / alpha = 0.05 stb;
run; quit;

*		Test correlations among variables in the multiple model;
proc corr data = AHD_Sample;
	var SalePrice GrLivArea LotArea;
run; quit;

**********************************************************************;
*	Linear Regression Models;
*	Logistic Transformation on SalePrice
**********************************************************************;

*	Set the new data set;
data AHD_Sample_Log;
	set AHD_Sample;
	logSalePrice = log(SalePrice);
run; quit;

*	First, create two simple linear regression models using a promising
	continuous variable;
*	Second, create multiple linear regression model that uses both
	variables from the simple linear regression model;
	
*	Simple Linear Regression Model, logSalePrice;
proc reg data = AHD_Sample_Log plots(unpack);
	model logSalePrice = GrLivArea / alpha = 0.05;
run; quit;

*	Simple Linear Regression Model, SalePrice;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = GrLivArea / alpha = 0.05;
run; quit;

*	Simple Linear Regression Model, logSalePrice;
proc reg data = AHD_Sample_Log plots(unpack);
	model logSalePrice = LotArea / alpha = 0.05;
run; quit;

*	Simple Linear Regression Model, SalePrice;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = LotArea / alpha = 0.05;
run; quit;

*	Multiple Linear Regression Model, logSalePrice;
proc reg data = AHD_Sample_Log plots(unpack);
	model logSalePrice = GrLivArea LotArea / alpha = 0.05 stb;
run; quit;

*	Multiple Linear Regression Model, SalePrice;
proc reg data = AHD_Sample plots(unpack);
	model SalePrice = GrLivArea LotArea / alpha = 0.05 stb;
run; quit;

*		Test correlations among variables in the multiple model;
proc corr data = AHD_Sample_Log;
	var logSalePrice GrLivArea LotArea;
run; quit;
