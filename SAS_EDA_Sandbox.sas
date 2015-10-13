**********************************************************************;
*	SAS_EDA_Sandbox;
*	Last updated: 2015-10-13 by MJG;
**********************************************************************;

*	Generic macros to help conduct EDA on any data set;
*	This section must be modified by the user;

*	Connect to data;
libname mydata '/sscc/home/m/mga293/411-DL/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data MB;
	set mydata.moneyball;
run; quit;

**********************************************************************;
*	SAS Macros;
**********************************************************************;

*	Globals;
%let data_og = MB;
%let data_trim = &data_og._trim;
%let data_imp = &data_trim._imp;
%let data_imp_man = &data_imp._man;
%let data_imp_mi = &data_imp._mi;
%let contents = &data_og._contents;
%let contents_trim = &contents._trim;
%let corr = &data_og._corr;
%let varname = name;
%let key = INDEX;				*Primary, foreign, or other key;
%let response = TARGET_WINS;	*Response Variable;

*	Macro to drop P-values and N-values from CORR output;
%macro corr(varname);
	data long_&corr.;
		set long_&corr.;
		if _name_ = "N&varname." then delete;
			else if _name_ = "P&varname." then delete;
	run; quit;
%mend;

*	Macro for scatterplots;
%macro scatter(varname);
	ods graphics on;
	proc sgscatter data = &data_og.;
		compare x = &varname. y = &response. / loess reg;
		title "Scatter Plot of &response. by &varname.";
		title2 "with LOESS smoother";
	run; quit;
	ods graphics off;
%mend;

*	Macro for histograms;
%macro histogram(varname);
	proc sgplot data = &data_og.;
		histogram &varname. / transparency = 0.5;
		density &varname. / type = normal;
		density &varname. / type = kernel;
		title "Histogram of &varname.";
		title2 "with normal and kernel density estimates";
	run; quit;
%mend;

*	Macro for boxplots;
%macro boxplot(varname);
	proc sgplot data = &data_og.;
		vbox &varname.;
		title "Boxplot of &varname.";
	run; quit;
%mend;

*	Macro to output summary stats from PROC MEANS across all variables;
*	As designed, must be used in conjunction with PROC TRANSPOSE;
%macro means(varname);
	proc means data = &data_defined. noprint;
	output out = &varname. (drop = _freq_ _type_)
		nmiss(&varname.)	= &varname._nmiss
		n(&varname.)		= &varname._n
		mean(&varname.)	 	= &varname._mean
		median(&varname.)	= &varname._median
		mode(&varname.) 	= &varname._mode
		std(&varname.)	 	= &varname._std
		skew(&varname.)	 	= &varname._skew
		P1(&varname.)	 	= &varname._P1
		P5(&varname.)		= &varname._P5
		P10(&varname.)	 	= &varname._P10
		P25(&varname.)	 	= &varname._P25
		P50(&varname.)	 	= &varname._P50
		P75(&varname.)	 	= &varname._P75
		P90(&varname.)	 	= &varname._P90
		P95(&varname.)	 	= &varname._P95
		P99 (&varname.)		= &varname._P99
		min(&varname.)	 	= &varname._min
		max(&varname.)	 	= &varname._max
		qrange(&varname.)	= &varname._qrange
		;
run; quit;
%mend;

*	Macro to transpose summary stats from %macro means(varname);
%macro transpose(varname);
	proc transpose data = &varname. out = &varname._t;
		var _numeric_;
		by _character_;
	run; quit;
%mend;

*	Macro to store summary stats from %macro means(varname);
*	Strip will remove leading or trailing space;
*	CALL SYMPUTX differs from CALL SYMPUT in that you specify
	where the macros are stored (e.g. local or global);
%macro symputx(varname);
	data _null_;
		set &varname._t;
			call symputx(_name_, strip(col1), 'g');
	run; quit;
%mend;

*	Trim Macro;
%macro trim(varname);
	data &data_trim.;
		set &data_trim.;
			&varname._T75	=	max(min(&varname.,&&&varname._P75),&&&varname._P25);
			&varname._T90	=	max(min(&varname.,&&&varname._P90),&&&varname._P10);
			&varname._T95	=	max(min(&varname.,&&&varname._P95),&&&varname._P5);
			&varname._T99	=	max(min(&varname.,&&&varname._P99),&&&varname._P1);
	run; quit;
%mend;

*	Macro for imputes (mean, median, mode);
%macro impute(varname);
	data &data_imp_man.;
		set &data_imp_man.;
			&varname._IMEW 	= &varname.;
				if missing(&varname._IMEW) then &varname._IMEW = &&&varname._mean;
			&varname._IMED 	= &varname.;
				if missing(&varname._IMED) then &varname._IMED = &&&varname._mode;
			&varname._IMOD	= &varname.;
				if missing(&varname._IMOD) then &varname._IMOD = &&&varname._median;
	run; quit;
%mend;

*	Macro for missing flags;
%macro missing(varname);
	data &data_defined.;
		set &data_defined.;
			&varname._MF = missing(&varname.);
	run; quit;
%mend;

**********************************************************************;
*	DATA MUNGING;
**********************************************************************;

*	Drop primary, foreign, or other key from data set;
*	Drop other unnecessary variables as necessary;
*	Not necessary (in fact, avoid if merging on key) but no value add
	as predictor variable;

data &data_og. (drop = &key.);
	set &data_og.;
run; quit;

**********************************************************************;
*	PROC CONTENTS (INITIAL);
**********************************************************************;

*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;
proc contents data = &data_og. out = &contents.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data &contents.;
	set &contents.(keep = name type length varnum format formatl
		informat informl just npos nobs);
run; quit;

*	View contents of data set, more info than OG PROC CONTENTS output;
proc print data = &contents.;
run; quit;

**********************************************************************;
*	PROC CORR (INITIAL);
**********************************************************************;

*	Use ODS Output to print Pearson's Correlation for data;
*	Can also specify "outp = &data._corr" in first line of PROC CORR;

ods trace on;
ods output PearsonCorr = wide_&corr.;
proc corr data = &data_og.;
	var _all_;
	with &response.;
run; quit;
ods trace off;

*	Transpose to long;
proc transpose data = wide_&corr. out = long_&corr.;
run; quit;

*	Drop P-values and N-values from output;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			call execute('%corr('||name||')');
	end;
run; quit;

*	Rename columns and set data back to corr (original);
data &corr.;
	set long_&corr.;
	rename _NAME_ = Variable COL1 = PPC;
run; quit;

**********************************************************************;
*	Scatter, Histogram, Boxplot;
**********************************************************************;

*	Conduct EDA on all _NUMERIC_ variables;
*	Excludes response variable and any primary, foreign, or other key;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			if name = "&response." then delete;
				else if name = "&key." then delete;
				else where type = 1;
					call execute('%scatter('||name||')');
					call execute('%histogram('||name||')');
					call execute('%boxplot('||name||')');
	end;
run; quit;

**********************************************************************;
*	PROC MEANS (INITIAL);
**********************************************************************;

%let data_defined = &data_og.;

*	For each variable in the data set, extract summary stats from
	proc means and store as varname, then transpose as varname_t;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			call execute('%means('||name||')');
			call execute('%transpose('||name||')');
			call execute('%symputx('||name||')');
	end;
run; quit;

*	Verify data with PROC MEANS;
proc means data = &data_og. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Verify the macro variables are stored correctly (RE: PROC MEANS);
%put _local_;		*	All local macro variables;
%put _global_;		*	All global macro variables;
%put _user_;		*	All macro variables;

**********************************************************************;
*	Trimming, Imputing, and Transforming;
**********************************************************************;

*	Important to follow TIT in this order;
*	For example, MEAN is extremely sensitive to outliers, so should
	trim to (e.g.) P1 & P99 before imputing based on data;
*	Similarly, want to impute and add new variables to data set so
	when transforming occurs it will do so on all versions of variables;
*	Do a second PROC CONTENTS after trimming to include trimmed
	variables for imptues and transforms;

***********************************;
*	Trimming;
***********************************;

*	Clone the OG data set as trim;
data &data_trim.;
	set &data_og.;
run; quit;

*	Append the data set;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			call execute('%trim('||name||')');
	end;
run; quit;

***********************************;
*	PROC CONTENTS (REDUX);
***********************************;

*	Intermediate step after the trim data set is created;
*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;

proc contents data = &data_trim. out = &contents_trim.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data &contents_trim.;
	set &contents_trim.(keep = name type length varnum format formatl
		informat informl just npos nobs);
run; quit;

*	View contents of data set, more info than OG PROC CONTENTS output;
proc print data = &contents_trim.;
run; quit;

***********************************;
*	PROC MEANS (REDUX);
***********************************;

%let data_defined = &data_trim.;

*	For each variable in the data set, extract summary stats from
	proc means and store as varname, then transpose as varname_t;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			call execute('%means('||name||')');
			call execute('%transpose('||name||')');
			call execute('%symputx('||name||')');
	end;
run; quit;

*	Verify data with PROC MEANS;
proc means data = &data_trim. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Verify the macro variables are stored correctly (RE: PROC MEANS);
%put _local_;		*	All local macro variables;
%put _global_;		*	All global macro variables;
%put _user_;		*	All macro variables;

***********************************;
*	Impute & Flags;
***********************************;

*	Conduct imputes on two data sets, cloned from &data_og.;
*	First creates new variables with MEAN, MEDIAN, MODE for missings;
*	Second uses PROC MI to replace missing values in current variables;
*	Benefit of keeping these separate is to see how well model does with
	manual imputes of central tendency vs. programmatic imputes;

*	NOTE ON FLAGS;
*	For manual, flag after to capture across all variables;
*	For PROC MI, flag before because PROC MI will REPLACE;

********************;
*	Create data sets;
********************;

*	Parent:	impute;
data &data_imp.;
	set &data_trim.;
run; quit;

*	Child:	manual impute;
data &data_imp_man.;
	set &data_imp.;
run; quit;

*	Child:	PROC MI impute;
data &data_imp_mi.;
	set &data_imp.;
run; quit;

********************;
*	Manual;
********************;

*	Since there are two data sets created (manual and PROC MI) that use
	the same macro, we define the data set before calling %missing();
*	NOTE: ORDER OF OPERATIONS MATTERS;
%let data_defined = &data_imp_man.;

*	Impute MMM for missing values;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			call execute('%impute('||name||')');
	end;
run; quit;				

*	Verify data with PROC MEANS;
proc means data = &data_imp_man. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Add in the missing flags;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			call execute('%missing('||name||')');
	end;
run; quit;

********************;
*	PROC MI;
********************;

*	Since there are two data sets created (manual and PROC MI) that use
	the same macro, we define the data set before calling %missing();
%let data_defined = &data_imp_mi.

*	Add in the missing flags;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			call execute('%missing('||name||')');
	end;
run; quit;

proc mi data = &data_imp_mi.;
	EM OUT = &data_imp_mi.;
	VAR _all_;
run; quit;

proc print data = &data_imp_mi. (obs = 100);
run; quit;

***********************************;
*	Transform;
***********************************;

*	There are two data sets with original, trimmed, and imputed variables;
*	Now we transform the variables in these data sets;

*	XXX;

**********************************************************************;
*	Principal Components;
**********************************************************************;

*	Since there are so many variables across these two data sets, manually
	building models will be time consuming, even when using automated
	variable selection techniques;
*	Instead, use principal components to reduce the dimensionality of
	the data, and create orthogonal predictor variables (IID);
*	This will also handle multicollinearity or VIFs;
*	Need to manually view scree plot to determine number of principal
	components to keep in regression;
*	In short, principal components is awesome;

*	XXX;

**********************************************************************;
*	Models;
**********************************************************************;

*	Create multiple OLS models for data set;
*	Four parts to each model:
	1. Regression
	2. Calculate residual: absolute value and square
	3. Calculate MAE & MSE
	4. View the results;

***********************************;
*	Models: Training;
***********************************;

*	1. Create the OLS model;
proc reg data = [DATA_SET_NAME]
	outest = [DATA_SET_NAME]_Sum;
	model &response. = 
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

*	2. Although residual is output, want to be sure to calculate it
	where it is not missing - SAS seems to skip this;
data [DATA_SET_NAME]_Res;
	set [DATA_SET_NAME];
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	3. MSE used for predictive modeling IS A DIFFERENT CALCULATION than
	MSE used in statistics and PROC REG, so must calculate it here;
proc means data = [DATA_SET_NAME]_Res mean nway noprint;
	class train;
	var abs_res square_res;
	output out = [DATA_SET_NAME]_EM
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	4. View the results;
proc print data = [DATA_SET_NAME]_EM;
run; quit;

***********************************;
*	Models: Test;
***********************************;

*	1. Create the OLS model;
proc reg data = [DATA_SET_NAME]
	outest = [DATA_SET_NAME]_Sum;
	model &response. = 
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

*	2. Although residual is output, want to be sure to calculate it
	where it is not missing - SAS seems to skip this;
data [DATA_SET_NAME]_Res;
	set [DATA_SET_NAME];
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	3. MSE used for predictive modeling IS A DIFFERENT CALCULATION than
	MSE used in statistics and PROC REG, so must calculate it here;
proc means data = [DATA_SET_NAME]_Res mean nway noprint;
	class test;
	var abs_res square_res;
	output out = [DATA_SET_NAME]_EM
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	4. View the results;
proc print data = [DATA_SET_NAME]_EM;
run; quit;

***********************************;
*	Models: Random;
***********************************;

*	1. Create the OLS model;
proc reg data = [DATA_SET_NAME]
	outest = [DATA_SET_NAME]_Sum;
	model &response. = 
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

*	2. Although residual is output, want to be sure to calculate it
	where it is not missing - SAS seems to skip this;
data [DATA_SET_NAME]_Res;
	set [DATA_SET_NAME];
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	3. MSE used for predictive modeling IS A DIFFERENT CALCULATION than
	MSE used in statistics and PROC REG, so must calculate it here;
proc means data = [DATA_SET_NAME]_Res mean nway noprint;
	class random;
	var abs_res square_res;
	output out = [DATA_SET_NAME]_EM
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	4. View the results;
proc print data = [DATA_SET_NAME]_EM;
run; quit;

**********************************************************************;
*	Scoring;
**********************************************************************;
*	Score multiple models based on Y-hat & error (residual);
*	Model with smallest %change between MSE and MAE in training and test
	is selected;
*	Model may NOT have the highest adjusted R-squared, but for this
	application model for predictive accuracy not statistical inference;
*	The model with the highest adjusted R-squared (or other evaluation
	criterion) may be overfit;
*	Actually do not need to do anything here because models are coded
	to output scoring;

*	Another way to get an output table;
*	Commented out so code will execute without issue;
/*
proc print data = [DATA_SET_NAME];
	var _IN_ _P_ _EDF_ _RSQ_ _ADJRSQ_ _CP_ _AIC_ _BIC_ _MSE_;
run; quit;
*/

**********************************************************************;
*	Output Accuracy;
**********************************************************************;
*	Create specific format, freq, and tables for model validation;
*	How many of the predicted values for response variable were within
	5%, 10%, and 15% of actuals on training and test?;

proc format;
	value $Prediction_Grade (default = 7)
	. = 'Missing'
	0.0 - 0.05 = 'Grade 0'
	0.05 <- 0.10 = 'Grade 1'
	0.10 <- 0.15 = 'Grade 2'
	0.15 <- high = 'Grade 3'
	;
run;

data [DATA_SET_NAME];
	set [DATA_SET_NAME];
	OV = abs(((yhat-log_test_response)/log_test_response));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = [DATA_SET_NAME];
	tables Prediction_Grade;
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;
