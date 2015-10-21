**********************************************************************;
*	411-DL | Unit_01;
*	Last updated: 2015-10-15 by MJG;
**********************************************************************;

*	Generic macros to help conduct EDA on any data set;
*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

*	Shorten data name, save to work library;
data MB;
	set mydata.moneyball;
run; quit;

**********************************************************************;
**********************************************************************;
*	SAS Macros;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Globals;
**********************************************************************;
%let data_og = MB;
%let data_trim = &data_og._trim;
%let data_imp = &data_trim._imp;
%let data_trans = &data_imp._trans;
%let data_pca = &data_trans._pca;
%let contents = &data_og._contents;
%let contents_trim = &contents._trim;
%let contents_trans = &contents._trans;
%let corr = &data_og._corr;
%let varname = name;
%let key = INDEX;				*Primary, foreign, or other key;
%let response = TARGET_WINS;	*Response Variable;

**********************************************************************;
*	Macro to drop P-values and N-values from CORR output;
**********************************************************************;
%macro corr(varname);
	data long_&corr.;
		set long_&corr.;
		if _name_ = "N&varname." then delete;
			else if _name_ = "P&varname." then delete;
	run; quit;
%mend;

**********************************************************************;
*	Macro for scatterplots;
**********************************************************************;
%macro scatter(varname);
	ods graphics on;
	proc sgscatter data = &data_defined.;
		compare x = &varname. y = &response. / loess reg;
		title "Scatter Plot of &response. by &varname.";
		title2 "with LOESS smoother";
	run; quit;
	ods graphics off;
%mend;

**********************************************************************;
*	Macro for histograms;
**********************************************************************;
%macro histogram(varname);
	proc sgplot data = &data_defined.;
		histogram &varname. / transparency = 0.5;
		density &varname. / type = normal;
		density &varname. / type = kernel;
		title "Histogram of &varname.";
		title2 "with normal and kernel density estimates";
	run; quit;
%mend;

**********************************************************************;
*	Macro for boxplots;
**********************************************************************;
%macro boxplot(varname);
	proc sgplot data = &data_defined.;
		vbox &varname.;
		title "Boxplot of &varname.";
	run; quit;
%mend;

**********************************************************************;
*	Macro to output summary stats from PROC MEANS across all variables;
*	As designed, must be used in conjunction with PROC TRANSPOSE;
**********************************************************************;
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

**********************************************************************;
*	Macro to transpose summary stats from %macro means(varname);
**********************************************************************;
%macro transpose(varname);
	proc transpose data = &varname. out = &varname._t;
		var _numeric_;
		by _character_;
	run; quit;
%mend;

**********************************************************************;
*	Macro to store summary stats from %macro means(varname);
*	Strip will remove leading or trailing space;
*	CALL SYMPUTX differs from CALL SYMPUT in that you specify
	where the macros are stored (e.g. local or global);
**********************************************************************;
%macro symputx(varname);
	data _null_;
		set &varname._t;
			call symputx(_name_, strip(col1), 'g');
	run; quit;
%mend;

**********************************************************************;
*	Macro for trims of data;
**********************************************************************;
%macro trim(varname);
	data &data_trim.;
		set &data_trim.;
			&varname._T75	=	max(min(&varname.,&&&varname._P75),&&&varname._P25);
			&varname._T90	=	max(min(&varname.,&&&varname._P90),&&&varname._P10);
			&varname._T95	=	max(min(&varname.,&&&varname._P95),&&&varname._P5);
			&varname._T99	=	max(min(&varname.,&&&varname._P99),&&&varname._P1);
	run; quit;
%mend;

**********************************************************************;
*	Macro for imputes (mean, median, mode);
*	Note: not used in this code, PROC MI makes this obsolete;
**********************************************************************;
%macro impute(varname);
	data &data_imp.;
		set &data_imp.;
			&varname._IMEW 	= &varname.;
				if missing(&varname._IMEW) then &varname._IMEW = &&&varname._mean;
			&varname._IMED 	= &varname.;
				if missing(&varname._IMED) then &varname._IMED = &&&varname._mode;
			&varname._IMOD	= &varname.;
				if missing(&varname._IMOD) then &varname._IMOD = &&&varname._median;
	run; quit;
%mend;

**********************************************************************;
*	Macro for missing flags;
**********************************************************************;
%macro missing(varname);
	data &data_imp.;
		set &data_imp.;
			MF_&varname. = missing(&varname.);
	run; quit;
%mend;

**********************************************************************;
*	Macro for natural log transform;
**********************************************************************;
%macro transform(varname);
	data &data_trans.;
		set &data_trans.;
			&varname._ln = sign(&varname.) * log(abs(&varname.)+1);
	run; quit;
%mend;

**********************************************************************;
**********************************************************************;
*	Initial EDA & Data Preparation on OG data set;
**********************************************************************;
**********************************************************************;

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
		if name = "&response." then delete;
			else if name = "&key." then delete;
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
proc corr data = &data_og. (drop = &key.);
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
*	Visual EDA (INITIAL);
**********************************************************************;

*	Conduct EDA on all _NUMERIC_ variables;
*	Excludes response variable and any primary, foreign, or other key;

%let data_defined = &data_og.;

data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
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

*	Verify macro variables are stored correctly;
%put _local_;		*	All local macro variables;
%put _global_;		*	All global macro variables;
%put _user_;		*	All macro variables;

**********************************************************************;
*	Outliers Identification;
**********************************************************************;

proc print data = &contents.;
run; quit;

*	Create initial model and examine results for possible outliers;
*	Does anything stand out as a candidate for outliers?;

*	Multiple Linear Regression Model;
*	Output results to AHD_MLR to prep for outlier identification;
proc reg data = &data_og. plots = (residuals(smooth));
	model &response. = 
	TEAM_BASERUN_CS
	TEAM_BASERUN_SB
	TEAM_BATTING_2B
	TEAM_BATTING_3B
	TEAM_BATTING_BB
	TEAM_BATTING_H
	/* TEAM_BATTING_HBP */
	TEAM_BATTING_HR
	TEAM_BATTING_SO
	TEAM_FIELDING_DP
	TEAM_FIELDING_E
	TEAM_PITCHING_BB
	TEAM_PITCHING_H
	TEAM_PITCHING_HR
	TEAM_PITCHING_SO
	/ stb clb vif;
	output out = &data_og._outlier cookd = cookd h = leverage student = residual
run; quit;

data &data_og._outlier;
	set &data_og._outlier;
	residual_sq = residual*residual;
	cook_lev = cookd*leverage;
run; quit;

*	First scatterplot: examine leverage versus squared residuals;
proc sgplot data = &data_og._outlier;
	scatter y = leverage x = residual_sq / datalabel = INDEX;
	title "Leverage and Squared Residuals";
run; quit;

*	Second scatterplot: examine Cook's D versus squared residuals;
proc sgplot data = &data_og._outlier;
	scatter y = cookd x = residual_sq / datalabel = INDEX;
	title "Cook's D and Squared Residuals";
run; quit;

*	From scatterplots, observations with these properities look
	to be good candidates for outliers. What properties (if any) do
	they have in common?;
data &data_og._outlier_slim;
	set &data_og._outlier;
	where ((cookd > 4/1486) or (leverage > 0.010) or (residual_sq > 8));
run; quit;

*	Sort data by (Cook's D * Leverage) score descending;
proc sort data = &data_og._outlier_slim;
	by descending cook_lev;
run; quit;

*	View the results;
proc print data = &data_og._outlier_slim;
run; quit;

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

*	Create the data set;
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
		if name = "&response." then delete;
			else if name = "&key." then delete;
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

***********************************;
*	Impute & Flags;
***********************************;

*	Impute using PROC MI to replace missing values in current variables;
*	NOTE ON FLAGS: for PROC MI, flag before as PROC MI will REPLACE;

********************;
*	PROC MI;
********************;

*	Create the data set;
data &data_imp.;
	set &data_trim.;
run; quit;

*	Add in the missing flags;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			call execute('%missing('||name||')');
	end;
run; quit;

*	Execute PROC MI;
proc mi data = &data_imp.;
	em out = &data_imp. maxiter = 1000;
	var _all_;
run; quit;

*	Verify data with PROC MEANS;
proc means data = &data_imp. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

***********************************;
*	Transform;
***********************************;

*	Now we create transformed versions of the variables in these data sets;
*	Use the PROC CONTENTS (REDUX) as that allows us to skip MF_ transforms;

*	Create the data set;
data &data_trans.;
	set &data_imp.;
run; quit;

*	Transform variables to log;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
				call execute('%transform('||name||')');
	end;
run; quit;		

proc contents data = &data_trans.;
run; quit;

**********************************************************************;
**********************************************************************;
*	Revisit visual and quantitative EDA on current data set;
**********************************************************************;
**********************************************************************;

***********************************;
*	Visual EDA (REDUX);
***********************************;

*	Conduct EDA on all _NUMERIC_ variables;
*	Excludes response variable and any primary, foreign, or other key;

%let data_defined = &data_trans.;

data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			where type = 1;
				call execute('%scatter('||name||')');
				call execute('%histogram('||name||')');
				call execute('%boxplot('||name||')');
	end;
run; quit;

*	For hand-built model, review variables, log transforms, etc;

*	Verify data with PROC MEANS;
proc means data = &data_trans. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

***********************************;
*	PROC CONTENTS (REDUX);
***********************************;

*	Intermediate step after the trans data set is created;
*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;

proc contents data = &data_trans. out = &contents_trans.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data &contents_trans.;
	set &contents_trans.(keep = name type length varnum format formatl
		informat informl just npos nobs);
		if name = "&response." then delete;
			else if name = "&key." then delete;
run; quit;

*	View contents of data set, more info than OG PROC CONTENTS output;
proc print data = &contents_trans.;
run; quit;

***********************************;
*	PROC CORR (REDUX);
***********************************;

*	Use ODS Output to print Pearson's Correlation for data;
*	Can also specify "outp = &data._corr" in first line of PROC CORR;

ods trace on;
ods output PearsonCorr = wide_&corr.;
proc corr data = &data_trans. (drop = &key.);
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
		set &contents_trans. nobs = num;
			call execute('%corr('||name||')');
	end;
run; quit;

*	Rename columns and set data back to corr (trans);
data &corr.;
	set long_&corr.;
	COL1 = abs(COL1);
	if missing(COL1) then delete;
	rename _NAME_ = Variable COL1 = PPC;
run; quit;

*	Sort the results;
proc sort data = &corr.;
	by descending PPC variable ;
run; quit;

*	Verify the results;
proc print data = &corr.;
run; quit;

**********************************************************************;
**********************************************************************;
*	Principal Components;
**********************************************************************;
**********************************************************************;

*	Use principal components to reduce the dimensionality of the data,
	and create orthogonal predictor variables (IID);
*	This will also handle multicollinearity or VIFs;
*	Need to manually view scree plot to determine number of principal
	components to keep in regression;

*	Drop response variable and subs of response, and key;
data &data_pca.;
	set &data_trans. (drop = &response.: &key.);
run; quit;

*	Compute principal components;
ods graphics on;
proc princomp data = &data_pca. out = &data_pca._output
	plots = scree(unpackpanel) N = 30;
	ods output eigenvectors = pca_ev;
run; quit;
ods graphics off;

*	Transpose data;
proc transpose data = pca_ev out = pca_ev_trans;
	id variable;
run; quit;

*	Prep for scoring;
data pca_ev_score;
	set pca_ev_trans;
	_TYPE_ = "SCORE";
run; quit;

*	Now save this data set to be used to score Moneyball_Test;

**********************************************************************;
*	Model Prep: Train / Test Split for Cross-Validation;
**********************************************************************;

*	Merge output, bring response and key back in;
*	Add in log transform of response;
data cv_data;
	merge &data_pca._output &data_og. (keep = &response. &key.);
	&response._ln = sign(&response.) * log(abs(&response.)+1);
run; quit;

*	Split the data create variables and flags as necessary;
data cv_data;
	set cv_data;
	if cmiss(of _all_) then delete;
	U = uniform(123);
	if (U < 0.70) then TRAIN = 1;
		else TRAIN = 0;
	if (U > 0.70) then TEST = 1;
		else TEST = 0;
	if (TRAIN = 1) then TRAIN_&response. = &response.;
		else TRAIN_&response. = .;
	if (TEST = 1) then TEST_&response. = &response.;
		else TEST_&response. = .;
	if (TRAIN = 1) then TRAIN_&response._ln = &response._ln;
		else TRAIN_&response._ln = .;
	if (TEST = 1) then TEST_&response._ln = &response._ln;
		else TEST_&response._ln = .;
run; quit;

proc contents data = cv_data;
run; quit;

*	Use PROC FREQ to determine how close we are to 70/30 split;
proc freq data = cv_data;
	tables train test;
run; quit;

**********************************************************************;
**********************************************************************;
*	Regression Modeling;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Operational Validation Format;
**********************************************************************;
*	Create specific format, freq, and tables for model validation;
*	How many of the predicted values for response variable were within
	5%, 10%, and 15% of actuals on training and test?;

proc format;
	value Prediction_Grade (default = 7)
	. = 'Missing'
	0.0 - 0.05 = 'Grade 0'
	0.05 <- 0.10 = 'Grade 1'
	0.10 <- 0.15 = 'Grade 2'
	0.15 <- high = 'Grade 3'
	;
run;

**********************************************************************;
*	OLS Models: Hand-picked, Normal Response;
**********************************************************************;

proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_BATTING_H_T99_ln
	TEAM_BATTING_2B_T99_ln
	TEAM_BATTING_3B_T99
	TEAM_BATTING_HR_T99_ln
	TEAM_BATTING_BB_T95
	TEAM_BASERUN_SB_T99_ln
	TEAM_PITCHING_H_T90_ln
	TEAM_PITCHING_HR_T99_ln
	TEAM_PITCHING_BB_T95
	TEAM_FIELDING_E_T95
	/* Missing Flags */
	MF_TEAM_BASERUN_SB
	/ adjrsq aic bic cp vif;
	output out = ols_all_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data ols_all_response_res;
	set ols_all_response;
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = ols_all_response_res mean nway nmiss;
	var abs_res square_res;
	output out = ols_all_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = ols_all_response_em;
run; quit;

*	Operational Validation;
data ols_all_response_OV;
	set ols_all_response;
	OV = abs(((yhat-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = ols_all_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

**********************************************************************;
*	OLS Models: Hand-picked, Log Response;
**********************************************************************;

proc reg data = cv_data plots = diagnostics;
	model &response._ln = 
	TEAM_BATTING_H_T99_ln
	TEAM_BATTING_2B_T99_ln
	TEAM_BATTING_3B_T99
	TEAM_BATTING_HR_T99_ln
	TEAM_BATTING_BB_T95
	TEAM_BASERUN_SB_T99_ln
	TEAM_PITCHING_H_T90_ln
	TEAM_PITCHING_HR_T99_ln
	TEAM_PITCHING_BB_T95
	TEAM_FIELDING_E_T95
	/* Missing Flags */
	MF_TEAM_BASERUN_SB
	/ adjrsq aic bic cp vif;
	output out = ols_ln_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data ols_ln_response_res;
	set ols_ln_response;
	res = (&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = ols_ln_response_res mean nway nmiss;
	var abs_res square_res;
	output out = ols_ln_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = ols_ln_response_em;
run; quit;

*	Operational Validation;
data ols_ln_response_OV;
	set ols_ln_response;
	OV = abs(((yhat-&response._ln)/&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = ols_ln_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

**********************************************************************;
*	AVS Models: Normal Response;
**********************************************************************;

*	Initial AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_:
	MF_:
	/ selection = stepwise slentry = 0.15 slstay = 0.15 vif;
	output out = sw_all_response predicted = yhat residual = res;
run; quit;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_BATTING_H
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T95
	TEAM_BATTING_BB_T75
	TEAM_FIELDING_DP_T99
	TEAM_BASERUN_SB_ln
	TEAM_FIELDING_DP_ln
	TEAM_FIELDING_E_T90_ln
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = sw_all_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data sw_all_response_res;
	set sw_all_response;
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = sw_all_response_res mean nway nmiss;
	var abs_res square_res;
	output out = sw_all_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = sw_all_response_em;
run; quit;

*	Operational Validation;
data sw_all_response_OV;
	set sw_all_response;
	OV = abs(((yhat-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_all_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

**********************************************************************;
*	AVS Models: Log Response;
**********************************************************************;

*	Initial AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response._ln = 
	TEAM_:
	MF_:
	/ selection = stepwise slentry = 0.15 slstay = 0.15 vif;
	output out = sw_ln_response predicted = yhat residual = res;
run; quit;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response._ln = 
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T90
	TEAM_BATTING_BB_T99
	TEAM_FIELDING_DP_T99
	TEAM_PITCHING_HR_T75
	/* TEAM_BASERUN_SB_ln */
	/* TEAM_BATTING_BB_ln */
	/* TEAM_BATTING_BB_T99_ln */
	/* TEAM_BATTING_H_ln */
	TEAM_BATTING_H_T99_ln
	/* TEAM_FIELDING_DP_ln */
	/* TEAM_FIELDING_E_T90_ln */
	/*	Missing Flags */
	MF_TEAM_BASERUN_SB
	MF_TEAM_FIELDING_DP
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = sw_ln_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data sw_ln_response_res;
	set sw_ln_response;
	res = (&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = sw_ln_response_res mean nway nmiss;
	var abs_res square_res;
	output out = sw_ln_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = sw_ln_response_em;
run; quit;

*	Operational Validation;
data sw_ln_response_OV;
	set sw_ln_response;
	OV = abs(((yhat-&response._ln)/&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_ln_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

**********************************************************************;
*	AVS Models: Normal Response (REDUX);
**********************************************************************;

proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_:
	MF_:
	/ selection = stepwise slentry = 0.15 slstay = 0.15 vif;
	output out = swr_all_response predicted = yhat residual = res;
run; quit;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_BATTING_H
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T95
	TEAM_BATTING_BB_T75
	TEAM_FIELDING_DP_T99
	TEAM_BASERUN_SB_ln
	TEAM_FIELDING_DP_ln
	TEAM_FIELDING_E_T90_ln
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = swr_all_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data swr_all_response_res;
	set swr_all_response;
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = swr_all_response_res mean nway nmiss;
	var abs_res square_res;
	output out = swr_all_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = swr_all_response_em;
run; quit;

*	Operational Validation;
data swr_all_response_OV;
	set swr_all_response;
	OV = abs(((yhat-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = swr_all_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

**********************************************************************;
*	PCA Models: Normal Response;
**********************************************************************;

*	Regression;
proc reg data = cv_data plots = diagnostics;
	model &response. = Prin1-Prin15 
	/ adjrsq aic bic cp vif;
	output out = pca_all_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data pca_all_response_res;
	set pca_all_response;
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = pca_all_response_res mean nway nmiss;
	var abs_res square_res;
	output out = pca_all_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = pca_all_response_em;
run; quit;

*	Operational Validation;
data pca_all_response_OV;
	set pca_all_response;
	OV = abs(((yhat-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = pca_all_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

**********************************************************************;
*	PCA Models: Log Response;
**********************************************************************;

*	Regression;
proc reg data = cv_data plots = diagnostics;
	model &response._ln = Prin1-Prin15
	/ adjrsq aic bic cp vif;
	output out = pca_all_ln predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data pca_all_ln_res;
	set pca_all_ln;
	res = (&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = pca_all_ln_res mean nway nmiss;
	var abs_res square_res;
	output out = pca_all_ln_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = pca_all_ln_em;
run; quit;

*	Operational Validation;
data pca_all_ln_OV;
	set pca_all_ln;
	OV = abs(((yhat-&response._ln)/&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = pca_all_ln_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

***********************************;
*	Models: Champion Model (REDUX);
*	AVS Stepwise Selected;
*	Cross Validation;
***********************************;

***********************************;
*	1. Overall;
***********************************;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response._ln = 
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T90
	TEAM_BATTING_BB_T99
	TEAM_FIELDING_DP_T99
	TEAM_PITCHING_HR_T75
	TEAM_BATTING_H_T99_ln
	/*	Missing Flags */
	MF_TEAM_BASERUN_SB
	MF_TEAM_FIELDING_DP
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = sw_ln_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data sw_ln_response_res;
	set sw_ln_response;
	res = (&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = sw_ln_response_res mean nway nmiss;
	var abs_res square_res;
	output out = sw_ln_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = sw_ln_response_em;
run; quit;

*	Operational Validation;
data sw_ln_response_OV;
	set sw_ln_response;
	OV = abs(((yhat-&response._ln)/&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_ln_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

***********************************;
*	2. Training;
***********************************;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model TRAIN_&response._ln = 
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T90
	TEAM_BATTING_BB_T99
	TEAM_FIELDING_DP_T99
	TEAM_PITCHING_HR_T75
	TEAM_BATTING_H_T99_ln
	/*	Missing Flags */
	MF_TEAM_BASERUN_SB
	MF_TEAM_FIELDING_DP
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = sw_train_ln_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data sw_train_ln_response_res;
	set sw_train_ln_response;
	res = (TRAIN_&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = sw_train_ln_response_res mean nway nmiss;
	var abs_res square_res;
	output out = sw_train_ln_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = sw_train_ln_response_em;
run; quit;

*	Operational Validation;
data sw_train_ln_response_OV;
	set sw_train_ln_response;
	OV = abs(((yhat-TRAIN_&response._ln)/TRAIN_&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_train_ln_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

***********************************;
*	3. Testing;
***********************************;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model TEST_&response._ln = 
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T90
	TEAM_BATTING_BB_T99
	TEAM_FIELDING_DP_T99
	TEAM_PITCHING_HR_T75
	TEAM_BATTING_H_T99_ln
	/*	Missing Flags */
	MF_TEAM_BASERUN_SB
	MF_TEAM_FIELDING_DP
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = sw_test_ln_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data sw_test_ln_response_res;
	set sw_test_ln_response;
	res = (TEST_&response._ln - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = sw_test_ln_response_res mean nway nmiss;
	var abs_res square_res;
	output out = sw_test_ln_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = sw_test_ln_response_em;
run; quit;

*	Operational Validation;
data sw_test_ln_response_OV;
	set sw_test_ln_response;
	OV = abs(((yhat-TEST_&response._ln)/TEST_&response._ln));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_test_ln_response_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

***********************************;
*	4. Truncation & Twist;
***********************************;

*	Testing effects of truncating and rounding yhat;
*	Use max(log(120)) and min(log(30));

*	Create data set and truncate;
data sw_ln_response_trim;
	set sw_ln_response;
	yhat = max(min(yhat,log(120)),log(30));
run; quit;

*	Transform from log(response) to e**response;
data sw_ln_response_twist;
	set sw_ln_response_trim;
	yhat_t = CONSTANT('E')**yhat;
run; quit;

*	Round off as teams cannot have 1.5 wins;
data sw_ln_response_twist;
	set sw_ln_response_twist;
	yhat_t = ROUND(yhat_t, 1);
run; quit;

*	Operational Validation;
data sw_ln_response_twist_OV;
	set sw_ln_response_twist;
	OV = abs(((yhat_t-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = sw_ln_response_twist_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response._LN";
run; quit;

**********************************************************************;
*	Models: Champion Model (REDUX);
*	AVS Stepwise Selected;;
**********************************************************************;

*	Chosen AVS Model;
proc reg data = cv_data plots = diagnostics;
	model &response. = 
	TEAM_BATTING_H
	TEAM_FIELDING_E
	TEAM_BASERUN_SB_T99
	TEAM_BATTING_2B_T75
	TEAM_BATTING_3B_T95
	TEAM_BATTING_BB_T75
	TEAM_FIELDING_DP_T99
	TEAM_BASERUN_SB_ln
	TEAM_FIELDING_DP_ln
	TEAM_FIELDING_E_T90_ln
	/ selection = rsquare start = 10 stop = 10 adjrsq aic bic cp vif;
	output out = swr_all_response predicted = yhat residual = res;
run; quit;

*	Calculate residual: absolute value and square;
data swr_all_response_res;
	set swr_all_response;
	res = (&response. - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Calculate MAE & MSE;
proc means data = swr_all_response_res mean nway nmiss;
	var abs_res square_res;
	output out = swr_all_response_em
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	View the results;
proc print data = swr_all_response_em;
run; quit;

*	Create data set and truncate;
data swr_all_response_twist;
	set swr_all_response;
	yhat = max(min(yhat, 120), 30);
run; quit;

*	Round off as teams cannot have 1.5 wins;
data swr_all_response_twist;
	set swr_all_response_twist;
	yhat = ROUND(yhat, 1);
run; quit;

*	Operational Validation;
data swr_all_response_twist_OV;
	set swr_all_response_twist;
	OV = abs(((yhat-&response.)/&response.));
	Prediction_Grade = put(OV, Prediction_Grade.);
	if Prediction_Grade = 'Missing' then delete;
run; quit;

proc freq data = swr_all_response_twist_OV;
	tables Prediction_Grade;
	title "Operational Validation of &response.";
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;
