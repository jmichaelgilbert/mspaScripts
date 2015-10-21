**********************************************************************;
*	SAS_EDA_Sandbox;
*	Last updated: 2015-10-21 by MJG;
**********************************************************************;

*	Generic macros to help conduct EDA on any data set;
*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = MB;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.moneyball;
run; quit;

**********************************************************************;
**********************************************************************;
*	Split data for cross-validation;
**********************************************************************;
**********************************************************************;

*	Split the data create variables and flags as necessary;
data &data_og.;
	set &data_og.;
	U = uniform(123);
	if (U <= 0.70) then TRAIN = 1;
		else TRAIN = 0;
	if TRAIN = 0 then TEST = 1;
		else TEST = 0;
run; quit;

*	Use PROC FREQ to determine how close we are to 70/30 split;
proc freq data = &data_og.;
	tables train test;
run; quit;

***********************************;
*	Training;
***********************************;

*	Create training data set;
data &data_og._70;
	set &data_og.;
	where train = 1;
run; quit;

*	Drop unnecessary variables;
data &data_og._70;
	set &data_og._70 (drop = u train test);
run; quit;

***********************************;
*	Test;
***********************************;

*	Create test data set;
data &data_og._30;
	set &data_og.;
	where test = 1;
run; quit;

*	Drop unnecessary variables;
data &data_og._30;
	set &data_og._30 (drop = u train test);
run; quit;

**********************************************************************;
**********************************************************************;
*	SAS Macros;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Globals;
**********************************************************************;

*	Declare data set to be used;
*	Either full, 70-split, or 30-split;
%let data_og = MB;

*	Declare subsequent macro variables;
%let data_trim = &data_og._trim;
%let data_imp = &data_trim._imp;
%let data_trans = &data_imp._trans;
%let data_pca = &data_trans._pca;
%let contents = &data_og._contents;
%let contents_trim = &contents._trim;
%let contents_trans = &contents._trans;
%let corr = &data_og._corr;
%let varname = name;
%let response = TARGET_WINS;	*Response Variable;
%let key = INDEX;		*Primary, foreign, or other key;

**********************************************************************;
*	Macro to drop P-values and N-values from CORR output;
**********************************************************************;
%macro corr(varname);
	data long_&corr.;
		set long_&corr.;
		if _name_ = "N&varname." then delete;
			else if _name_ = "N&response." then delete;
			else if _name_ = "P&varname." then delete;
			else if _name_ = "P&response." then delete;
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

*	Create the data set;
data &data_imp.;
	set &data_trim.;
run; quit;

********************;
*	Missing Flags;
********************;

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

*	Execute PROC MI;
proc mi data = &data_imp.;
	em out = &data_imp. maxiter = 1000;
	var _all_;
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
			else where type = 1;
				call execute('%scatter('||name||')');
				call execute('%histogram('||name||')');
				call execute('%boxplot('||name||')');
	end;
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

**********************************************************************;
*	Model Prep;
**********************************************************************;

*	Merge output, bring response and key back in;
*	Add in log transform of response;
data cv_data;
	merge &data_pca._output &data_og. (keep = &response. &key.);
	&response._ln = sign(&response.) * log(abs(&response.)+1);
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;
