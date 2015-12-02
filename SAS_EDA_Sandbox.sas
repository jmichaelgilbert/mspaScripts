**********************************************************************;
*	SAS_EDA_Sandbox;
*	Last updated: 2015-12-02 by MJG;
**********************************************************************;

*	Generic macros to help conduct EDA on any data set;
*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = ABC_Sample;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.abc_sample;
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

***********************************;
*	Clean-up;
***********************************;

*	Drop unnecessary variables;
data &data_og.;
	set &data_og. (drop = u train test);
run; quit;


**********************************************************************;
**********************************************************************;
*	SAS Macros;
**********************************************************************;
**********************************************************************;

*	Declare data set to be used;
*	Either full, 70-split, or 30-split;
%let data_og = ABC_Sample;

**********************************************************************;
*	Globals;
**********************************************************************;

*	Declare subsequent macro variables;
%let data_imp 		= &data_og._imp;
%let data_trim 		= &data_og._trim;
%let data_trans 	= &data_og._trans;
%let data_cat 		= &data_og._cat;
%let data_model 	= &data_og._model;
%let modelfile 		= &data_og._modelfile;
%let contents 		= &data_og._contents;
%let contents_trim	= &contents._trim;
%let contents_trans	= &contents._trans;
%let contents_model	= &contents._model;
%let corr 		= &data_og._corr;
%let varname 		= name;
%let key 		= INDEX;				*Primary, foreign, or other key;
%let response 		= TARGET;				*Response Variable;

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
*	Macro for renaming variables (for AVS & PROC MI);
**********************************************************************;
%macro rename_num(varname);
	data &data_og.;
		set &data_og. (rename = (&varname. = N_&varname.));
	run; quit;
%mend;

%macro rename_cat(varname);
	data &data_og.;
		set &data_og. (rename = (&varname. = C_&varname.));
	run; quit;
%mend;

**********************************************************************;
*	Macro for scatterplots;
**********************************************************************;
*	Note: unsure if there is a way to set loessobsmax = maxobs or
	something similar that would be based on the obs in the data set;
%macro scatter(varname);
	ods graphics on / loessobsmax = 15000;
	proc sgscatter data = &data_defined.;
		compare x = &varname. y = &response. / loess reg;
		title "Scatter Plot of &response. by &varname.";
		title2 "with LOESS smoother";
	run; quit;
	ods graphics off;
%mend;

**********************************************************************;
*	Macro for QQ plots;
**********************************************************************;
*	Note: data are normalized, but in event they are not, to draw
	theoretical normal distribution line use this code:
	qqplot &varname. / normal(mu = 10 sigma = 0.3 color = blue);
%macro qq(varname);
	ods graphics on;
	proc univariate data = &data_defined. noprint;
		qqplot &varname.; 
		title "QQ Plot of &varname.";
		title2 "with normal distribution reference line";
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
*	Macro for barplots;
**********************************************************************;
%macro barplot(varname);
	ods graphics on;
	proc sgplot data = &data_defined.;
		vbar &varname. / response = &response.
		datalabel categoryorder = respasc;
	run; quit;
	ods graphics off;
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
	run; quit;
%mend;

**********************************************************************;
*	Macro to store summary stats from %macro means(varname);
*	Strip will remove leading or trailing space;
*	CALL SYMPUTX differs from CALL SYMPUT in that you specify
*	where the macros are stored (e.g. local or global);
**********************************************************************;
%macro symputx_num(varname);
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
*	Macro for storing categorical levels;
**********************************************************************;
%macro freq(varname);
	proc freq data = &data_defined. noprint;
		tables &varname. / out = &varname. (drop = COUNT PERCENT);
	run; quit;
%mend;

**********************************************************************;
*	Macro for storing categorical levels as global macro variables;
**********************************************************************;
%macro symputx_cat(varname);
	data _null_;
		set &varname. end = lastobs;
			call symputx(cats("&varname.", _N_), &varname.);
			if lastobs then call symputx("&varname._n", _N_);
	run; quit;
%mend;

**********************************************************************;
*	Macro for merging categorical levels into data set;
**********************************************************************;
%macro catmerge(varname);
	data &data_defined.;
		set &data_defined.;
			%do i = 1 %to &&&varname._n;
				&varname._&&&varname.&i = (&varname. = "&&&varname.&i");
			%end;
	run; quit;
%mend;

**********************************************************************;
*	Macro for natural log, square, and square root transforms;
**********************************************************************;
%macro transform(varname);
	data &data_trans.;
		set &data_trans.;
			&varname._ln = sign(&varname.) * log(abs(&varname.)+1);
			&varname._rt = sign(&varname.) * sqrt(abs(&varname.)+1);
			&varname._sq = (&varname.*&varname.);
	run; quit;
%mend;


**********************************************************************;
**********************************************************************;
*	Initial EDA & Data Preparation on OG data set;
**********************************************************************;
**********************************************************************;


**********************************************************************;
*	Identify missing and invalid values;
**********************************************************************;

*	PROC MEANS for numeric variables ;
proc means data = &data_og. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	PROC FREQ for categorical (character) variables;
proc freq data = &data_og.;
	tables _character_;
run; quit;

**********************************************************************;
*	PROC CONTENTS;
**********************************************************************;

*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;

*	Create contents data set;
proc contents data = &data_og. out = &contents.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
*	Drop response, key, and other specified variables;
data &contents.;
	set &contents.(keep = name type length varnum format formatl
		informat informl just npos nobs);
		if name =: "&response." then delete;
			else if name = "&key." then delete;
run; quit;

*	View the results;
proc print data = &contents.;
run; quit;

***********************************;
*	Rename;
***********************************;

*	Rename numeric variables with a prefix for AVS and PROC MI;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%rename_num('||name||')');
	end;
run; quit;

*	Rename categorical variables with a prefix for AVS and PROC MI;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 2;
				call execute('%rename_cat('||name||')');
	end;
run; quit;

*	Re-create contents data set with new variable names;
proc contents data = &data_og. out = &contents.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
*	Drop response, key, and other specified variables;
data &contents.;
	set &contents.(keep = name type length varnum format formatl
		informat informl just npos nobs);
		if name = "&response." then delete;
			else if name = "&key." then delete;
run; quit;

*	View the results;
proc print data = &contents.;
run; quit;

**********************************************************************;
*	PROC CORR;
**********************************************************************;

*	Use ODS Output to print Pearson's Correlation for data;
*	Can also specify "outp = &data._corr" in first line of PROC CORR;

ods trace on;
ods output PearsonCorr = wide_&corr.;
proc corr data = &data_og.;
	var N_:;
	with &response.;
run; quit;
ods trace off;

*	Transpose to long;
proc transpose data = wide_&corr. out = long_&corr.;
run; quit;

*	View the results;
proc print data = long_&corr.;
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

*	Sort by PPC;
proc sort data = &corr. out = &corr.;
	by descending PPC;
run; quit;

*	View the results;
proc print data = &corr.;
run; quit;

**********************************************************************;
*	Visual EDA;
**********************************************************************;

*	Conduct EDA on variables;
*	Excludes response variable and any primary, foreign, or other key;

%let data_defined = &data_og.;

*	Numeric variables;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%qq('||name||')');				
				call execute('%scatter('||name||')');
				call execute('%histogram('||name||')');
				call execute('%boxplot('||name||')');
	end;
run; quit;

*	Categorical variables;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 2;
				call execute('%barplot('||name||')');
	end;
run; quit;

*	Response variable;
data _null_;
	call execute('%qq('||"&response."||')');				
	call execute('%histogram('||"&response."||')');
	call execute('%boxplot('||"&response."||')');
run; quit;


**********************************************************************;
**********************************************************************;
*	Imputing, Trimming, and Transforming;
**********************************************************************;
**********************************************************************;


*	Different order - impute first, then trim, then transform;
*	During truncate, SAS appears to treat missing = 0, since the
	resulting child variables at various trims do not have missing values
	even if the parent variables did;

***********************************;
*	Imputes & Missing Flags;
***********************************;

*	Impute using PROC MI to replace missing values in current variables;
*	Be sure to create missing flags (MF_) first;

*	Create the data set;
data &data_imp.;
	set &data_og.;
run; quit;

********************;
*	Missing Flags;
********************;

*	Add in missing flags;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%missing('||name||')');
	end;
run; quit;

*	Verify data with PROC MEANS;
proc means data = &data_imp. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

********************;
*	PROC MI;
********************;

*	Execute PROC MI;
proc mi data = &data_imp. seed = 123 minimum = 0;
	em out = &data_imp.;
	var N_:;
run; quit;

***********************************;
*	Trims;
***********************************;

*	Create the data set;
data &data_trim.;
	set &data_imp.;
run; quit;

%let data_defined = &data_trim.;

*	For each numeric variable in the data set, extract summary stats 
	from proc means and store as varname, then transpose as varname_t;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%means('||name||')');
				call execute('%transpose('||name||')');
				call execute('%symputx_num('||name||')');
	end;
run; quit;

*	Verify values with PROC MEANS;
proc means data = &data_trim. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Verify values as global macro variables;
%put _global_;

*	Append the data set;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%trim('||name||')');
	end;
run; quit;

proc contents data = &data_trim.;
run; quit;

***********************************;
*	PROC CONTENTS;
***********************************;

*	Intermediate step after the trim data set is created;
*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;

*	Create contents data set;
proc contents data = &data_trim. out = &contents_trim.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data &contents_trim.;
	set &contents_trim.(keep = name type length varnum format formatl
		informat informl just npos nobs);
		if name =: "&response." then delete;
			else if name = "&key." then delete;
			else if name =: "MF" then delete;
run; quit;

*	View the results;
proc print data = &contents_trim.;
run; quit;

***********************************;
*	Transform;
***********************************;

*	Now create transformed versions of the variables in these data sets;

*	Create the data set;
data &data_trans.;
	set &data_trim.;
run; quit;

*	Transform variables to natural log, square, and square root;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			where type = 1;
				call execute('%transform('||name||')');
	end;
run; quit;		

*	View the contents;
proc contents data = &data_trans.;
run; quit;


**********************************************************************;
**********************************************************************;
*	Categorical Variables;
**********************************************************************;
**********************************************************************;


*	Create the data set;
data &data_cat.;
	set &data_trans.;
run; quit;

*	So far manipulation has focused on numeric variables;
*	What about categorical?;

*	Identify categorical variables and levels;
proc freq data = &data_cat.;
	tables _character_;
run; quit;

*	Create dummy variables;
%let data_defined = &data_cat.;

*	Extract levels and store as global macro variables;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			where type = 2;
				call execute('%freq('||name||')');
				call execute('%symputx_cat('||name||')');
	end;
run; quit;

*	Create the new variables and merge;
data _null_;
	do i = 1 to num;
		set &contents_trim. nobs = num;
			where type = 2;
				call execute('%catmerge('||name||')');
	end;
run; quit;

*	Verify the results;
proc contents data = &data_cat.;
run; quit;

*	Verify values with PROC MEANS;
proc means data = &data_cat. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;


**********************************************************************;
**********************************************************************;
*	Final Cleanup;
**********************************************************************;
**********************************************************************;


*	Create the data set;
data &data_model.;
	set &data_trans.;
run; quit;

*	Check the contents;
proc contents data = &data_model.;
run; quit;


**********************************************************************;
**********************************************************************;
*	Models;
**********************************************************************;
**********************************************************************;


*	Now go forth and build;
