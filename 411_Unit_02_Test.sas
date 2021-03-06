**********************************************************************;
*	411-DL | Unit_02;
*	Last updated: 2015-11-04 by MJG;
**********************************************************************;

*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = INS_TEST;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.logit_insurance;
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


**********************************************************************;
*	Globals;
**********************************************************************;

*	Declare data set to be used;
*	Either full, 70-split, or 30-split;
%let data_og = INS_TEST_30;

*	Declare subsequent macro variables;
%let data_imp = &data_og._imp;
%let data_trim = &data_og._trim;
%let data_trans = &data_og._trans;
%let data_cat = &data_og._cat;
%let data_model = &data_og._model;
%let contents = &data_og._contents;
%let contents_trim = &contents._trim;
%let contents_trans = &contents._trans;
%let contents_model = &contents._model;
%let corr = &data_og._corr;
%let varname = name;
%let key = INDEX;				*Primary, foreign, or other key;
%let response = TARGET_FLAG;	*Response Variable;

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
	where the macros are stored (e.g. local or global);
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
*	Macro for natural log and squared transform;
**********************************************************************;
%macro transform(varname);
	data &data_trans.;
		set &data_trans.;
			&varname._ln = sign(&varname.) * log(abs(&varname.)+1);
			&varname._sq = (&varname.*&varname.);
			&varname._rt = sqrt(&varname.);
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


*	Variable CAR_AGE has a negative value, not possible;
*	Perhaps sign should be changed;
*	Use "(CAR_AGE ne .)" otherwise missing will also be returned;
proc print data = &data_og.;
	where (CAR_AGE ne .) and (CAR_AGE < 0);
run; quit;
*	One observation, CAR_TYPE = Pickup & BLUEBOOK = $15,390;

*	Is it reasonable that sign was entered incorrectly?;
proc means data = &data_og. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
	where CAR_AGE = 3;
	class CAR_TYPE;
	var BLUEBOOK;
run; quit;
*	Mean value of BLUEBOOK for CAR_TYPE = Pickup & CAR_AGE = 3 is $14,814;
*	Reasonable that sign was entered incorrectly;

*	Correct erroneous value on CAR_AGE;
data &data_og.;
	set &data_og.;
	CAR_AGE = abs(CAR_AGE);
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
		if name = "&response." then delete;
			else if name = "&key." then delete;
			else if name = "TARGET_AMT" then delete;
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
			else if name = "TARGET_AMT" then delete;
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

*	View the results;
proc print data = &corr.;
run; quit;

**********************************************************************;
*	Visual EDA;
**********************************************************************;

*	Conduct EDA on variables;
*	Excludes response variable and any primary, foreign, or other key;
*	Excludes %scatter as &response. is binary and not useful;

%let data_defined = &data_og.;

*	Numeric variables;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
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
*	Imputes & Flags;
***********************************;

*	Create the data set;
data &data_imp.;
	set &data_og.;
run; quit;

*	Add in missing flags;
data _null_;
	do i = 1 to num;
		set &contents. nobs = num;
			where type = 1;
				call execute('%missing('||name||')');
	end;
run; quit;

*	Verify data with PROC MEANS;
proc means data = &data_og. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Years on Job by Job & Education;
proc means data = &data_og. nmiss mean;
class C_JOB C_EDUCATION;
var N_YOJ;
run; quit;

*	Years on Job by Job;
proc means data = &data_og. nmiss mean;
class C_JOB;
var N_YOJ;
run; quit;

*	Income by Job & Education;
proc means data = &data_og. nmiss mean;
class C_JOB C_EDUCATION;
var N_INCOME;
run; quit;

*	Income by Job;
proc means data = &data_og. nmiss mean;
class C_JOB;
var N_INCOME;
run; quit;

*	Home Value by Job & Education;
proc means data = &data_og. nmiss mean;
class C_JOB C_EDUCATION;
var N_HOME_VAL;
run; quit;

*	Home Value by Job;
proc means data = &data_og. nmiss mean;
class C_JOB;
var N_HOME_VAL;
run; quit;

*	Car Age by Job & Education;
proc means data = &data_og. nmiss mean;
class C_JOB C_EDUCATION;
var N_CAR_AGE;
run; quit;

*	Car Age by Job;
proc means data = &data_og. nmiss mean;
class C_JOB;
var N_CAR_AGE;
run; quit;

*	Handle missing data;
data &data_imp.;
	set &data_imp.;
	
	*	Age;
	if missing(N_AGE) then N_AGE = 45;
	
	*	Years on Job;
	if missing(N_YOJ) then do;
		if C_JOB = "Home Maker"	or C_JOB = "Student" then N_YOJ = 6;
		else N_YOJ = 11;
	end;
	
	*	Income;
	if missing(N_INCOME) then do;
		if C_JOB = "Clerical" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 25700;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 48300;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 35000;
			else N_INCOME = 33900;
		end;
		if C_JOB = "Doctor" then do;
			if C_EDUCATION = "Phd" 				then N_INCOME = 128700;
			else N_INCOME = 128700;
		end;
		if C_JOB = "Home Maker" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 5100;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 15100;
			if C_EDUCATION = "Masters"			then N_INCOME = 15500;
			if C_EDUCATION = "PhD"				then N_INCOME = 25300;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 7700;
			else N_INCOME = 12100;
		end;
		if C_JOB = "Lawyer" then do;
			if C_EDUCATION = "Masters"			then N_INCOME = 85500;
			if C_EDUCATION = "PhD"				then N_INCOME = 119100;
			else N_INCOME = 88300;
		end;
		if C_JOB = "Manager" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 49200;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 77400;
			if C_EDUCATION = "Masters"			then N_INCOME = 90800;
			if C_EDUCATION = "PhD"				then N_INCOME = 141600;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 59500;
			else N_INCOME = 87500;
		end;
		if C_JOB = "Professional" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 34500;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 77000;
			if C_EDUCATION = "Masters"			then N_INCOME = 89100;
			if C_EDUCATION = "PhD"				then N_INCOME = 133500;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 59300;
			else N_INCOME = 76600;
		end;
		if C_JOB = "Student" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 4500;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 9200;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 6400;
			else N_INCOME = 6300;
		end;
		if C_JOB = "z_Blue Collar" then do;
			if C_EDUCATION = "< High School" 	then N_INCOME = 40800;
			if C_EDUCATION = "Bachelors" 		then N_INCOME = 77500;
			if C_EDUCATION = "Masters"			then N_INCOME = 66700;
			if C_EDUCATION = "z_High School" 	then N_INCOME = 55000;
			else N_INCOME = 59000;
		end;
		else N_INCOME = 61900;
	end;
	
	*	Home Value;
	if missing(N_HOME_VAL) then do;
		if C_JOB = "Clerical" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 103100;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 144800;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 119300;
			else N_HOME_VAL = 117600;
		end;
		if C_JOB = "Doctor" then do;
			if C_EDUCATION = "Phd" 				then N_HOME_VAL = 240300;
			else N_HOME_VAL = 240300;
		end;
		if C_JOB = "Home Maker" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 65600;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 102200;
			if C_EDUCATION = "Masters"			then N_HOME_VAL = 79800;
			if C_EDUCATION = "PhD"				then N_HOME_VAL = 112800;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 79500;
			else N_HOME_VAL = 86900;
		end;
		if C_JOB = "Lawyer" then do;
			if C_EDUCATION = "Masters"			then N_HOME_VAL = 198000;
			if C_EDUCATION = "PhD"				then N_HOME_VAL = 224700;
			else N_HOME_VAL = 200200;
		end;
		if C_JOB = "Manager" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 120900;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 190500;
			if C_EDUCATION = "Masters"			then N_HOME_VAL = 198500;
			if C_EDUCATION = "PhD"				then N_HOME_VAL = 273100;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 153700;
			else N_HOME_VAL = 199300;
		end;
		if C_JOB = "Professional" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 132900;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 190600;
			if C_EDUCATION = "Masters"			then N_HOME_VAL = 212600;
			if C_EDUCATION = "PhD"				then N_HOME_VAL = 290600;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 163800;
			else N_HOME_VAL = 190700;
		end;
		if C_JOB = "Student" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 16100;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 12300;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 16100;
			else N_HOME_VAL = 15400;
		end;
		if C_JOB = "z_Blue Collar" then do;
			if C_EDUCATION = "< High School" 	then N_HOME_VAL = 130700;
			if C_EDUCATION = "Bachelors" 		then N_HOME_VAL = 181600;
			if C_EDUCATION = "Masters"			then N_HOME_VAL = 85500;
			if C_EDUCATION = "z_High School" 	then N_HOME_VAL = 155200;
			else N_HOME_VAL = 157800;
		end;
		else N_HOME_VAL = 154900;
	end;

	*	Car Age;
	if missing(N_CAR_AGE) then do;
		if C_JOB = "Clerical" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 3;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 4;
			else N_CAR_AGE = 5;
		end;
		if C_JOB = "Doctor" then do;
			if C_EDUCATION = "Phd" 				then N_CAR_AGE = 14;
			else N_CAR_AGE = 14;
		end;
		if C_JOB = "Home Maker" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 3;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "Masters"			then N_CAR_AGE = 15;
			if C_EDUCATION = "PhD"				then N_CAR_AGE = 13;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 4;
			else N_CAR_AGE = 8;
		end;
		if C_JOB = "Lawyer" then do;
			if C_EDUCATION = "Masters"			then N_CAR_AGE = 14;
			if C_EDUCATION = "PhD"				then N_CAR_AGE = 14;
			else N_CAR_AGE = 14;
		end;
		if C_JOB = "Manager" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 2;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "Masters"			then N_CAR_AGE = 14;
			if C_EDUCATION = "PhD"				then N_CAR_AGE = 14;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 5;
			else N_CAR_AGE = 10;
		end;
		if C_JOB = "Professional" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 6;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "Masters"			then N_CAR_AGE = 14;
			if C_EDUCATION = "PhD"				then N_CAR_AGE = 17;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 5;
			else N_CAR_AGE = 9;
		end;
		if C_JOB = "Student" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 4;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 5;
			else N_CAR_AGE = 5;
		end;
		if C_JOB = "z_Blue Collar" then do;
			if C_EDUCATION = "< High School" 	then N_CAR_AGE = 3;
			if C_EDUCATION = "Bachelors" 		then N_CAR_AGE = 9;
			if C_EDUCATION = "Masters"			then N_CAR_AGE = 14;
			if C_EDUCATION = "z_High School" 	then N_CAR_AGE = 5;
			else N_CAR_AGE = 6;
		end;
		else N_CAR_AGE = 8;
	end;
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
		if name = "&response." then delete;
			else if name = "&key." then delete;
			else if name = "TARGET_AMT" then delete;
		if name =: "MF" then delete; 
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

*	Transform variables to log;
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

*	C_JOB has missing values;
*	Examine levels with PROC MEANS;
proc means data = &data_og. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
	class C_JOB;
	var N_INCOME;
run; quit;

*	Rename categorical levels as necessary;
*	Remove spaces;
data &data_cat.;
	set &data_cat.;
	if C_EDUCATION = "<High School" 			then C_EDUCATION = "LT_HS";
	if C_EDUCATION = "z_High School" 			then C_EDUCATION = "z_HS";
	if C_JOB = "z_Blue Collar" 					then C_JOB = "z_BC";
	if C_JOB = "Home Maker" 					then C_JOB = "Home_Maker";
	if C_CAR_TYPE = "Panel Truck" 				then C_CAR_TYPE = "Panel_Truck";
	if C_CAR_TYPE = "Sports Car" 				then C_CAR_TYPE = "Sports_Car";
	if C_RED_CAR = "no" 						then C_RED_CAR = "No";
	if C_RED_CAR = "yes" 						then C_RED_CAR = "Yes";
	if C_URBANICITY = "Highly Urban/ Urban" 	then C_URBANICITY = "Urban";
	if C_URBANICITY = "z_Highly Rural/ Rural" 	then C_URBANICITY = "z_Rural";
run; quit;

*	Impute missing categorical variables;
data &data_cat.;
	set &data_cat.;
		MF_JOB = missing(C_JOB);
		if missing(C_JOB) then do;
			if N_INCOME > 100000 then C_JOB = "Doctor";	
			else if N_INCOME > 80000 then C_JOB = "Lawyer";
			else C_JOB = "BC";
		end;
run; quit;		
		
*	Create remaining dummy variables;
data &data_cat.;
	set &data_cat.;	
		N_AGE_Risk_Yes	=	(N_AGE <= 30 | N_AGE >= 60);
		N_AGE_Risk_No	=	(N_AGE_Risk_Yes = 0);
		
		N_BLUEBOOK_Hi	=	(N_BLUEBOOK >= 27000);
		N_BLUEBOOK_Lo	=	(N_BLUEBOOK_Hi = 0);
		
		N_CLM_FREQ_No	=	(N_CLM_FREQ = 0);
		N_CLM_FREQ_Yes	=	(N_CLM_FREQ > 0);
		N_CLM_FREQ_Hi	=	(N_CLM_FREQ >= 2);
		N_CLM_FREQ_Lo	=	(N_CLM_FREQ_Hi = 0);

		N_DEGREE		=	(C_EDUCATION_Bachelors = 1 or
							C_EDUCATION_Masters = 1 or
							C_EDUCATION_PhD = 1);

		N_HOMEKIDS_No	=	(N_HOMEKIDS = 0);
		N_HOMEKIDS_Yes	=	(N_HOMEKIDS > 0);
		
		N_INCOME_No 	= 	(N_INCOME = 0);
		N_INCOME_Yes 	= 	(N_INCOME > 0);
		N_INCOME_Hi		=	(N_INCOME >= 85000);
		N_INCOME_Lo		=	(N_INCOME_Hi = 0);
		
		N_KIDSDRIV_No	=	(N_KIDSDRIV = 0);
		N_KIDSDRIV_Yes	=	(N_KIDSDRIV > 0);
		
		N_MVR_PTS_No	=	(N_MVR_PTS = 0);
		N_MVR_PTS_Yes	=	(N_MVR_PTS > 0);
		N_MVR_PTS_Hi	=	(N_MVR_PTS >= 4);
		N_MVR_PTS_Lo	=	(N_MVR_PTS_Hi = 0);
		
		N_OLDCLAIM_No	=	(N_OLDCLAIM = 0);
		N_OLDCLAIM_Yes	=	(N_OLDCLAIM > 0);
		N_OLDCLAIM_Hi	=	(N_OLDCLAIM >= 9500);
		N_OLDCLAIM_Lo	=	(N_OLDCLAIM_Hi = 0);
		
		N_RENTER_Yes	=	(N_HOME_VAL <= 14400);
		N_RENTER_No		=	(N_RENTER_Yes = 0);
		
		N_TRAVTIME_Hi	=	(N_TRAVTIME >= 50);
		N_TRAVTIME_Lo	=	(N_TRAVTIME_Hi = 0);
run; quit;

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
	set &data_cat.;
run; quit;

*	Drop character variables;
data &data_model.;
	set &data_model.;
	drop where type _CHARACTER_;
run; quit;

proc contents data = &data_model.;
run; quit;


**********************************************************************;
**********************************************************************;
*	FIN;
**********************************************************************;
**********************************************************************;
