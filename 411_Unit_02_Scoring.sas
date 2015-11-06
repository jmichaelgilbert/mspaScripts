**********************************************************************;
*	411-DL | Unit_02;
*	Last updated: 2015-11-04 by MJG;
**********************************************************************;

*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = INS;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.logit_insurance_test;
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
%let data_og = INS;

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

*	Impute using PROC MI to replace missing values in current variables;
*	Be sure to create missing flags (MF_) first;

********************;
*	PROC MI;
********************;

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
proc means data = &data_imp. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

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
*	Models;
**********************************************************************;
**********************************************************************;


**********************************************************************;
*	Model 7;
**********************************************************************;

data &data_model._M7 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS		=
	-3.9144									+
	(C_CAR_TYPE_Minivan		*	-0.4557)	+
	(C_CAR_TYPE_Pickup		*	0.1411)		+
	(C_CAR_TYPE_Sports_Car	*	0.5662)		+
	(C_CAR_TYPE_z_SUV		*	0.3134)		+
	(C_CAR_USE_Commercial	*	0.6907)		+
	(C_EDUCATION_z_HS		*	0.3357)		+
	(C_EDUCATION_LT_HS		*	0.2826)		+
	(C_JOB_z_BC				*	0.3884)		+
	(C_JOB_Clerical			*	0.3761)		+
	(C_JOB_Student			*	0.086)		+
	(C_MSTATUS_z_No			*	0.4881)		+
	(C_PARENT1_Yes			*	0.3512)		+
	(C_REVOKED_Yes			*	0.9072)		+
	(C_URBANICITY_Urban		*	2.3519)		+
	(N_AGE					*	-0.00317)	+
	(N_CAR_AGE				*	0.000845)	+
	(N_CLM_FREQ				*	0.1984)		+
	(N_HOME_VAL_T75			*	-0.00000148)+
	(N_HOMEKIDS				*	0.047)		+
	(N_INCOME_T90			*	-0.00000838)+
	(N_KIDSDRIV				*	0.3745)		+
	(N_MVR_PTS				*	0.1165)		+
	(N_OLDCLAIM				*	-0.00001)	+
	(N_TIF					*	-0.0546)	+
	(N_TRAVTIME				*	0.0149)		+
	(N_YOJ					*	-0.0124)	+
	(MF_JOB					*	0.1456)		+
	(MF_N_AGE				*	2.1264)		+
	(MF_N_CAR_AGE			*	0.1449)		+
	(MF_N_HOME_VAL			*	-0.035)		+
	(MF_N_INCOME			*	0.00677)	+
	(MF_N_YOJ				*	0.0628)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;

	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 8;
**********************************************************************;

data &data_model._M8 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-3.9023 								+ 
	(C_CAR_TYPE_Minivan		*	-0.4572)	+
	(C_CAR_TYPE_Pickup		*	0.1405)		+
	(C_CAR_TYPE_Sports_Car	*	0.5552)		+
	(C_CAR_TYPE_z_SUV		*	0.3042)		+
	(C_CAR_USE_Commercial	*	0.6981)		+
	(C_EDUCATION_z_HS		*	0.2341)		+
	(C_EDUCATION_LT_HS		*	0.177)		+
	(C_EDUCATION_Bachelors	*	-0.103)		+
	(C_JOB_z_BC				*	0.4477)		+
	(C_JOB_Clerical			*	0.4479)		+
	(C_JOB_Home_Maker		*	0.1568)		+
	(C_JOB_Student			*	0.1826)		+
	(C_MSTATUS_z_No			*	0.4937)		+
	(C_PARENT1_Yes			*	0.3565)		+
	(C_REVOKED_Yes			*	0.909)		+
	(C_URBANICITY_Urban		*	2.3576)		+
	(N_AGE					*	-0.00362)	+
	(N_CAR_AGE				*	-0.00162)	+
	(N_CLM_FREQ				*	0.1989)		+
	(N_HOME_VAL_T75			*	-0.00000144)+
	(N_HOMEKIDS				*	0.0435)		+
	(N_INCOME_T90			*	-0.00000803)+
	(N_KIDSDRIV				*	0.3755)		+
	(N_MVR_PTS				*	0.117)		+
	(N_OLDCLAIM				*	-0.00001)	+
	(N_TIF					*	-0.0546)	+
	(N_TRAVTIME				*	0.0149)		+
	(N_YOJ					*	-0.00976)	+
	(MF_JOB					*	0.1035)		+
	(MF_N_AGE				*	2.1246)		+
	(MF_N_CAR_AGE			*	0.1427)		+
	(MF_N_HOME_VAL			*	-0.0359)	+
	(MF_N_INCOME			*	0.00865)	+
	(MF_N_YOJ				*	0.0663)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 9;
**********************************************************************;

data &data_model._M9 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-5.7362									+	
	(C_CAR_TYPE_Pickup		*	0.4325)		+
	(C_CAR_TYPE_Sports_Car	*	0.8345)		+
	(C_CAR_TYPE_z_SUV		*	0.6303)		+
	(C_CAR_USE_Commercial	*	0.8456)		+
	(C_EDUCATION_z_HS		*	0.4583)		+
	(C_EDUCATION_LT_HS		*	0.5223)		+
	(C_JOB_z_BC				*	0.4065)		+
	(C_JOB_Clerical			*	0.631)		+
	(C_JOB_Home_Maker		*	0.3525)		+
	(C_JOB_Student			*	0.2153)		+
	(C_MSTATUS_z_No			*	0.5577)		+
	(C_PARENT1_Yes			*	0.1682)		+
	(C_REVOKED_Yes			*	0.9771)		+
	(C_URBANICITY_Urban		*	2.3246)		+
	(N_AGE_Risk_Yes			*	0.6069)		+
	(N_CLM_FREQ_Yes			*	0.7553)		+
	(N_RENTER_Yes			*	0.3256)		+
	(N_HOMEKIDS_Yes			*	0.1885)		+
	(N_KIDSDRIV_Yes			*	0.6315)		+
	(N_INCOME_No			*	0.7662)		+
	(N_MVR_PTS_Yes			*	0.3122)		+
	(N_CAR_AGE				*	-0.00545)	+
	(N_OLDCLAIM				*	-0.00002)	+
	(N_TIF					*	-0.0546)	+
	(N_TRAVTIME				*	0.0153)		+
	(N_YOJ					*	0.00682)	+
	(MF_JOB					*	0.1504)		+
	(MF_N_AGE				*	2.4206)		+
	(MF_N_CAR_AGE			*	0.1814)		+
	(MF_N_HOME_VAL			*	0.0757)		+
	(MF_N_INCOME			*	0.0208)		+
	(MF_N_YOJ				*	0.0722)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 10;
**********************************************************************;

data &data_model._M10 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-5.4118									+	
	(C_CAR_TYPE_Minivan		*	-0.4217)	+
	(C_CAR_TYPE_Pickup		*	0.2303)		+
	(C_CAR_TYPE_Sports_Car	*	0.579)		+
	(C_CAR_TYPE_z_SUV		*	0.3759)		+
	(C_CAR_USE_Commercial	*	0.6973)		+
	(C_EDUCATION_z_HS		*	0.4984)		+
	(C_EDUCATION_LT_HS		*	0.5155)		+
	(C_JOB_z_BC				*	0.4384)		+
	(C_JOB_Clerical			*	0.5806)		+
	(C_JOB_Student			*	0.1334)		+
	(C_MSTATUS_z_No			*	0.5596)		+
	(C_PARENT1_Yes			*	0.1675)		+
	(C_REVOKED_Yes			*	0.9688)		+
	(C_URBANICITY_Urban		*	2.3165)		+
	(N_AGE_Risk_Yes			*	0.6231)		+
	(N_CLM_FREQ_Yes			*	0.7512)		+
	(N_RENTER_Yes			*	0.3194)		+
	(N_HOMEKIDS_Yes			*	0.2005)		+
	(N_KIDSDRIV_Yes			*	0.6288)		+
	(N_INCOME_No			*	0.9439)		+
	(N_MVR_PTS_Yes			*	0.308)		+
	(N_CAR_AGE				*	-0.00609)	+
	(N_OLDCLAIM				*	-0.00002)	+
	(N_TIF					*	-0.0543)	+
	(N_TRAVTIME				*	0.0155)		+
	(N_YOJ					*	0.00576)	+
	(MF_JOB					*	0.0606)		+
	(MF_N_AGE				*	2.3347)		+
	(MF_N_CAR_AGE			*	0.1823)		+
	(MF_N_HOME_VAL			*	0.0676)		+
	(MF_N_INCOME			*	0.0429)		+
	(MF_N_YOJ				*	0.0662)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 11;
**********************************************************************;

data &data_model._M11 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-5.4632									+	
	(C_CAR_TYPE_Minivan		*	-0.4291)	+
	(C_CAR_TYPE_Pickup		*	0.2197)		+
	(C_CAR_TYPE_Sports_Car	*	0.5459)		+
	(C_CAR_TYPE_z_SUV		*	0.3454)		+
	(C_CAR_USE_Commercial	*	0.697)		+
	(C_EDUCATION_z_HS		*	0.4534)		+
	(C_EDUCATION_LT_HS		*	0.465)		+
	(C_EDUCATION_Bachelors	*	-0.0109)	+
	(C_JOB_z_BC				*	0.5135)		+
	(C_JOB_Clerical			*	0.6654)		+
	(C_JOB_Home_Maker		*	0.3701)		+
	(C_JOB_Student			*	0.2963)		+
	(C_MSTATUS_z_No			*	0.5572)		+
	(C_PARENT1_Yes			*	0.1739)		+
	(C_REVOKED_Yes			*	0.9752)		+
	(C_URBANICITY_Urban		*	2.3277)		+
	(N_AGE_Risk_Yes			*	0.6253)		+
	(N_CLM_FREQ_Yes			*	0.7498)		+
	(N_RENTER_Yes			*	0.3262)		+
	(N_HOMEKIDS_Yes			*	0.1917)		+
	(N_KIDSDRIV_Yes			*	0.6314)		+
	(N_INCOME_No			*	0.7826)		+
	(N_MVR_PTS_Yes			*	0.3107)		+
	(N_CAR_AGE				*	-0.00532)	+
	(N_OLDCLAIM				*	-0.00002)	+
	(N_TIF					*	-0.0543)	+
	(N_TRAVTIME				*	0.0154)		+
	(N_YOJ					*	0.00743)	+
	(MF_JOB					*	0.0801)		+
	(MF_N_AGE				*	2.3175)		+
	(MF_N_CAR_AGE			*	0.1723)		+
	(MF_N_HOME_VAL			*	0.0699)		+
	(MF_N_INCOME			*	0.0227)		+
	(MF_N_YOJ				*	0.0708)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 12;
**********************************************************************;

data &data_model._M12 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-1.6432									+	
	(C_CAR_TYPE_Minivan		*	-0.6951)	+
	(C_CAR_TYPE_Sports_Car	*	0.2841)		+
	(C_CAR_USE_Private		*	-0.6154)	+
	(C_EDUCATION_Bachelors	*	-0.4242)	+
	(C_EDUCATION_Masters	*	-0.37)		+
	(C_EDUCATION_PhD		*	-0.4446)	+
	(C_JOB_z_BC				*	0.262)		+
	(C_JOB_Clerical			*	0.3221)		+
	(C_JOB_Doctor			*	-0.2387)	+
	(C_JOB_Manager			*	-0.6919)	+
	(C_MSTATUS_Yes			*	-0.6243)	+
	(C_REVOKED_Yes			*	0.874)		+
	(C_URBANICITY_Urban		*	2.3466)		+
	(N_CLM_FREQ_No			*	-0.5323)	+
	(N_INCOME_No			*	0.7074)		+
	(N_INCOME_Hi			*	-0.495)		+
	(N_OLDCLAIM_Hi			*	-0.3795)	+
	(N_AGE_Risk_Yes			*	0.6115)		+
	(N_HOMEKIDS_No			*	-0.2741)	+
	(N_AGE					*	-0.00207)	+
	(N_TRAVTIME_T99			*	0.0154)		+
	(N_HOME_VAL_ln			*	-0.027)		+
	(N_KIDSDRIV_ln			*	0.7348)		+
	(N_MVR_PTS_ln			*	0.2533)		+
	(N_TIF_ln				*	-0.3233)	+
	(MF_N_INCOME			*	0.0197)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;

**********************************************************************;
*	Model 13;
**********************************************************************;

data &data_model._M13 (keep = index P_TARGET_FLAG P_TARGET_AMT);
	set &data_model.;

	LOG_ODDS	 	=
	-1.6617									+	
	(C_CAR_TYPE_Minivan		*	-0.6954)	+
	(C_CAR_TYPE_Sports_Car	*	0.281)		+
	(C_CAR_USE_Private		*	-0.6295)	+
	(C_EDUCATION_Bachelors	*	-0.4257)	+
	(C_EDUCATION_Masters	*	-0.3533)	+
	(C_EDUCATION_PhD		*	-0.4339)	+
	(C_JOB_z_BC				*	0.2624)		+
	(C_JOB_Clerical			*	0.3307)		+
	(C_JOB_Doctor			*	-0.2142)	+
	(C_JOB_Manager			*	-0.6952)	+
	(C_MSTATUS_Yes			*	-0.6217)	+
	(C_REVOKED_Yes			*	0.876)		+
	(C_URBANICITY_Urban		*	2.349)		+
	(N_CLM_FREQ_No			*	-0.5348)	+
	(N_INCOME_No			*	0.6967)		+
	(N_INCOME_Hi			*	-0.4935)	+
	(N_OLDCLAIM_Hi			*	-0.3836)	+
	(N_AGE_Risk_Yes			*	0.6141)		+
	(N_HOMEKIDS_No			*	-0.2713)	+
	(N_AGE					*	-0.00202)	+
	(N_TRAVTIME_T99			*	0.0155)		+
	(N_HOME_VAL_ln			*	-0.0273)	+
	(N_KIDSDRIV_ln			*	0.7371)		+
	(N_MVR_PTS_ln			*	0.2523)		+
	(N_TIF_ln				*	-0.3227)	+
	(MF_JOB					*	-0.0622)	+
	(MF_N_AGE				*	2.3363)		+
	(MF_N_CAR_AGE			*	0.1642)		+
	(MF_N_HOME_VAL			*	0.0448)		+
	(MF_N_INCOME			*	0.0117)		+
	(MF_N_YOJ				*	0.0618)
	;

	P_TARGET_AMT	=
	3258.00804								+
	(C_EDUCATION_z_HS		*	-584.81766)	+
	(C_CAR_USE_Commercial	*	187.48512)	+
	(C_CAR_TYPE_Panel_Truck	*	-175.83876)	+
	(C_JOB_Manager			*	-1022.07127)+
	(C_JOB_Professional		*	396.82587)	+
	(C_MSTATUS_Yes			*	-932.10996)	+
	(C_REVOKED_Yes			*	-1082.74818)+
	(C_SEX_z_F				*	-527.56370)	+
	(N_CAR_AGE_T90_sq		*	-5.00398)	+
	(N_HOME_VAL_T75_rt		*	1.73686)	+
	(N_MVR_PTS_T90_sq		*	30.71701)	+
	(N_BLUEBOOK_T99_rt		*	27.61087)	+
	(N_OLDCLAIM_T99_sq		*	0.0000007383869)	+
	(MF_N_HOME_VAL			*	243.11383)
	;
	
	ODDS			= 	exp(LOG_ODDS);
	P_TARGET_FLAG 	= 	((ODDS) / (1 + ODDS));

run; quit;


**********************************************************************;
**********************************************************************;
*	FIN;
**********************************************************************;
**********************************************************************;


