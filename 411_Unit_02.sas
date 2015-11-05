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
data &data_og.;
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
%let data_og = INS_70;

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
*	Models;
**********************************************************************;
**********************************************************************;


**********************************************************************;
*	Automated Variable Selection;
*	Note: only tested on 100 (full) data set;
**********************************************************************;

***********************************;
*	Model 1.1 @ 100;
*	AVS: Forward Selection;
***********************************;

proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 
	C_:
	N_:
	MF_:
	/ selection = forward slentry = 0.10 rsquare lackfit roceps = 0.1;
	output out = ins_fw_100 predicted = yhat;
run; quit;

***********************************;
*	Model 2.1 @ 100;
*	AVS: Backward Elimination;
***********************************;

proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 
	C_:
	N_:
	MF_:
	/ selection = backward slstay = 0.10 rsquare lackfit roceps = 0.1;
	output out = ins_bw_100 predicted = yhat;
run; quit;

***********************************;
*	Model 3.1 @ 100;
*	AVS: Stepwise Selection;
***********************************;

proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 
	C_:
	N_:
	MF_:
	/ selection = stepwise slentry = 0.10 slstay = 0.10 rsquare lackfit roceps = 0.1;
	output out = ins_sw_100 predicted = yhat;
run; quit;

**********************************************************************;
*	Hand Picked - Yes Class;
**********************************************************************;

***********************************;
*	Model 4.1 @ 100;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
class 	C_CAR_TYPE
		C_CAR_USE
		C_EDUCATION
		C_JOB
		C_MSTATUS
		C_PARENT1
		C_REVOKED
		C_URBANICITY
		/ param = ref;
		
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE
	C_CAR_USE
	C_EDUCATION
	C_JOB
	C_MSTATUS
	C_PARENT1
	C_REVOKED
	C_URBANICITY

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOMEKIDS
	N_HOME_VAL_T75
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	MF_JOB
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_c1_100_class predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_c1_100_class edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_c1_100_class (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_c1_100_class;
run; quit;

***********************************;
*	Model 5.1 @ 100;
***********************************;

*	Model;
proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
class 	C_CAR_TYPE
		C_CAR_USE
		C_EDUCATION
		C_JOB
		C_MSTATUS
		C_PARENT1
		C_REVOKED
		C_URBANICITY
		/ param = ref;
		
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE
	C_CAR_USE
	C_EDUCATION
	C_JOB
	C_MSTATUS
	C_PARENT1
	C_REVOKED
	C_URBANICITY

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOMEKIDS
	N_HOME_VAL_T75
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	MF_JOB
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_c2_100_class predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_c2_100_class edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_c2_100_class (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_c2_100_class;
run; quit;

***********************************;
*	Model 6.1 @ 100;
***********************************;

*	Model;
proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
class 	C_CAR_TYPE
		C_CAR_USE
		C_EDUCATION
		C_JOB
		C_MSTATUS
		C_PARENT1
		C_REVOKED
		C_URBANICITY
		/ param = ref;

model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE
	C_CAR_USE
	C_EDUCATION
	C_JOB
	C_MSTATUS
	C_PARENT1
	C_REVOKED
	C_URBANICITY
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_c3_100_class predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_c3_100_class edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_c3_100_class (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_c3_100_class;
run; quit;

**********************************************************************;
*	Hand Picked - No Class - Abridged;
**********************************************************************;

***********************************;
*	Model 7.1 @ 100;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m7_100 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m7_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m7_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m7_100;
run; quit;

***********************************;
*	Model 7.2 @ 70;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_70_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m7_70 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m7_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m7_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m7_70;
run; quit;

***********************************;
*	Model 7.3 @ 30;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_30_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m7_30 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m7_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m7_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m7_30;
run; quit;

***********************************;
*	Model 8.1 @ 100;
***********************************;

*	Model;
proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m8_100 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m8_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m8_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m8_100;
run; quit;

***********************************;
*	Model 8.2 @ 70;
***********************************;

*	Model;
proc logistic data = INS_70_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m8_70 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m8_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m8_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m8_70;
run; quit;

***********************************;
*	Model 8.3 @ 30;
***********************************;

*	Model;
proc logistic data = INS_30_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE
	N_CAR_AGE
	N_CLM_FREQ
	N_HOME_VAL_T75
	N_HOMEKIDS
	N_INCOME_T90
	N_KIDSDRIV
	N_MVR_PTS
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m8_30 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m8_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m8_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m8_30;
run; quit;

***********************************;
*	Model 9.1 @ 100;
***********************************;

proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m9_100 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m9_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m9_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m9_100;
run; quit;

***********************************;
*	Model 9.2 @ 70;
***********************************;

proc logistic data = INS_70_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m9_70 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m9_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m9_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m9_70;
run; quit;

***********************************;
*	Model 9.3 @ 30;
***********************************;

proc logistic data = INS_30_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student

	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban

	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ

	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m9_30 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m9_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m9_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m9_30;
run; quit;

**********************************************************************;
*	Hand Picked - No Class - Extended Dummy Variables;
**********************************************************************;

***********************************;
*	Model 10.1 @ 100;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial
	
	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m10_100 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m10_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m10_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m10_100;
run; quit;

***********************************;
*	Model 10.2 @ 70;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_70_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial
	
	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m10_70 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m10_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m10_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m10_70;
run; quit;

***********************************;
*	Model 10.3 @ 30;
***********************************;

*	Model;
ods graphics on;
proc logistic data = INS_30_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial
	
	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m10_30 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_m10_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m10_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m10_30;
run; quit;

***********************************;
*	Model 11.1 @ 100;
***********************************;

*	Model;
proc logistic data = INS_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m11_100 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m11_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m11_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m11_100;
run; quit;

***********************************;
*	Model 11.2 @ 70;
***********************************;

*	Model;
proc logistic data = INS_70_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m11_70 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m11_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m11_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m11_70;
run; quit;

***********************************;
*	Model 11.3 @ 30;
***********************************;

*	Model;
proc logistic data = INS_30_Model
plot(label) = (roc(ID=prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Pickup
	C_CAR_TYPE_Sports_Car
	C_CAR_TYPE_z_SUV
	
	C_CAR_USE_Commercial

	C_EDUCATION_z_HS
	C_EDUCATION_LT_HS
	C_EDUCATION_Bachelors

	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Home_Maker
	C_JOB_Student
	
	C_MSTATUS_z_No
	C_PARENT1_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_AGE_Risk_Yes
	N_CLM_FREQ_Yes
	N_RENTER_Yes
	
	N_HOMEKIDS_Yes
	N_KIDSDRIV_Yes
	N_INCOME_No
	N_MVR_PTS_Yes
	
	N_CAR_AGE
	N_OLDCLAIM
	N_TIF
	N_TRAVTIME
	N_YOJ
	
	MF_JOB
	MF_N_AGE
	MF_N_CAR_AGE
	MF_N_HOME_VAL
	MF_N_INCOME
	MF_N_YOJ
	
	/ rsquare lackfit roceps = 0.1;
	output out = ins_m11_30 predicted = P_TARGET_FLAG;
run; quit;

*	KS Stats;
proc npar1way data = ins_m11_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_m11_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_m11_30;
run; quit;

**********************************************************************;
*	Wilck Replica @ 100;
**********************************************************************;

*	Replicate model in sync code based on currently created variables
	That is, DO NOT create new variables to match model;
*	OK, only new variable created = DEGREE and not used in any other model;

ods graphics on;
proc logistic data = INS_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Sports_Car

	C_CAR_USE_Private
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Doctor
	C_JOB_Manager
	
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_CLM_FREQ_No
	N_INCOME_No
	N_INCOME_Hi
	N_OLDCLAIM_Hi
	N_AGE_RISK_Yes
	N_HOMEKIDS_No
	
	N_AGE
	N_DEGREE
	N_TRAVTIME_T99
	
	N_HOME_VAL_ln
	N_KIDSDRIV_ln
	N_MVR_PTS_ln
	N_TIF_ln

	MF_N_INCOME

	/ rsquare lackfit roceps = 0.1;
	output out = ins_wilck_100 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_wilck_100 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_wilck_100 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_wilck_100;
run; quit;

**********************************************************************;
*	Wilck Replica @ 70;
**********************************************************************;

*	Replicate model in sync code based on currently created variables
	That is, DO NOT create new variables to match model;
*	OK, only new variable created = DEGREE and not used in any other model;

ods graphics on;
proc logistic data = INS_70_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Sports_Car

	C_CAR_USE_Private
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Doctor
	C_JOB_Manager
	
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_CLM_FREQ_No
	N_INCOME_No
	N_INCOME_Hi
	N_OLDCLAIM_Hi
	N_AGE_RISK_Yes
	N_HOMEKIDS_No
	
	N_AGE
	N_DEGREE
	N_TRAVTIME_T99
	
	N_HOME_VAL_ln
	N_KIDSDRIV_ln
	N_MVR_PTS_ln
	N_TIF_ln

	MF_N_INCOME

	/ rsquare lackfit roceps = 0.1;
	output out = ins_wilck_70 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_wilck_70 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_wilck_70 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_wilck_70;
run; quit;

**********************************************************************;
*	Wilck Replica @ 30;
**********************************************************************;

*	Replicate model in sync code based on currently created variables
	That is, DO NOT create new variables to match model;
*	OK, only new variable created = DEGREE and not used in any other model;

ods graphics on;
proc logistic data = INS_30_Model
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model TARGET_FLAG(ref = "0") = 

	C_CAR_TYPE_Minivan
	C_CAR_TYPE_Sports_Car

	C_CAR_USE_Private
	
	C_JOB_z_BC
	C_JOB_Clerical
	C_JOB_Doctor
	C_JOB_Manager
	
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_URBANICITY_Urban
	
	N_CLM_FREQ_No
	N_INCOME_No
	N_INCOME_Hi
	N_OLDCLAIM_Hi
	N_AGE_RISK_Yes
	N_HOMEKIDS_No
	
	N_AGE
	N_DEGREE
	N_TRAVTIME_T99
	
	N_HOME_VAL_ln
	N_KIDSDRIV_ln
	N_MVR_PTS_ln
	N_TIF_ln

	MF_N_INCOME

	/ rsquare lackfit roceps = 0.1;
	output out = ins_wilck_30 predicted = P_TARGET_FLAG;
run; quit;
ods graphics off;

*	KS Stats;
proc npar1way data = ins_wilck_30 edf;
	class &response.;
	var P_TARGET_FLAG;
run; quit;

*	Scoring;
data ins_wilck_30 (keep = INDEX TARGET_FLAG P_TARGET_FLAG);
	set ins_wilck_30;
run; quit;


**********************************************************************;
**********************************************************************;
*	FIN;
**********************************************************************;
**********************************************************************;

