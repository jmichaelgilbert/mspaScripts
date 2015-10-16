**********************************************************************;
*	411-DL | Unit_01_Scoring;
*	Last updated: 2015-10-15 by MJG;
**********************************************************************;

*	Generic macros to help conduct EDA on any data set;
*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

*	Shorten data name, save to work library;
data MB_Test;
	set mydata.moneyball_test;
run; quit;

**********************************************************************;
**********************************************************************;
*	SAS Macros;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Globals;
**********************************************************************;
%let data_og = MB_Test;
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
%let response = P_TARGET_WINS;	*Response Variable;

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
*	Predict Response;
**********************************************************************;

data MB_Scored;
	set &data_trans.;
	P_TARGET_WINS =
	((-4.97734)				+
	(TEAM_FIELDING_E		*	(-0.00080028))	+
	(TEAM_BASERUN_SB_T99	*	(0.00072553))	+
	(TEAM_BATTING_2B_T75	*	(-0.00155))		+
	(TEAM_BATTING_3B_T90	*	(0.00148))		+
	(TEAM_BATTING_BB_T99	*	(0.00025813))	+
	(TEAM_FIELDING_DP_T99	*	(-0.00153))		+
	(TEAM_PITCHING_HR_T75	*	(0.00111))		+
	(TEAM_BATTING_H_T99_ln	*	(1.33208))		+
	(MF_TEAM_BASERUN_SB		*	(0.20190))		+
	(MF_TEAM_FIELDING_DP	*	(0.12910)));
run; quit;

*	Truncate;
data MB_Twist;
	set MB_Scored (keep = INDEX P_TARGET_WINS);
	P_TARGET_WINS = max(min(P_TARGET_WINS, log(120)), log(30));
run; quit;

*	Transform from log(response) to e**response;
data MB_Twist;
	set MB_Twist;
	P_TARGET_WINS = CONSTANT('E')**P_TARGET_WINS;
run; quit;

*	Round off as teams cannot have 1.5 wins;
data MB_Twist;
	set MB_Twist;
	P_TARGET_WINS = ROUND(P_TARGET_WINS, 1);
run; quit;

*	Verify the results;
proc means data = MB_Twist NOLABELS
    NMISS N MEAN MODE STD SKEW
    P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

proc print data = MB_Twist;
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;