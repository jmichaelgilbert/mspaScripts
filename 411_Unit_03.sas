**********************************************************************;
*	411-DL | Unit_03;
*	Last updated: 2015-12-01 by MJG;
**********************************************************************;

*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = WINO;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.wine;
run; quit;

*	Print head and test connection;
proc print data = &data_og. (obs = 20);
run; quit;


**********************************************************************;
**********************************************************************;
*	Split data for cross-validation;
**********************************************************************;
**********************************************************************;


*	Split the data create variables and flags as necessary;
data &data_og.;
	set &data_og.;
	U = uniform(666);
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
%let data_og = WINO;

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
%let corr 			= &data_og._corr;
%let varname 		= name;
%let key 			= INDEX;				*Primary, foreign, or other key;
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
*	Macro for qq plots;
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

**********************************************************************;
*	Rename variables as necessary;
**********************************************************************;

*	Rename variables due to length (issues w/ determining var in AVS);
data &data_og.;
	set &data_og.
		(rename = 
			(TotalSulfurDioxide = TSD
			FreeSulfurDioxide 	= FSD
			ResidualSugar 		= RS
			VolatileAcidity 	= VA));
run; quit;

*	View the results;
proc contents data = &data_og.;
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

*	Re-create contents data set with new variable names;
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
				call execute('%histogram('||name||')');
				call execute('%boxplot('||name||')');
	end;
run; quit;

*	Response variable;
data _null_;
	call execute('%qq('||"&response."||')');				
	call execute('%histogram('||"&response."||')');
	call execute('%boxplot('||"&response."||')');
run; quit;

**********************************************************************;
*	Additional Data Prep & Exploration;
**********************************************************************;

*	Create two new variables for later use, but must be done before
	PROC MI since it will overwrite / replace missing;
data &data_og.;
	set &data_og.;
	&response._FLAG	= (&response. > 0);
	&response._AMT 	= (&response. - 1);
	if &response._FLAG = 0 then &response._AMT = .;
run; quit;

*	View the results;
proc print data = &data_og. (obs = 20);
	var &response.:;
run; quit;

*	Explore relationships of those who purchased wine against
	two variables with highest PPC;
*	Will be useful when creating dummy variables in Final Cleanup;
proc freq data = &data_og.;
	table N_STARS * TARGET_FLAG / missing;
run; quit;

proc freq data = &data_og.;
	table N_LabelAppeal * TARGET_FLAG / missing;
run; quit;

*	Explore underlying traits of those who did buy wine, how many
	cases they purchased;
proc means data = &data_og. nmiss mean median min max;
	class TARGET_FLAG TARGET;
	var N_:;
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

proc contents data = &data_og.;
run; quit;

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
*	Note: do not specify minimum = 0 option since data are normalized 
	and some variables correctly take on negative values;
proc mi data = &data_imp. seed = 666;
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
*	Final Cleanup;
**********************************************************************;
**********************************************************************;


*	Create the data set;
data &data_model.;
	set &data_trans.;
run; quit;

*	Create remaining dummy variables;
data &data_model.;
	set &data_model.;
		
		N_STARS_0		=	(0.0 <= N_STARS < 0.5);
		N_STARS_1		=	(0.5 <= N_STARS < 1.5);
		N_STARS_2		=	(1.5 <= N_STARS < 2.5);
		N_STARS_3		=	(2.5 <= N_STARS < 3.5);
		N_STARS_4		=	(3.5 <= N_STARS <= 4.0);
		N_STARS_GTE2	=	(1.5 <=	N_STARS <= 4.0);
		N_STARS_GTE3	=	(2.5 <= N_STARS <= 4.0);
		
		N_LabelAppeal_1	=	(-2.0 <= N_LabelAppeal < -1.5);
		N_LabelAppeal_2	=	(-1.5 <= N_LabelAppeal < -0.5);
		N_LabelAppeal_3	=	(-0.5 <= N_LabelAppeal < 0.5);
		N_LabelAppeal_4	=	(0.5 <= N_LabelAppeal < 1.5);
		N_LabelAppeal_5	=	(1.5 <= N_LabelAppeal <= 2.0);
		N_LA_GTE3		=	(-0.5 <= N_LabelAppeal <= 2.0);
		N_LA_GTE4		=	(0.5 <= N_LabelAppeal <= 2.0);

run; quit;

*	View the results;
proc print data = &data_model. (obs = 20);
	var 
		N_STARS
		N_STARS_0
		N_STARS_1
		N_STARS_2
		N_STARS_3
		N_STARS_4
		N_STARS_GTE2
		N_STARS_GTE3
	
		N_LabelAppeal
		N_LabelAppeal_1
		N_LabelAppeal_2
		N_LabelAppeal_3
		N_LabelAppeal_4
		N_LabelAppeal_5
		N_LA_GTE3
		N_LA_GTE4
		;
run; quit;

*	Check the contents;
proc contents data = &data_model.;
run; quit;


**********************************************************************;
**********************************************************************;
*	Models;
**********************************************************************;
**********************************************************************;


*	Create modelfile;
data &modelfile.;
	set &data_model.;
run; quit;

**********************************************************************;
*	OLS Model;
**********************************************************************;

*	AVS Stepwise;
proc reg data = &modelfile. plots = diagnostics;
model &response. = 
	N_:
	MF_:
	/ selection = stepwise slentry = 0.11 slstay = 0.11 vif;
output out = &modelfile. predicted = yhat_ols residual = res_ols;
run; quit;

*	Chosen model;
proc reg data = &modelfile. plots = diagnostics;
model &response. =
		
	MF_N_Alcohol
	MF_N_pH
	MF_N_RS
	MF_N_STARS
	
	N_AcidIndex_T99_sq
	N_Alcohol_T99
	N_Chlorides_T75_ln
	N_CitricAcid_T95_ln
	N_Density_T75_ln
	N_FSD_T75_sq
	N_LabelAppeal
	N_pH_T99_ln
	N_RS_T75_rt
	N_STARS_3
	N_STARS_ln
	N_Sulphates_T95_sq
	N_TSD_T75_ln
	N_VA_T90_rt

	/ selection = rsquare start = 18 stop = 18 mse adjrsq aic bic cp vif;
output out = &modelfile. predicted = yhat_ols residual = res_ols;
run; quit;

**********************************************************************;
*	Logistic Model;
**********************************************************************;

*	AVS Stepwise;
proc logistic data = &modelfile.
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model &response._FLAG(ref = "0") =
	MF_:
	N_:
	/ selection = stepwise slentry = 0.11 slstay = 0.11 rsquare lackfit roceps = 0.1;
output out = &modelfile. predicted = yhat_log;
run; quit;

*	Chosen model;
proc logistic data = &modelfile.
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model &response._FLAG(ref = "0") =

	MF_N_Alcohol
	MF_N_Chlorides
	MF_N_FSD
	MF_N_pH
	MF_N_RS
	MF_N_STARS
	MF_N_Sulphates
	MF_N_TSD

	N_AcidIndex_T99_sq
	N_Alcohol_T95_ln
	N_Chlorides_T75_ln
	N_FSD_T75_rt
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_sq
	N_RS_T75
	N_STARS_3
	N_STARS_GTE2
	N_STARS_T90_ln
	N_Sulphates_T75
	N_TSD_T75_ln
	N_VA_T75_sq

	/ rsquare lackfit roceps = 0.1;
output out = &modelfile. predicted = yhat_log;
run; quit;

*	KS Stats;
proc npar1way data = &modelfile. edf;
	class &response._FLAG;
	var yhat_log;
run; quit;

**********************************************************************;
*	Poisson Model;
**********************************************************************;

*	AVS Stepwise;
proc hpgenselect data = &modelfile.;
model &response. = 	
	MF_:
	N_:	
	/ link = log dist = poisson;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_poi;
run; quit;

*	Chosen model;
proc genmod data = &modelfile. plots = all;
model &response. = 	
	
	MF_N_Alcohol
	MF_N_Chlorides
	MF_N_FSD
	MF_N_pH
	MF_N_STARS
	MF_N_TSD

	N_AcidIndex_T99_sq
	N_Alcohol_T75_sq
	N_Chlorides_T75
	N_Density_T75_ln
	N_FSD_T75_ln
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_sq
	N_STARS_3
	N_STARS_4
	N_STARS_GTE2
	N_STARS_ln
	N_TSD_T75_ln
	N_VA_T75
	
	/ link = log dist = poisson;
output out = &modelfile. predicted = yhat_poi;
run; quit;

**********************************************************************;
*	Zero Inflated Poisson Model;
**********************************************************************;

*	AVS Stepwise;
proc hpgenselect data = &modelfile.;
model &response. = 	
	MF_:
	N_:	
	/ link = log dist = zip;
zeromodel
	MF_:
	N_:
	/ link = logit;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_zip pzero = yhat_zip_zero;
run; quit;

*	Chosen model;
proc genmod data = &modelfile. plots = all;
model &response. = 	

	MF_N_STARS

	N_AcidIndex_T95_sq
	N_Alcohol_T75_sq
	N_Density_T75_ln
	N_LabelAppeal
	N_LabelAppeal_3
	N_LabelAppeal_4
	N_STARS
	N_STARS_3
	N_STARS_4
	N_VA_T75_ln

	/ link = log dist = zip;
	
zeromodel

	MF_N_Alcohol
	MF_N_pH
	MF_N_STARS
	
	N_AcidIndex_T99_sq
	N_Alcohol_T75_ln
	N_FSD_T75_rt
	N_LabelAppeal
	N_LabelAppeal_3
	N_LabelAppeal_4
	N_pH_T75_sq
	N_RS_T75
	N_STARS_T90_ln
	N_Sulphates_T75_rt
	N_TSD_T75_ln
	N_VA_T75_sq

	/ link = logit;
output out = &modelfile. predicted = yhat_zip pzero = yhat_zip_zero;
run; quit;

**********************************************************************;
*	Hurdle Logistic Poisson Model;
**********************************************************************;

*	AVS Stepwise on TARGET_AMT;
proc hpgenselect data = &modelfile.;
model &response._AMT = 	
	MF_:
	N_:	
	/ link = log dist = poisson;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_hlp_poi;
run; quit;

*	Logistic Model;
proc logistic data = &modelfile.
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model &response._FLAG(ref = "0") =

	MF_N_Alcohol
	MF_N_Chlorides
	MF_N_FSD
	MF_N_pH
	MF_N_RS
	MF_N_STARS
	MF_N_Sulphates
	MF_N_TSD

	N_AcidIndex_T99_sq
	N_Alcohol_T95_ln
	N_Chlorides_T75_ln
	N_FSD_T75_rt
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_sq
	N_RS_T75
	N_STARS_3
	N_STARS_GTE2
	N_STARS_T90_ln
	N_Sulphates_T75
	N_TSD_T75_ln
	N_VA_T75_sq

	/ rsquare lackfit roceps = 0.1;
output out = &modelfile. predicted = yhat_hlp_log;
run; quit;

*	Poisson Model;
proc genmod data = &modelfile. plots = all;
model &response._AMT = 	
	
	MF_N_STARS
	MF_N_Alcohol
	MF_N_FSD
	MF_N_TSD
	
	N_AcidIndex_T95_sq
	N_Alcohol_T75_sq
	N_Density_T75_ln
	N_FSD_T75_ln
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_ln
	N_STARS
	N_STARS_3
	N_STARS_GTE2
	N_TSD_T75_ln
	N_VA_T75_ln
	
	/ link = log dist = poisson;
output out = &modelfile. predicted = yhat_hlp_poi;
run; quit;

*	Convert to prediction;
data &modelfile.;
	set &modelfile.;
	yhat_hlp = yhat_hlp_log * (yhat_hlp_poi + 1);
run; quit;

**********************************************************************;
*	Negative Binomial Model;
**********************************************************************;

*	AVS Stepwise;
proc hpgenselect data = &modelfile.;
model &response. = 	
	MF_:
	N_:	
	/ link = log dist = nb;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_nb;
run; quit;

*	Chosen model;
proc genmod data = &modelfile. plots = all;
model &response. = 	

	MF_N_Alcohol
	MF_N_Chlorides
	MF_N_FSD
	MF_N_pH
	MF_N_STARS
	MF_N_TSD

	N_AcidIndex_T99_sq
	N_Alcohol_T75_sq
	N_Chlorides_T75
	N_Density_T75_ln
	N_FSD_T75_ln
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_sq
	N_STARS_3
	N_STARS_4
	N_STARS_GTE2
	N_STARS_ln
	N_TSD_T75_ln
	N_VA_T75

	/ link = log dist = nb;
output out = &modelfile. predicted = yhat_nb;
run; quit;

**********************************************************************;
*	Zero Inflated Negative Binomial Model;
**********************************************************************;

*	AVS Stepwise;
proc hpgenselect data = &modelfile.;
model &response. = 	
	MF_:
	N_:	
	/ link = log dist = zinb;
zeromodel 
	MF_N:
	N_:
	/ link = logit;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_zinb pzero = yhat_zinb_zero;
run; quit;

*	Chosen model;
proc genmod data = &modelfile. plots = all;
model &response. = 	

	MF_N_STARS

	N_AcidIndex_T95_sq
	N_Alcohol_T75_sq
	N_Density_T75_ln
	N_LabelAppeal
	N_LabelAppeal_3
	N_LabelAppeal_4
	N_STARS
	N_STARS_3
	N_STARS_4
	N_VA_T75_ln

	/ link = log dist = zinb;
	
zeromodel

	MF_N_Alcohol
	MF_N_pH
	MF_N_STARS
	
	N_AcidIndex_T99_sq
	N_Alcohol_T75_ln
	N_FSD_T75_rt
	N_LabelAppeal
	N_LabelAppeal_3
	N_LabelAppeal_4
	N_pH_T75_sq
	N_RS_T75
	N_STARS_T90_ln
	N_Sulphates_T75_rt
	N_TSD_T75_ln
	N_VA_T75_sq

	/ link = logit;
output out = &modelfile. predicted = yhat_zinb pzero = yhat_zinb_zero;
run; quit;

**********************************************************************;
*	Hurdle Logistic Negative Binomial Model;
**********************************************************************;

*	AVS Stepwise on TARGET_AMT;
proc hpgenselect data = &modelfile.;
model &response._AMT = 	
	MF_:
	N_:	
	/ link = log dist = nb;
selection method = stepwise details = all;
id _all_;
output out = &modelfile. predicted = yhat_hlnb_nb;
run; quit;

*	Logistic Model;
proc logistic data = &modelfile.
plot(label) = (roc(ID = prob) effect influence(unpack)) plots(maxpoints = none);
model &response._FLAG(ref = "0") =

	MF_N_Alcohol
	MF_N_Chlorides
	MF_N_FSD
	MF_N_pH
	MF_N_RS
	MF_N_STARS
	MF_N_Sulphates
	MF_N_TSD

	N_AcidIndex_T99_sq
	N_Alcohol_T95_ln
	N_Chlorides_T75_ln
	N_FSD_T75_rt
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_sq
	N_RS_T75
	N_STARS_3
	N_STARS_GTE2
	N_STARS_T90_ln
	N_Sulphates_T75
	N_TSD_T75_ln
	N_VA_T75_sq

	/ rsquare lackfit roceps = 0.1;
output out = &modelfile. predicted = yhat_hlnb_log;
run; quit;

*	Negative Binomial Model;
proc genmod data = &modelfile. plots = all;
model &response._AMT = 	

	MF_N_STARS
	MF_N_Alcohol
	MF_N_FSD
	MF_N_TSD
	
	N_AcidIndex_T95_sq
	N_Alcohol_T75_sq
	N_Density_T75_ln
	N_FSD_T75_ln
	N_LabelAppeal
	N_LabelAppeal_4
	N_LabelAppeal_5
	N_pH_T75_ln
	N_STARS
	N_STARS_3
	N_STARS_GTE2
	N_TSD_T75_ln
	N_VA_T75_ln

	/ link = log dist = nb;
output out = &modelfile. predicted = yhat_hlnb_nb;
run; quit;

*	Convert to prediction;
data &modelfile.;
	set &modelfile.;
	yhat_hlnb = yhat_hlnb_log * (yhat_hlnb_nb + 1);
run; quit;

**********************************************************************;
**********************************************************************;
*	Model Validation;
**********************************************************************;
**********************************************************************;

%let validation = validation;

data &validation.;
	set &modelfile.;
		MSE_ols		=	(abs(&response. - yhat_ols)**2);
		MSE_log		=	(abs(&response._flag - yhat_log)**2);
		MSE_poi		=	(abs(&response. - yhat_poi)**2);
		MSE_zip		=	(abs(&response. - yhat_zip)**2);
		MSE_hlp		=	(abs(&response. - yhat_hlp)**2);
		MSE_nb		=	(abs(&response. - yhat_nb)**2);
		MSE_zinb	=	(abs(&response. - yhat_zinb)**2);
		MSE_hlnb	=	(abs(&response. - yhat_hlnb)**2);
run; quit;

proc means data = &validation. noprint;
	output out = &data_og._mse
		mean(MSE_ols) 	=	MSE_ols
		mean(MSE_log)	=	MSE_log
		mean(MSE_poi)	=	MSE_poi
		mean(MSE_zip)	=	MSE_zip
		mean(MSE_hlp)	=	MSE_hlp
		mean(MSE_nb)	=	MSE_nb
		mean(MSE_zinb)	=	MSE_zinb
		mean(MSE_hlnb)	=	MSE_hlnb
		;
run; quit;

data &data_og._rmse (drop = _TYPE_ _FREQ_);
	set &data_og._mse;
		RMSE_ols	=	sqrt(MSE_ols);
		RMSE_log	=	sqrt(MSE_log);
		RMSE_poi	=	sqrt(MSE_poi);
		RMSE_zip	=	sqrt(MSE_zip);
		RMSE_hlp	=	sqrt(MSE_hlp);
		RMSE_nb		=	sqrt(MSE_nb);
		RMSE_zinb	=	sqrt(MSE_zinb);
		RMSE_hlnb	=	sqrt(MSE_hlnb);
run; quit;

proc print data = &data_og._rmse;
run; quit;

**********************************************************************;
**********************************************************************;
*	FIN;
**********************************************************************;
**********************************************************************;