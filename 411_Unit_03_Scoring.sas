**********************************************************************;
*	411-DL | Unit_03_Scoring;
*	Last updated: 2015-12-01 by MJG;
**********************************************************************;

*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
*	Declare data set;
**********************************************************************;
%let data_og = WINO_Score;

*	Shorten data set name, save to work library;
data &data_og.;
	set mydata.wine_test;
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
%let data_og = WINO_Score;

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

*	Create modelfile;
data &modelfile.;
	set &data_model.;
run; quit;

**********************************************************************;
**********************************************************************;
*	Scoring;
**********************************************************************;
**********************************************************************;


*	Create scorfile;
data scorefile;
	set &modelfile.;

**********************************************************************;
*	OLS Model;
**********************************************************************;

P_OLS	=					(	16.77228	)	+
	MF_N_Alcohol		*	(	0.07404		)	+
	MF_N_pH				*	(	-0.09173		)	+
	MF_N_RS				*	(	0.07524		)	+
	MF_N_STARS			*	(	-2.29302		)	+
	N_AcidIndex_T99_sq	*	(	-0.01071		)	+
	N_Alcohol_T99		*	(	0.01172		)	+	
	N_Chlorides_T75_ln	*	(	-1.36121		)	+
	N_CitricAcid_T95_ln	*	(	0.04568		)	+	
	N_Density_T75_ln	*	(	-22.29223	)	+
	N_FSD_T75_sq		*	(	0.00003226	)	+
	N_LabelAppeal		*	(	0.41564		)	+	
	N_pH_T99_ln			*	(	-0.10769		)	+
	N_RS_T75_rt			*	(	0.0201		)	+	
	N_STARS_3			*	(	-0.09834		)	+
	N_STARS_ln			*	(	2.2621		)	+		
	N_Sulphates_T95_sq	*	(	-0.02063		)	+
	N_TSD_T75_ln		*	(	0.11841		)	+	
	N_VA_T90_rt			*	(	-0.03253		)
	;

**********************************************************************;
*	Poisson Model;
**********************************************************************;

P_POI	= 					(	6.0083	)	+
	MF_N_Alcohol		*	(	0.0234	)	+
	MF_N_Chlorides		*	(	-0.0043	)	+
	MF_N_FSD			*	(	0.0139	)	+
	MF_N_pH				*	(	-0.038	)	+
	MF_N_STARS			*	(	-1.0599	)	+
	MF_N_TSD			*	(	0.0117	)	+
	N_AcidIndex_T99_sq	*	(	-0.0048	)	+
	N_Alcohol_T75_sq	*	(	0.001	)	+
	N_Chlorides_T75		*	(	-0.3878	)	+
	N_Density_T75_ln	*	(	-6.5578	)	+
	N_FSD_T75_ln		*	(	0.026	)	+
	N_LabelAppeal		*	(	0.1926	)	+
	N_LabelAppeal_4		*	(	-0.055	)	+
	N_LabelAppeal_5		*	(	-0.12	)	+
	N_pH_T75_sq			*	(	-0.0153	)	+
	N_STARS_3			*	(	0.2445	)	+
	N_STARS_4			*	(	0.473	)	+
	N_STARS_GTE2		*	(	0.4898	)	+
	N_STARS_ln			*	(	-0.4666	)	+
	N_TSD_T75_ln		*	(	0.0417	)	+
	N_VA_T75			*	(	-0.1759	)	
	;

P_POI		=	exp(P_POI);

**********************************************************************;
*	Zero Inflated Poisson Model;
**********************************************************************;

*	All;
P_ZIP_ALL	=				(	4.7502	)	+
	MF_N_STARS			*	(	-0.1786	)	+
	N_AcidIndex_T95_sq	*	(	-0.0015	)	+
	N_Alcohol_T75_sq	*	(	0.0014	)	+
	N_Density_T75_ln	*	(	-5.5481	)	+
	N_LabelAppeal		*	(	0.2257	)	+
	N_LabelAppeal_3		*	(	0.0879	)	+
	N_LabelAppeal_4		*	(	0.0514	)	+
	N_STARS				*	(	0.1519	)	+
	N_STARS_3			*	(	-0.0638	)	+
	N_STARS_4			*	(	-0.1243	)	+
	N_VA_T75_ln			*	(	-0.1129	)	
	;
	
*	Zero model;
P_ZIP_ZERO	=				(	-5.2686	)	+
	MF_N_Alcohol		*	(	-0.2303	)	+
	MF_N_pH				*	(	0.4091	)	+
	MF_N_STARS			*	(	5.2724	)	+
	N_AcidIndex_T99_sq	*	(	0.0184	)	+
	N_Alcohol_T75_ln	*	(	1.6547	)	+
	N_FSD_T75_rt		*	(	-0.0821	)	+
	N_LabelAppeal		*	(	1.2847	)	+
	N_LabelAppeal_3		*	(	0.5398	)	+
	N_LabelAppeal_4		*	(	0.3808	)	+
	N_pH_T75_sq			*	(	0.1646	)	+
	N_RS_T75			*	(	-0.0112	)	+
	N_STARS_T90_ln		*	(	-7.1056	)	+
	N_Sulphates_T75_rt	*	(	2.294	)	+
	N_TSD_T75_ln		*	(	-0.3608	)	+
	N_VA_T75_sq			*	(	1.0501	)	
	;

P_ZIP_ALL	=	exp(P_ZIP_ALL);
P_ZIP_ZERO	=	exp(P_ZIP_ZERO) / (1 + exp(P_ZIP_ZERO));
P_ZIP		=	P_ZIP_ALL * (1 - P_ZIP_ZERO);	

**********************************************************************;
*	Hurdle Logistic Poisson Model;
**********************************************************************;

*	Logistic;
P_HLP_LOG	=				(	0.8503	)	+
	MF_N_Alcohol		*	(	0.1875	)	+
	MF_N_Chlorides		*	(	-0.1192	)	+
	MF_N_FSD			*	(	0.0433	)	+
	MF_N_pH				*	(	-0.3267	)	+
	MF_N_RS				*	(	0.05	)	+
	MF_N_STARS			*	(	-4.0208	)	+
	MF_N_Sulphates		*	(	-0.0876	)	+
	MF_N_TSD			*	(	0.015	)	+
	N_AcidIndex_T99_sq	*	(	-0.0181	)	+
	N_Alcohol_T95_ln	*	(	-0.3889	)	+
	N_Chlorides_T75_ln	*	(	-1.4663	)	+
	N_FSD_T75_rt		*	(	0.0645	)	+
	N_LabelAppeal		*	(	-0.9031	)	+
	N_LabelAppeal_4		*	(	0.2168	)	+
	N_LabelAppeal_5		*	(	0.5071	)	+
	N_pH_T75_sq			*	(	-0.1504	)	+
	N_RS_T75			*	(	0.012	)	+
	N_STARS_3			*	(	0.3979	)	+
	N_STARS_GTE2		*	(	0.4445	)	+
	N_STARS_T90_ln		*	(	4.2395	)	+
	N_Sulphates_T75		*	(	-0.7345	)	+
	N_TSD_T75_ln		*	(	0.3302	)	+
	N_VA_T75_sq			*	(	-1.0593	)	
	;

*	Poisson;
P_HLP_POI 	=				(	5.0645	)	+
	MF_N_STARS			*	(	-0.2004	)	+
	MF_N_Alcohol		*	(	0.0056	)	+
	MF_N_FSD			*	(	0.0083	)	+
	MF_N_TSD			*	(	0.0099	)	+
	N_AcidIndex_T95_sq	*	(	-0.0013	)	+
	N_Alcohol_T75_sq	*	(	0.0018	)	+
	N_Density_T75_ln	*	(	-6.6259	)	+
	N_FSD_T75_ln		*	(	0.0127	)	+
	N_LabelAppeal		*	(	0.4052	)	+
	N_LabelAppeal_4		*	(	-0.1727	)	+
	N_LabelAppeal_5		*	(	-0.3861	)	+
	N_pH_T75_ln			*	(	0.1509	)	+
	N_STARS				*	(	0.1126	)	+
	N_STARS_3			*	(	-0.0081	)	+
	N_STARS_GTE2		*	(	0.0398	)	+
	N_TSD_T75_ln		*	(	-0.0089	)	+
	N_VA_T75_ln			*	(	-0.1177	)
	;

*	Conversion;
P_HLP_LOG 	=	exp(P_HLP_LOG) / (1 + exp(P_HLP_LOG));
P_HLP_POI 	=	exp(P_HLP_POI);
P_HLP 		=	P_HLP_LOG * (P_HLP_POI + 1);

**********************************************************************;
*	Negative Binomial Model;
**********************************************************************;

P_NB	=						6.0083		+
	MF_N_Alcohol		*	(	0.0234	)	+
	MF_N_Chlorides		*	(	-0.0043	)	+
	MF_N_FSD			*	(	0.0139	)	+
	MF_N_pH				*	(	-0.038	)	+
	MF_N_STARS			*	(	-1.0599	)	+
	MF_N_TSD			*	(	0.0117	)	+
	N_AcidIndex_T99_sq	*	(	-0.0048	)	+
	N_Alcohol_T75_sq	*	(	0.001	)	+
	N_Chlorides_T75		*	(	-0.3878	)	+
	N_Density_T75_ln	*	(	-6.5578	)	+
	N_FSD_T75_ln		*	(	0.026	)	+
	N_LabelAppeal		*	(	0.1926	)	+
	N_LabelAppeal_4		*	(	-0.055	)	+
	N_LabelAppeal_5		*	(	-0.12	)	+
	N_pH_T75_sq			*	(	-0.0153	)	+
	N_STARS_3			*	(	0.2445	)	+
	N_STARS_4			*	(	0.473	)	+
	N_STARS_GTE2		*	(	0.4898	)	+
	N_STARS_ln			*	(	-0.4666	)	+
	N_TSD_T75_ln		*	(	0.0417	)	+
	N_VA_T75			*	(	-0.1759	)	
	;

P_NB 		=	exp(P_NB);

**********************************************************************;
*	Zero Inflated Negative Binomial Model;
**********************************************************************;

*	All;
P_ZINB_ALL	=					4.7574		+
	MF_N_STARS			*	(	-0.1752	)	+
	N_AcidIndex_T95_sq	*	(	-0.0013	)	+
	N_Alcohol_T75_sq	*	(	0.0014	)	+
	N_Density_T75_ln	*	(	-5.5544	)	+
	N_LabelAppeal		*	(	0.2248	)	+
	N_LabelAppeal_3		*	(	0.0845	)	+
	N_LabelAppeal_4		*	(	0.0486	)	+
	N_STARS				*	(	0.1492	)	+
	N_STARS_3			*	(	-0.0639	)	+
	N_STARS_4			*	(	-0.1214	)	+
	N_VA_T75_ln			*	(	-0.1068	)	
	;

*	Zero model;
P_ZINB_ZERO	=					-5.4719		+
	MF_N_Alcohol		*	(	-0.2215	)	+
	MF_N_pH				*	(	0.3608	)	+
	MF_N_STARS			*	(	4.209	)	+
	N_AcidIndex_T99_sq	*	(	0.0188	)	+
	N_Alcohol_T75_ln	*	(	1.2593	)	+
	N_FSD_T75_rt		*	(	-0.0718	)	+
	N_LabelAppeal		*	(	0.9731	)	+
	N_LabelAppeal_3		*	(	0.4296	)	+
	N_LabelAppeal_4		*	(	0.3294	)	+
	N_pH_T75_sq			*	(	0.156	)	+
	N_RS_T75			*	(	-0.0119	)	+
	N_STARS_T90_ln		*	(	-4.8868	)	+
	N_Sulphates_T75_rt	*	(	2.136	)	+
	N_TSD_T75_ln		*	(	-0.3474	)	+
	N_VA_T75_sq			*	(	1.0825	)
	;

P_ZINB_ALL	=	exp(P_ZINB_ALL);
P_ZINB_ZERO	=	exp(P_ZINB_ZERO) / (1 + exp(P_ZINB_ZERO));
P_ZINB		=	P_ZINB_ALL * (1 - P_ZINB_ZERO);	

**********************************************************************;
*	Hurdle Logistic Negative Binomial Model;
**********************************************************************;


*	Logistic;
P_HLNB_LOG	=					0.8503		+
	MF_N_Alcohol		*	(	0.1875	)	+
	MF_N_Chlorides		*	(	-0.1192	)	+
	MF_N_FSD			*	(	0.0433	)	+
	MF_N_pH				*	(	-0.3267	)	+
	MF_N_RS				*	(	0.05	)	+
	MF_N_STARS			*	(	-4.0208	)	+
	MF_N_Sulphates		*	(	-0.0876	)	+
	MF_N_TSD			*	(	0.015	)	+
	N_AcidIndex_T99_sq	*	(	-0.0181	)	+
	N_Alcohol_T95_ln	*	(	-0.3889	)	+
	N_Chlorides_T75_ln	*	(	-1.4663	)	+
	N_FSD_T75_rt		*	(	0.0645	)	+
	N_LabelAppeal		*	(	-0.9031	)	+
	N_LabelAppeal_4		*	(	0.2168	)	+
	N_LabelAppeal_5		*	(	0.5071	)	+
	N_pH_T75_sq			*	(	-0.1504	)	+
	N_RS_T75			*	(	0.012	)	+
	N_STARS_3			*	(	0.3979	)	+
	N_STARS_GTE2		*	(	0.4445	)	+
	N_STARS_T90_ln		*	(	4.2395	)	+
	N_Sulphates_T75		*	(	-0.7345	)	+
	N_TSD_T75_ln		*	(	0.3302	)	+
	N_VA_T75_sq			*	(	-1.0593	)	
	;

*	Poisson;
P_HLNB_POI 	=					5.0645		+
	MF_N_STARS			*	(	-0.2004	)	+
	MF_N_Alcohol		*	(	0.0056	)	+
	MF_N_FSD			*	(	0.0083	)	+
	MF_N_TSD			*	(	0.0099	)	+
	N_AcidIndex_T95_sq	*	(	-0.0013	)	+
	N_Alcohol_T75_sq	*	(	0.0018	)	+
	N_Density_T75_ln	*	(	-6.6259	)	+
	N_FSD_T75_ln		*	(	0.0127	)	+
	N_LabelAppeal		*	(	0.4052	)	+
	N_LabelAppeal_4		*	(	-0.1727	)	+
	N_LabelAppeal_5		*	(	-0.3861	)	+
	N_pH_T75_ln			*	(	0.1509	)	+
	N_STARS				*	(	0.1126	)	+
	N_STARS_3			*	(	-0.0081	)	+
	N_STARS_GTE2		*	(	0.0398	)	+
	N_TSD_T75_ln		*	(	-0.0089	)	+
	N_VA_T75_ln			*	(	-0.1177	)
	;

*	Conversion;
P_HLNB_LOG 	=	exp(P_HLNB_LOG) / (1 + exp(P_HLNB_LOG));
P_HLNB_POI 	=	exp(P_HLNB_POI);
P_HLNB 		=	P_HLNB_LOG * (P_HLNB_POI + 1);

**********************************************************************;
*	Rounding;
**********************************************************************;

P_OLS_R		=	round(P_OLS, 1);
P_POI_R		=	round(P_POI, 1);
P_ZIP_R		=	round(P_ZIP, 1);
P_HLP_R		=	round(P_HLP, 1);
P_NB_R		=	round(P_NB, 1);
P_ZINB_R	=	round(P_ZINB, 1);
P_HLNB_R	=	round(P_HLNB, 1);

run; quit;

**********************************************************************;
*	Results;
**********************************************************************;

data scorefile (keep = INDEX P_:);
	set scorefile;
run; quit;

proc print data = scorefile (obs = 20);
run; quit;


