**********************************************************************;
*	411-DL | Week 00;
*	Last updated: 2015-09-14 by MJG;
**********************************************************************;

**********************************************************************;
**********************************************************************;
*	Tutorial 01: SAS Data Step;
**********************************************************************;
**********************************************************************;

**********************************************************************;
*	Part 1: Creating SAS Data Sets;
**********************************************************************;
*	Create two data sets using DATALINES;
data temp1;
	length dimkey $2; 
	length x 8.0; 
	length Y 8.0; 
	input dimkey $ x y; 
datalines; 
01 100 12.2
02 300 7.45
03 200 10.0
04 500 5.67
05 300 4.55
;
run; quit;

data temp2; 
	length dimkey $2; 
	length Z 8.0; 
	length first_name $10; 
	length last_name $10; 
	input dimkey $ z first_name $ last_name $; 
datalines; 
01 100 steve miller
02 300 Steve Utrup
04 500 JacK wilsoN
05 300 AbRAham LINcoln
06 100 JackSON SmiTH
07 200 EarL Campbell
08 400 WiLLiam Right
;
run; quit;

**********************************************************************;
*	Part 2: PROC PRINT;
**********************************************************************;

*	Print these data for viewing;
title "Data = temp1";
proc print data = temp1;
run; quit;

title "Data = temp2";
proc print data = temp2;
run; quit;

**********************************************************************;
*	Part 3: PROC CONTENTS;
**********************************************************************;

*	Explore the contents of these data;
title; 
*	Note: the above resets the title statement to blank;
proc contents data = temp1;
run; quit;

proc contents data = temp2;
run; quit;

**********************************************************************;
*	Part 4: Manipulating SAS Data Sets;
**********************************************************************;

data temp1;
	set temp1;
	w = 2*y + 1;
	*	See Little SAS Book pp. 84-85;
	if (x < 150) then segment = 1;
	else if (x < 250) then segment = 2;
	else segment = 3;
run; quit;

data temp2;
	set temp2;
	*	See Little SAS Book pp. 78-79;
	proper_first_name = propcase(first_name);
	upper_last_name = upcase(last_name);
	first_initial = substr(upcase(first_name),1,1);
	last_initial = substr(upcase(last_name),1,1);
	initials = compress(first_initial||last_initial);
run; quit;

title "Data = temp1";
proc print data = temp1;
run; quit;

title "Data = temp2";
proc print data = temp2(obs = 15);
*	Note: why are we setting observations to 15, there are only 7;
run; quit;

**********************************************************************;
*	Part 5: Subsetting SAS Data Sets;
**********************************************************************;
*	See Little SAS Book pp. 86-87, 328-330;

data s1;
	set temp1;
	if (segment = 1);
run; quit;

data s2;
	set temp1;
	if (segment ne 1) then delete;
run; quit;

*	This will work, but it is not really proper;
data s3;
	set temp1;
	where (segment = 1);
run; quit;

*	A where clause really belongs in the set statement;
data s4;
	set temp1 (where = (segment = 1));
run; quit;

*	Now verify the data sets are the same;
%macro Data_Compare(num);
	title;
	proc compare base = s1 compare = s&num. novalues;
	run; quit;
%mend;

data _null_;
	do i = 2 to 4;
		call execute('%Data_Compare('||i||')');
	end;
run; quit;

**********************************************************************;
*	Part 6: Combining SAS Data Sets;
**********************************************************************;

***********************************;
*	Stacking two data sets together;
*	See Little SAS Book pp. 180-181;
***********************************;
data stacked_data;
	set temp1 temp2;
run; quit;

title "Data = stacked_data";
proc print data = stacked_data;
run; quit;

***********************************;
*	Creating an ordered stack of two data sets;
*	See Little SAS Book pp. 182-183;
***********************************;
*	First, sort to be in same order (necessary for merge);
proc sort data = temp1;
	by dimkey;
run; quit;

proc sort data = temp2;
	by dimkey;
run; quit;

*	Now create stacked data set based on key (dimkey);
data ordered_stack;
	set temp1 temp2;
	by dimkey;
run; quit;

*	Print results;
title "Data = ordered_stack";
proc print data = ordered_stack;
run; quit;

***********************************;
*	Merge data sets;
*	See Little SAS Book pp. 184-187;
***********************************;
data merged_data;
	merge temp1 temp2;
	by dimkey;
run; quit;

*	Print results;
title "Data = merged_data";
proc print data = merged_data;
run; quit;

***********************************;
*	Using different joins with an IN statement;
*	Note: usually easier to use PROC SQL;
*	See Little SAS Book pp. 200-201;
***********************************;
*	LEFT JOIN;
title "LEFT JOIN OUTPUT";
data left_join;
	merge temp1 (in = a) temp2 (in = b);
	by dimkey;
	if (a = 1);
run; quit;

proc print data = left_join;
run; quit;

*	RIGHT JOIN;
title "RIGHT JOIN OUTPUT";
data right_join;
	merge temp1 (in = a) temp2 (in = b);
	by dimkey;
	if (b = 1);
run; quit;

proc print data = right_join;
run; quit;

*	INNER JOIN;
title "INNER JOIN OUTPUT";
data inner_join;
	merge temp1 (in = a) temp2 (in = b);
	by dimkey;
	if (a = 1) and (b = 1);
run; quit;

proc print data = inner_join;
run; quit;

**********************************************************************;
**********************************************************************;
*	Tutorial 02: SAS Scatter Plot;
**********************************************************************;
**********************************************************************;

*	Connect to data;
libname mydata '/sscc/home/m/mga293/411-DL/SAS_Data' access = readonly;

*	Shorten data name, save to work library;
data AQ;
	set mydata.anscombe;
run; quit;

**********************************************************************;
*	Part 1: PROC CORR;
**********************************************************************;
*	Set up the macro;
%macro PROC_CORR(num);
	ods graphics on;
	proc corr data = AQ;
		var x&num.;
		with y&num.;
		title "PROC CORR Example: Anscombe's Quartet";
		title2 "X&num. and Y&num. Correlation";
	run; quit;
	ods graphics off;
%mend;

*	Run the macro;
data _null_;
	do i = 1 to 4;
		call execute('%PROC_CORR('||i||')');
	end;
run; quit;

**********************************************************************;
*	Part 2: PROC SGPLOT;
**********************************************************************;
*	Set up the macro;
%macro PROC_SGPLOT(num);
	ods graphics on;
	proc sgplot data = AQ;
		loess x = x&num. y = y&num. / nomarkers;
		reg x = x&num. y = y&num.;
		title "PROC SGPLOT Example: Anscombe's Quartet";
		title2 "X&num. and Y&num. Scatter Plot with LOESS and Regression";
	run; quit;
	ods graphics off;
%mend;

*	Run the macro;
data _null_;
	do i = 1 to 4;
		call execute('%PROC_SGPLOT('||i||')');
	end;
run; quit;

**********************************************************************;
*	Part 3: PROC SGSCATTER;
**********************************************************************;
*	Set up the macro;
%macro PROC_SGSCATTER(num);
	ods graphics on;
	proc sgscatter data = AQ;
		compare x = x&num. y = y&num. / loess reg;
		title "PROC SGSCATTER Example: Anscombe's Quartet";
		title2 "X&num. and Y&num. Scatter with LOESS and Regression";
	run; quit;
	ods graphics off;
%mend;

*	Run the macro;
data _null_;
	do i = 1 to 4;
		call execute('%PROC_SGSCATTER('||i||')');
	end;
run; quit;

**********************************************************************;
**********************************************************************;
*	Tutorial 03: OLS Regression;
**********************************************************************;
**********************************************************************;

*	Connect to data;
libname mydata '/sscc/home/m/mga293/411-DL/SAS_Data' access = readonly;

*	Shorten data name, save to work library;
data CC;
	set mydata.cigarette_consumption;
run; quit;

title "OLS Regression SAS Tutorial";

*	Use PROC CORR across all vars;
proc corr data = CC
	plot = matrix(histogram nvar = all);
	title2 "Scatterplot Matrix";
run; quit;

*	Use PROC REG to model highest correlated independent variable;
proc reg data = CC
	plots(only) = (diagnostics residuals fitplot);
	model sales = income;
	title2 "Base Model";
run; quit;

*	Use PROC REG to model multiple variables;
*	OLS Model using Part E on pp. 87 RABE;
*	Note: if model contains >1 regressor, fitplot cannot be produced;
proc reg data = CC
	plots(only) = (diagnostics residuals);
	model sales = age income price / vif;
	title2 "Optimal Model";
	output out = fitted_model pred = yhat residual = resid 
		ucl = ucl lcl = lcl;
run; quit;

**********************************************************************;
**********************************************************************;
*	Tutorial 04: Logistic Regression;
**********************************************************************;
**********************************************************************;

*	Connect to data;
libname mydata '/sscc/home/m/mga293/411-DL/SAS_Data' access = readonly;

*	Shorten data name, save to work library;
data FR;
	set mydata.financial_ratios;
run; quit;

*	Use PROC MEANS to produce simple univariate statistics for
	numeric variables;
*	Note: much more controlled output vs. PROC UNIVARIATE;
*	Note: CLASS statement will show statistics by Y = 1 and Y = 0
title "Logistic Regression EDA - Examine Means";
proc means data = FR min p25 p50 p75 max ndec = 2;
	class Y;
	var X1 X2 X3;
run; quit;

*	Expand beyond min, 25th, 50th, 75th, and max percentiles;
title "Logistic Regression EDA - Examine Means";
proc means data = FR p5 p10 p25 p50 p75 p90 p95 ndec = 2;
	class Y;
	var X1 X2 X3;
run; quit;

*	Use DATA step to discretize continuous variables;
data bankrupt;
	set FR;
	if (X1 < 0) then X1_discrete = 0; else X1_discrete = 1;
	if (X2 < 0) then X2_discrete = 0; else X2_discrete = 1;
	if (X3 < 0) then X3_discrete = 0; else X3_discrete = 1;
run; quit;

*	Examine frequencies of new variables for predictive relevance
	to response variable;
title "Logistic Regression EDA - Examine Frequencies";
proc freq data = bankrupt;
	table (X1_discrete X2_discrete X3_discrete) * Y / missing;
run; quit;

*	PROC LOGISTIC can be used for single variable or multiple variable
	logistic regression;
*	RABE pp. 338-340 is a multiple logistic regression;
title "Logistic Regression - All Continuous Variables";
proc logistic data = bankrupt descending;
	model Y = X1 X2 X3;
run; quit;

*	Use PROC LOGISTIC with AVS (here we use backward);
title "Logistic Regression - All Variables, Backward Selection";
proc logistic data = bankrupt descending;
	model Y = X1 X2 X3 X1_discrete X2_discrete X3_discrete
	/ selection = backward;
run; quit;

*	Include plot of Receiver Operating Characteristic (ROC)
	curve to PROC LOGISTIC output;
title "Logistic Regression - Select Single Predictor";
ods graphics on;
proc logistic data = bankrupt descending plots(only) = roc(id = prob);
	model Y = X1 / outroc = roc;
run; quit;
ods graphics off;

proc print data = roc;
run; quit;

*	FIN
