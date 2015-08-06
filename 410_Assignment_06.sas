**********************************************************************;
*	410-DL | Assignment 06;
*	Last updated: 2015-08-02 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data stocks;
	set mydata.stock_portfolio_data;
run; quit;

*	Proc contents;
proc contents data = stocks
	out = stocks_contents(keep = varnum name nobs) noprint;
run; quit;

**********************************************************************;
*	SAS Macros;
**********************************************************************;
*	Macro for log returns;
%macro log_returns(varname);
	data stocks;
		set stocks;
		return_&varname. = log(&varname./lag1(&varname.));
	run; quit;
%mend;

**********************************************************************;
*	Part 1: Data Prep;
**********************************************************************;

*	Compute the log-returns - log of the ratio of today's price
	to yesterday's price;
*	Note that the data needs to be sorted in the correct direction in
	order for us to compute the correct return;

*	Sort data;
proc sort data = stocks;
	by date;
run; quit;

*	Call macro, calculate log-returns;
data temp;
	do i = 1 to num;
		set stocks_contents nobs = num;
			if name = 'Date' then delete;
			call execute('%log_returns('||name||')');
	end;
run; quit;

*	Name the log-return for VV as the response variable;
data stocks;
	set stocks;
	response_VV = return_VV;
run; quit;

*	Print observations and verify;
proc print data = stocks;
run; quit;

**********************************************************************;
*	Part 2: Correlations & Market Index;
**********************************************************************;

*	Use ODS Output to print Pearons' Correlation for portfolio;
*	Can also specify "outp = port_corr" in first line of PROC CORR;
ods trace on;
ods output PearsonCorr = port_corr;
proc corr data = stocks;
	var return_:;
	with response_VV;
run; quit;
ods trace off;

*	Print to verify output;
proc print data = port_corr;
run; quit;

**********************************************************************;
*	Part 3: Reshape Output in port_corr;
**********************************************************************;

*	Note we use "return_:" to wildcard, the data set port_corr also
	contains separate variables as "Preturn_:" which are the p-values
	for the correlation coefficients, we drop those here;
data wide_corr;
	set port_corr (keep = return_:);
run; quit;

*	Note that wide_correlations is a 'wide' data set and we need a 
	'long' data set;
*	Use PROC TRANSPOSE to convert data from one format to the other;
proc transpose data = wide_corr out = long_corr;
run; quit;

*	Now data are in long format, we do two things in the code below:
	(1) rename _NAME_ to TKR and use substring to drop "return_"
	(2) rename column "COL1" to "correlation";
*	Remember, these are all correlations to response_VV;
data long_corr;
	set long_corr;
	tkr = substr(_NAME_, 8, 3);
	drop _NAME_;
	rename COL1 = correlation;
run;

*	Print to verify output;
proc print data = long_corr;
run; quit;

**********************************************************************;
*	Part 4: Visualize the Correlations;
**********************************************************************;

*	Merge on sector id and make a colored bar plot;

*	Create sectors for each ticker (TKR), since tickers are unique
	we will treat it as a primary key and merge on the TKR variable;
*	Note: we use 1-4 below because SAS treats the tab (for formatting)
	as a character, which makes tickers with length = 3 really be = 4;
data secto;
	input tkr $ 1-3 sector $ 4-35;
	datalines;
AA Industrial - Metals
BAC Banking
BHI Oil Field Services
CVX Oil Refining
DD Industrial - Chemical
DOW Industrial - Chemical
DPS Soft Drinks
GS Banking
HAL Oil Field Services
HES Oil Refining
HON Manufacturing
HUN Industrial - Chemical
JPM Banking
KO Soft Drinks
MMM Manufacturing
MPC Oil Refining
PEP Soft Drinks
SLB Oil Field Services
WFC Banking
XOM Oil Refining
VV Market Index
	;
run; quit;

*	Print to verify output;
proc print data = sector; 
run; quit;

*	Use PROC SQL to join, turn off warning as this one does not matter;
proc sql nowarn;
	create table merge_corr as
	select *
	from long_corr, sector
	where long_corr.tkr = sector.tkr;
quit;

*	Sort data by correlation, default is ascending;
proc sort data = merge_corr;
	by correlation;
run; quit;

*	Print to verify output;
proc print data = merge_corr;
run; quit;

*	Make Grouped Bar Plot;
* 	See also: p. 48 Statistical Graphics Procedures By Example;
ods graphics on;
title 'Correlations with the Market Index ($VV)';
proc sgplot data = merge_corr;
	format correlation 3.2;
	vbar tkr / response = correlation group = sector
	groupdisplay = cluster datalabel categoryorder = respasc;
run; quit;
ods graphics off;

**********************************************************************;
*	Part 5: Compute Principal Componenets for Return Data;
**********************************************************************;

*	Create return_data, drop return_VV from earlier;
data return_data;
	set stocks (keep = return_:);
	drop return_VV;
run; quit;

*	Print to verify output;
proc print data = return_data(obs = 10);
run; quit;

*	Compute principal components;
ods graphics on;
proc princomp data = return_data out = pca_output
	outstat = eigenvectors plots = scree(unpackpanel);
run; quit;
ods graphics off;

*	Print to verify output;
proc print data = pca_output(obs=10); 
run; quit;

proc print data = eigenvectors(where = (_TYPE_ = 'SCORE'));
run; quit;

*	Display the two plots and the Eigenvalue table from the output;

*	Plot the first two eigenvectors;
data pca_two;
	set eigenvectors(where = (_NAME_ in ('Prin1','Prin2')));
	drop _TYPE_ ;
run; quit;

*	Print to verify output;
proc print data = pca_two; 
run; quit;

*	Transpose from wide to long;
proc transpose data = pca_two out = long_pca; 
run; quit;

*	Print to verify output;
proc print data=long_pca; 
run; quit;

*	Now data are in long format, we do two things in the code below:
	(1) rename _NAME_ to TKR and use substring to drop "return_"
	(2) rename column "COL1" to "correlation";
*	Remember, these are all correlations to response_VV;
data long_pca;
	set long_pca;
	format tkr $3.;
	tkr = substr(_NAME_,8,3);
	drop _NAME_;
run;

*	Print to verify output;
proc print data=long_pca; 
run; quit;

*	Use PROC SQL to join, turn off warning as this one does not matter;
proc sql nowarn;
	create table merge_pca as
	select *
	from long_pca, sector
	where long_pca.tkr = sector.tkr;
quit;

*	Print to verify output;
proc print data = merge_pca; 
run; quit;

*	Plot the first two principal components;
ods graphics on;
title1 'Scatterplot of Principal Components Prin1 & Prin2';
title2 'Eigenvectors, Grouped by Sector';
proc sgplot data = merge_pca;
	scatter x = Prin1 y = Prin2 / datalabel = tkr group = sector;
run; quit;
ods graphics off;

**********************************************************************;
*	Part 6: Create Train / Test Split for Cross-Validation;
**********************************************************************;

*	Create test/train split for cross-validation;
*	Delete the first row because it's filled with missing values
	from the next day lag;
data cv_data;
	merge pca_output stocks(keep = response_VV);
	if cmiss(of _all_) then delete;
	u = uniform(123);
	if (u < 0.70) then train = 1;
		else train = 0;
	if (u > 0.70) then test = 1;
		else test = 0;
	if (train = 1) then train_response = response_VV;
		else train_response = .;
	if (test = 1) then test_response = response_VV;
		else test_response = .;
run;

*	Use PROC FREQ to determine how close we are to 70/30 split;
proc freq data = cv_data;
	tables train test;
run; quit;

proc print data = cv_data(obs = 10); 
run; quit;

**********************************************************************;
*	Part 7: Regression Modeling: Individual Stocks & VV as Response;
**********************************************************************;

***********************************;
*	Training Model;
***********************************;
proc reg data = cv_data plots = diagnostics(unpack);
	model train_response = return_: / vif;
	output out = train_all predicted = yhat residual = res;
run; quit;

*	Need to calculate MAE and MSE;
data train_all_res;
	set train_all;
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

proc means data = train_all_res mean nway nmiss;
	class train;
	var abs_res square_res;
	output out = em_train_all
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

proc print data = em_train_all;
run; quit;

***********************************;
*	Testing Model;
***********************************;
proc reg data = cv_data plots = diagnostics(unpack);
	model test_response = return_: / vif;
	output out = test_all predicted = yhat residual = res;
run; quit;

*	Need to calculate MAE and MSE;
data test_all_res;
	set test_all;
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

proc means data = test_all_res mean nway nmiss;
	class test;
	var abs_res square_res;
	output out = em_test_all
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

proc print data = em_test_all;
run; quit;

**********************************************************************;
*	Part 8: Regression Modeling: First Eight Principal Components;
**********************************************************************;

***********************************;
*	Training Model;
***********************************;
proc reg data = cv_data plots = diagnostics(unpack);
	model train_response = Prin1-Prin8 / vif;
	output out = train_pca predicted = yhat residual = res;
run; quit;

*	Need to calculate MAE and MSE;
data train_pca_res;
	set train_pca;
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

proc means data = train_pca_res mean nway nmiss;
	class train;
	var abs_res square_res;
	output out = em_train_pca
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

proc print data = em_train_pca;
run; quit;

***********************************;
*	Testing Model;
***********************************;
proc reg data = cv_data plots = diagnostics(unpack);
	model test_response = Prin1-Prin8 / vif;
	output out = test_pca predicted = yhat residual = res;
run; quit;

*	Need to calculate MAE and MSE;
data test_pca_res;
	set test_pca;
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

proc means data = test_pca_res mean nway nmiss;
	class test;
	var abs_res square_res;
	output out = em_test_pca
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

proc print data = em_test_pca;
run; quit;

**********************************************************************;
*	FIN;
**********************************************************************;
