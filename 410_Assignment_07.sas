**********************************************************************;
*	410-DL | Assignment 07;
*	Last updated: 2015-08-06 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data stocks;
	set mydata.stock_portfolio_data;
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

*	Drop variables;
data stocks;
	set stocks;
	drop AA HON MMM DPS KO PEP MPC GS;
run; quit;

*	Proc contents;
proc contents data = stocks
	out = stocks_contents(keep = varnum name nobs) noprint;
run; quit;

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
proc print data = stocks(obs = 10);
run; quit;

*	Create new data set only of return data, use : as wildcard;
*	Delete missing observations (Day 1, no prior return to calculate);
data return_data;
	set stocks (keep = return_:);
	if cmiss(of _all_) then delete;
run; quit;

*	Print observations and verify;
proc print data = return_data(obs = 10);
run; quit;

**********************************************************************;
*	Part 2: Principal Factor Analysis: No Rotation;
**********************************************************************;

*	Questions:
	(1) How many factors did SAS retain?
	(2) Do we have enough factors to support our initial hypothesis?
	(3) Do our common factors exhibit a simple factor structure?
	(4) What criterion did SAS use to select the number of factors to
		retain?
	(5) is there anything strange about the default SAS output?
	(6) In addition to estimating the common factors, SAS will produce
		a plot of Factor 1 against Factor 2. Is there anything interesting
		in this plot?
;
ods graphics on;
proc factor data = return_data method = principal priors = smc 
	rotate = none plots = (all);
run; quit;
ods graphics off;

**********************************************************************;
*	Part 3: Principal Factor Analysis: Varimax Rotation;
**********************************************************************;

*	Questions:
	(1) Did SAS retain the same number of factors?
	(2) What components of the PROC FACTOR output did the rotation change?
	(3) Did we obtain a 'simple structure' from our factor rotation?
	(4) Did we increase the interpretability using the factor rotation?
;
ods graphics on;
proc factor data = return_data method = principal priors = smc 
	rotate = varimax plots = (all);
run; quit;
ods graphics off;

**********************************************************************;
*	Part 4: Maximum Likelihood Factor Analysis: Varimax Rotation;
**********************************************************************;

*	Questions:
	(1) How many common factors are suggested by ML Factor Analysis?
	(2) How does ML Factor Analysis arrive at this number of factors,
		and in general, how do we interpret the output from a ML Factor
		Analysis?
	(3) From a modeling perspective, what does ML Factor Analysis provide
		that Principal Factor Analysis does not?
;
ods graphics on;
proc factor data = return_data method = ML priors = smc rotate = varimax
	plots = (loadings);
run; quit;
ods graphics off;

**********************************************************************;
*	Part 5: Maximum Likelihood Factor Analysis: 
*			Varimax Rotation & Priors = Max;
**********************************************************************;

*	Questions:
	(1) How many common factors are now suggested by ML Factor Analysis?
	(2) Is this a valid factor analysis? Why or why not?
	(3) What does this suggest about the sensitivity of common factor
		estimation to the prior estimates of the communalities?
;
ods graphics on;
proc factor data = return_data method = ML priors = max rotate = varimax
	plots = (loadings);
run; quit;
ods graphics off;

**********************************************************************;
*	FIN;
**********************************************************************;
