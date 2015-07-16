**********************************************************************;
*	410-DL | Sandbox Code;
*	Last updated: 2015-07-13 by MJG;
**********************************************************************;

*	Connect to data;
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access = readonly;

*	Shorten data name, save to work library;
data AHD;
	set mydata.ames_housing_data;
run; quit;

*	Simple Linear Regression Model #1;
proc reg data = AHD_Sample 
	outest = SLR1
	plots = (residuals(smooth));
	SLR1: model SalePrice = GrLivArea / alpha = 0.05 clb vif;
	title 'Simple Linear Regression Model 1';
run; quit;

proc print data = SLR1;
run; quit;