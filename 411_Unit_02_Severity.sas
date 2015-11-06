**********************************************************************;
*	411-DL | Unit_02;
*	Last updated: 2015-11-04 by MJG;
**********************************************************************;

*	This section must be modified by the user;

libname mydata '/folders/myfolders/sasuser.v94' access = readonly;

**********************************************************************;
**********************************************************************;
*	Severity Models;
**********************************************************************;
**********************************************************************;

%let data_model = INS_MODEL;
%let data_sev = INS_Severity;
%let contents_sev = &data_sev._contents;
%let response = TARGET_AMT;

*	Set flag for PROC MEANS;
data &data_sev.;
	set &data_model.;
	TARGET_AMT_FLAG = (TARGET_AMT > 0);
run; quit;

*	Use output in Excel to do %change between 
	TARGET_AMT_FLAG = 0 & TARGET_AMT_FLAG = 1;
proc means data = &data_sev. mean median;
	class TARGET_AMT_FLAG;
	var _all_;
run;

*	Verify values with PROC MEANS;
proc means data = &data_sev. NOLABELS
	NMISS N MEAN MEDIAN MODE STD SKEW 
	P1 P5 P10 P25 P50 P75 P90 P95 P99 MIN MAX QRANGE;
run; quit;

*	Remove zero values from data set;
*	Shorten data set name, save to work library;
data &data_sev.;
	set &data_sev.;
	if &response. > 0;
run; quit;

***********************************;
*	PROC CONTENTS;
***********************************;

*	Intermediate step after the trim data set is created;
*	List out the column names and data types for the data set;
*	This is necessary as almost all macros depend on this output to
	extract variable names in data set for looping;

*	Create contents data set;
proc contents data = &data_sev. out = &contents_sev.;
run; quit;

*	Drop unnecessary variables gained from PROC CONTENTS;
data &contents_sev.;
	set &contents_sev.(keep = name type length varnum format formatl
		informat informl just npos nobs);
		if name = "&response." then delete;
			else if name = "&key." then delete;
			else if name = "TARGET_AMT" then delete;
		if name =: "MF" then delete; 
run; quit;

*	View the results;
proc print data = &contents_trim.;
run; quit;

**********************************************************************;
*	PROC CORR;
**********************************************************************;
ods trace on;
ods output PearsonCorr = wide_&corr.;
proc corr data = &data_sev.;
	var N_: C_:;
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
*	Models;
**********************************************************************;
**********************************************************************;


**********************************************************************;
*	Automated Variable Selection;
*	Note: only tested on 100 (full) data set;
**********************************************************************;

***********************************;
*	Model 1.1 @ 100 - Forward Selection;
***********************************;

*	AVS;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_:
	N_:
	MF_:
	/ selection = forward slentry = 0.11 adjrsq aic bic cp vif;
	output out = ins_sev_fw11_100 predicted = yhat residual = res;
run; quit;

*	Model;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_SEX_z_F
	N_BLUEBOOK_T99_rt
	N_CAR_AGE_T90_sq
	N_HOME_VAL_T75_rt
	N_MVR_PTS_T90_sq
	/ selection = rsquare start = 7 stop = 7 adjrsq aic bic cp vif;
	output out = ins_sev_fw11_100 predicted = yhat residual = res;
run; quit;

*	Begin steps to calculate MAE & MSE;
data ins_sev_fw11_100_res;
	set ins_sev_fw11_100;
	res = (TARGET_AMT - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Complete steps to calculate MAE & MSE;
proc means data = ins_sev_fw11_100_res mean nway noprint;
	var abs_res square_res;
	output out = ins_sev_fw11_stat
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = ins_sev_fw11_stat;
run; quit;

***********************************;
*	Model 1.2 @ 100 - Forward Selection;
***********************************;

*	AVS;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_:
	N_:
	MF_:
	/ selection = forward slentry = 0.15 adjrsq aic bic cp vif;
	output out = ins_sev_fw15_100 predicted = yhat residual = res;
run; quit;

*	Model;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_EDUCATION_z_HS
	C_JOB_Manager
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_SEX_z_F
	N_BLUEBOOK_T99_rt
	N_CAR_AGE_T90_sq
	N_HOME_VAL_T75_rt
	N_MVR_PTS_T90_sq
	/ selection = rsquare start = 9 stop = 9 adjrsq aic bic cp vif;
	output out = ins_sev_fw15_100 predicted = yhat residual = res;
run; quit;

*	Begin steps to calculate MAE & MSE;
data ins_sev_fw15_100_res;
	set ins_sev_fw15_100;
	res = (TARGET_AMT - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Complete steps to calculate MAE & MSE;
proc means data = ins_sev_fw15_100_res mean nway noprint;
	var abs_res square_res;
	output out = ins_sev_fw15_stat
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = ins_sev_fw15_stat;
run; quit;

***********************************;
*	Model 2.1 @ 100 - Stepwise Selection
***********************************;

*	AVS;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_:
	N_:
	MF_:
	/ selection = stepwise slentry = 0.11 slstay = 0.11 adjrsq aic bic cp vif;
	output out = ins_sev_sw11_100 predicted = yhat residual = res;
run; quit;

*	Model;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 
	C_REVOKED_Yes
	C_SEX_z_F
	N_BLUEBOOK_T99_rt
	N_CAR_AGE_T90_sq
	N_MVR_PTS_T90_sq
	/ selection = rsquare start = 5 stop = 5 adjrsq aic bic cp vif;
	output out = ins_sev_sw11_100 predicted = yhat residual = res;
run; quit;

*	Begin steps to calculate MAE & MSE;
data ins_sev_sw11_100_res;
	set ins_sev_sw11_100;
	res = (TARGET_AMT - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Complete steps to calculate MAE & MSE;
proc means data = ins_sev_sw11_100_res mean nway noprint;
	var abs_res square_res;
	output out = ins_sev_sw11_stat
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = ins_sev_sw11_stat;
run; quit;

***********************************;
*	Model 2.2 @ 100 - Stepwise Selection;
***********************************;

*	Note: produces identical model to forward selection with
	slentry = 0.15;

***********************************;
*	Model 3.1 @ 100 - Hand Picked;
***********************************;

*	Model;
proc reg data = &data_sev. plots = diagnostics;
model TARGET_AMT = 

	C_EDUCATION_z_HS
	C_CAR_USE_Commercial
	C_CAR_TYPE_Panel_Truck
	C_JOB_Manager
	C_JOB_Professional
	C_MSTATUS_Yes
	C_REVOKED_Yes
	C_SEX_z_F

	N_CAR_AGE_T90_sq
	N_HOME_VAL_T75_rt
	N_MVR_PTS_T90_sq
	N_BLUEBOOK_T99_rt
	N_OLDCLAIM_T99_sq

	MF_N_HOME_VAL

	/ vif;
	output out = ins_sev_100 predicted = yhat residual = res;
run; quit;

*	Begin steps to calculate MAE & MSE;
data ins_sev_100_res;
	set ins_sev_100;
	res = (TARGET_AMT - yhat);
	where res is not missing;
	abs_res = abs(res);
	square_res = (res**2);
run; quit;

*	Complete steps to calculate MAE & MSE;
proc means data = ins_sev_100_res mean nway noprint;
	var abs_res square_res;
	output out = ins_sev_stat
	mean(abs_res) = MAE
	mean(square_res) = MSE;
run; quit;

*	Calculated MAE and MSE;
proc print data = ins_sev_stat;
run; quit;







