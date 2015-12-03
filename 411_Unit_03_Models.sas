
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

*	Chosen model;
proc reg data = &modelfile.;
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

*	Chosen model;
proc logistic data = &modelfile.;
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

*	Chosen model;
proc genmod data = &modelfile.;
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

*	Chosen model;
proc genmod data = &modelfile.;
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

*	Logistic Model;
proc logistic data = &modelfile.;
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
proc genmod data = &modelfile.;
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

*	Chosen model;
proc genmod data = &modelfile.;
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

*	Chosen model;
proc genmod data = &modelfile.;
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

*	Logistic Model;
proc logistic data = &modelfile.;
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
proc genmod data = &modelfile.;
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