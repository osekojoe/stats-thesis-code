
libname vbov '/home/u63688618/uhasselt/thesis/combined';

data vbov.vbovRQ4MI;
set vbov.vbovsas;

	if planned = "birthCenter" then plannedd=1;
	if planned = "home" then plannedd=2;
	if planned = "hospital" then plannedd=3;
	if planned = "midwifeLedUni" then plannedd=4;
	
	if actual = "birthCenter" then actualdd=1;
	if actual = "home" then actualdd=2;
	if actual = "hospital" then actualdd=3;
	if actual = "midwifeLedUni" then actualdd=4;
	
	if birthingResponsibility = "obstetrician" then responsibledd=0;
	if birthingResponsibility = "midwife" then responsibledd=1;
	
	if mural = "extramural" then muraldd=1;
	if mural = "intramural" then muraldd=0;
	
	if mothertongue = "french" then langdd=1;
	if mothertongue = "dutch" then langdd=2;
	if mothertongue = "english" then langdd=3;
	if mothertongue = "other" then langdd=4;
	
	if relationship = "Ma" then relationshipdd=1;
	if relationship = "Si" then relationshipdd=0;
	
	if educLevel = "te" then educationdd=1;
	if educLevel = "se" then educationdd=0;
	
	if pregnancyOnset = "spontaneous" then conceptiondd=1;
	if pregnancyOnset = "ICSI" then conceptiondd=2;
	if pregnancyOnset = "IUI" then conceptiondd=3;
	if pregnancyOnset = "IVF" then conceptiondd=4;
	if pregnancyOnset = "KI" then conceptiondd=5;
	if pregnancyOnset = "hormonal" then conceptiondd=6;
run;


PROC FREQ DATA=vbov.vbovRQ4MI;
    TABLE referral / CHISQ; 
RUN;

data vbov.vbovRQ4MI;
   set vbov.vbovRQ4MI;
   keep plannedd actualdd responsibledd muraldd langdd maternalAge relationshipdd educationdd miscarriage
   		abortion conceptiondd;
run;


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for PLANNED */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ4MI out=imp_data nimpute=10 seed=2025 ;
   class plannedd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var plannedd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   *fcs logistic(plannedd = langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = plannedd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = plannedd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = plannedd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = plannedd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = plannedd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = plannedd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = plannedd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model plannedd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class plannedd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model plannedd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=glogit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=glogit)=lgsparms;
  class langdd relationshipdd educationdd miscarriage abortion conceptiondd;
  modeleffects Intercept maternalAge langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd;
  ods output ParameterEstimates=mi_parms;
run;

       
data mi_parms;
  set mi_parms;
  where parm ne 'Intercept';
  OR=exp(estimate);
  LCL_OR=exp(LCLMean);
  UCL_OR=exp(UCLMean);
run;

proc print data=mi_parms noobs;
  var parm langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd OR LCL_OR UCL_OR;
  title 'Combined odds ratio estimates and confidence limits';
run;


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for ACTUAL */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ4MI out=imp_data nimpute=10 seed=2025 ;
   class actualdd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var actualdd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   *fcs logistic(actualdd = langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = actualdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = actualdd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = actualdd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = actualdd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = actualdd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = actualdd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = actualdd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;


data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model actualdd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class actualdd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model actualdd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=glogit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=glogit)=lgsparms;
  class langdd relationshipdd educationdd miscarriage abortion conceptiondd;
  modeleffects Intercept maternalAge langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd;
  ods output ParameterEstimates=mi_parms;
run;

       
data mi_parms;
  set mi_parms;
  where parm ne 'Intercept';
  OR=exp(estimate);
  LCL_OR=exp(LCLMean);
  UCL_OR=exp(UCLMean);
run;

proc print data=mi_parms noobs;
  var parm langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd OR LCL_OR UCL_OR;
  title 'Combined odds ratio estimates and confidence limits';
run;




/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for RESPONSIBLE PERSON */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ4MI out=imp_data nimpute=10 seed=2025 ;
   class responsibledd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var responsibledd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   *fcs logistic(responsibledd = langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = responsibledd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = responsibledd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = responsibledd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = responsibledd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = responsibledd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = responsibledd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = responsibledd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;


data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model responsibledd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class responsibledd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model responsibledd = maternalAge langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;


ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval)=lgsparms;
  class langdd relationshipdd educationdd miscarriage abortion conceptiondd;
  modeleffects Intercept maternalAge langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd;
  ods output ParameterEstimates=mi_parms;
run;

       
data mi_parms;
  set mi_parms;
  where parm ne 'Intercept';
  OR=exp(estimate);
  LCL_OR=exp(LCLMean);
  UCL_OR=exp(UCLMean);
run;

proc print data=mi_parms noobs;
  var parm langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd OR LCL_OR UCL_OR;
  title 'Combined odds ratio estimates and confidence limits';
run;


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for REFERRAL */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ4MI out=imp_data nimpute=10 seed=2025 ;
   class referral muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var referral muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   *fcs logistic(referral = langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = referral muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = referral muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = referral muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = referral muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = referral muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = referral muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = referral muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;


data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model referral = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class referral muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model referral = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval)=lgsparms;
  class muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd;
  modeleffects Intercept maternalAge muraldd langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd;
  ods output ParameterEstimates=mi_parms;
run;

       
data mi_parms;
  set mi_parms;
  where parm ne 'Intercept';
  OR=exp(estimate);
  LCL_OR=exp(LCLMean);
  UCL_OR=exp(UCLMean);
run;

proc print data=mi_parms noobs;
  var parm muraldd langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd OR LCL_OR UCL_OR;
  title 'Combined odds ratio estimates and confidence limits';
run;

