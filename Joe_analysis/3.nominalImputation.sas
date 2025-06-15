libname vbov '/home/u63688618/uhasselt/thesis/combined';


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for PERINEUM */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class perineumdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var perineumdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(perineumdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = perineumdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = perineumdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = perineumdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = perineumdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = perineumdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = perineumdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = perineumdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model perineumdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd / VIF;
RUN;

PROC CORR DATA=imputed3;
   VAR maternalAge muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class perineumdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model perineumdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for EPISIOTOMY */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class episiotomydd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var episiotomydd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(episiotomydd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = episiotomydd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = episiotomydd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = episiotomydd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = episiotomydd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = episiotomydd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = episiotomydd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = episiotomydd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model episiotomydd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;

ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class episiotomydd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model episiotomydd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=logit)=lgsparms;
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


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for FETALMONITORING */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var fetalSurvdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(fetalSurvdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = fetalSurvdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = fetalSurvdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = fetalSurvdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = fetalSurvdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / VIF;
RUN;

ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class fetalSurvdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model fetalSurvdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for GBS */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class GBSdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var GBSdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(GBSdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = GBSdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = GBSdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = GBSdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = GBSdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = GBSdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = GBSdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = GBSdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model GBSdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class GBSdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model GBSdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=logit)=lgsparms;
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

/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for FETAL RESUSCITATION */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class resuscdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var resuscdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(resuscdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = resuscdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = resuscdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = resuscdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = resuscdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = resuscdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = resuscdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = resuscdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model resuscdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class resuscdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model resuscdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for DELIVERY POSITION */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class positiondd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var positiondd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(positiondd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = positiondd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = positiondd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = positiondd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = positiondd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = positiondd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = positiondd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = positiondd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model positiondd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class positiondd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model positiondd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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



/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for WATERBIRTH */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class bathBirthdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var bathBirthdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(bathBirthdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = bathBirthdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = bathBirthdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = bathBirthdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = bathBirthdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = bathBirthdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = bathBirthdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = bathBirthdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model bathBirthdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class bathBirthdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model bathBirthdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=logit)=lgsparms;
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



/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for GENDER */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class genderdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var genderdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(genderdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = genderdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = genderdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = genderdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = genderdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = genderdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = genderdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = genderdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model genderdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class genderdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model genderdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=logit)=lgsparms;
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



/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for  PLACENTALPHASE */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class placentaldd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var placentaldd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(placentaldd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = placentaldd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = placentaldd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = placentaldd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = placentaldd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = placentaldd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = placentaldd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = placentaldd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model placentaldd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;

ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class placentaldd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model placentaldd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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



/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for Birth WEIGHT  */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var birthWeight muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   	
   fcs regression(birthWeight = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = birthWeight muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = birthWeight muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = birthWeight muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = birthWeight muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = birthWeight muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = birthWeight muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = birthWeight muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed2;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed2 PLOTS=(RESIDUALBYPREDICTED QQPLOT);
   model birthWeight = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd / VIF;
   OUTPUT OUT=resid_data R=residual;
RUN;

ODS GRAPHICS ON / OUTPUTFMT=PNG IMAGENAME="Residual_Histogram" RESET;
ODS LISTING GPATH="/home/u63688618/uhasselt/thesis/combined";

PROC UNIVARIATE DATA=resid_data NORMAL;
   VAR residual;
   HISTOGRAM residual / NORMAL;
   QQPLOT residual / NORMAL;
RUN;

ODS GRAPHICS OFF;

ods select none;
proc mixed data=imp_data;
   by _Imputation_;
   class muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / reference=first;
   model birthWeight = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / s;
   	*by _imputation_;
   	ods output SolutionF = mxparms;
run;

ods select all;
proc print data=mxparms noobs;
  where _Imputation_=1;
  title 'linear regression Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=full)=mxparms;
  class muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd;
  modeleffects Intercept maternalAge muraldd langdd relationshipdd educationdd miscarriage 
  		abortion conceptiondd;
  ods output ParameterEstimates=mi_parms;
run;


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for DELIVERY METHOD */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class deliverydd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var deliverydd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(deliverydd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = deliverydd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = deliverydd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = deliverydd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = deliverydd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = deliverydd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = deliverydd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = deliverydd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model deliverydd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class deliverydd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model deliverydd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
   	*by _imputation_;
   	ods output ParameterEstimates=lgsparms;
run;

ods select all;
proc print data=lgsparms noobs;
  where _Imputation_=1;
  title 'LOGISTIC Model Coefficients (First Imputation)';
run;

proc mianalyze parms(classvar=classval link=logit)=lgsparms;
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



