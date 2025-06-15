
libname vbov '/home/u63688618/uhasselt/thesis/combined';



/* --- Variables recoded to numerical categories for imputation --- */



data vbov.vbovdecod;
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



data vbov.vbovRQ3MI;
set vbov.vbovdecod;
	if bloodLoss = "<500ml" then bloodlossdd=1;
	if bloodLoss = "500-1000ml" then bloodlossdd=2;
	if bloodLoss = ">1000ml" then bloodlossdd=3;
	
	if apgar1cat = "low" then apgar1catdd=1;
	if apgar1cat = "medium" then apgar1catdd=2;
	if apgar1cat = "high" then apgar1catdd=3;
	
	if apgar5cat = "low" then apgar5catdd=1;
	if apgar5cat = "medium" then apgar5catdd=2;
	if apgar5cat = "high" then apgar5catdd=3;
	
	if perineum = "intact" then perineumdd=1;
	if perineum = "1DT" then perineumdd=2;
	if perineum = "2DT" then perineumdd=3;
	if perineum = "3DT" then perineumdd=4;
	if perineum = "4DT" then perineumdd=4;
	if perineum = "episiotomy" then perineumdd=5;
	
	if episiotomy = "no" then episiotomydd=0;
	if episiotomy = "yes" then episiotomydd=1;
	
	if fetalSurv = "none" then fetalSurvdd=1;
	if fetalSurv = "intermittent" then fetalSurvdd=2;
	if fetalSurv = "continuous" then fetalSurvdd=3;
	
	if GBS = "negative" then GBSdd=0;
	if GBS = "positive" then GBSdd=1;
	
	if resuscitation = "none" then resuscdd=0;
	if resuscitation = "inflat" then resuscdd=1;
	if resuscitation = "oxygen" then resuscdd=1;
	if resuscitation = "stimul" then resuscdd=1;
	if resuscitation = "ventil" then resuscdd=1;
	if resuscitation = "yes" then resuscdd=1;
	
	if placentalPhase = "spontaneous" then placentaldd=1;
	if placentalPhase = "activePolicy" then placentaldd=2;
	if placentalPhase = "manualRevisio" then placentaldd=3;
	
	if birthingPosition = "LithotomyInLegBraces" then positiondd=1;
	if birthingPosition = "HandsOnKnees" then positiondd=2;
	if birthingPosition = "LithotomyWithoutLegSupport" then positiondd=3;
	if birthingPosition = "birthingStool" then positiondd=4;
	if birthingPosition = "lyingInBath" then positiondd=5;
	if birthingPosition = "lyingOnSide" then positiondd=6;
	if birthingPosition = "squatting" then positiondd=7;
	if birthingPosition = "standing" then positiondd=8;
	
	if bathBirth = "no" then bathBirthdd=0;
	if bathBirth = "yes" then bathBirthdd=1;
	
	if gender = "girl" then genderdd=0;
	if gender = "boy" then genderdd=1;
	
	if delivery = "nonspontan" then deliverydd=0;
	if delivery = "spontaneous" then deliverydd=1;
	
run;

PROC FREQ DATA=vbov.vbovRQ3MI;
    TABLE episiotomydd / CHISQ; 
RUN;

PROC FREQ DATA=vbov.vbovRQ3MI;
    TABLE resuscdd * mural / CHISQ; 
RUN;

proc means data=vbov.vbovRQ3MI n mean std min max;
   var birthWeight;
run;


proc univariate data=vbov.vbovRQ3MI normal;
   var birthWeight;
   histogram birthWeight / normal;
   inset mean std skewness kurtosis / position=ne;
run;

proc univariate data=vbov.vbovRQ3MI normal;
   var maternalAge;
   histogram maternalAge / normal;
   inset mean std skewness kurtosis / position=ne;
run;


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for BLOODLOSS */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class bloodlossdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var bloodlossdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(bloodlossdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = bloodlossdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = bloodlossdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = bloodlossdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = bloodlossdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = bloodlossdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = bloodlossdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = bloodlossdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model bloodlossdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;

*PO assumption;
proc logistic data=imputed3;
   class bloodlossdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model bloodlossdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / link=logit;
run;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class bloodlossdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model bloodlossdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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




/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for APGAR1MIN */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class apgar1catdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var apgar1catdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(apgar1catdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = apgar1catdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = apgar1catdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = apgar1catdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = apgar1catdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = apgar1catdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = apgar1catdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = apgar1catdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model apgar1catdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class apgar1catdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model apgar1catdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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


/*-----------------------------------------------------------*/
/* 1) MULTIPLE IMPUTATION with PROC MI for APGAR5MIN */
/*-----------------------------------------------------------*/
proc mi data=vbov.vbovRQ3MI out=imp_data nimpute=10 seed=2025 ;
   class apgar5catdd muraldd langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;

   var apgar5catdd muraldd maternalAge langdd relationshipdd educationdd miscarriage
   		abortion conceptiondd;
   		
   fcs logistic(apgar5catdd = muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(langdd = apgar5catdd muraldd relationshipdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(relationshipdd = apgar5catdd muraldd langdd educationdd miscarriage abortion conceptiondd);
   fcs logistic(educationdd = apgar5catdd muraldd langdd relationshipdd miscarriage abortion conceptiondd);
   fcs logistic(miscarriage = apgar5catdd muraldd langdd relationshipdd educationdd abortion conceptiondd);
   fcs logistic(abortion = apgar5catdd muraldd langdd relationshipdd educationdd miscarriage conceptiondd);
   fcs logistic(conceptiondd = apgar5catdd muraldd langdd relationshipdd educationdd miscarriage abortion);
   fcs regression(maternalAge = apgar5catdd muraldd langdd relationshipdd educationdd miscarriage abortion conceptiondd);
run;

data imputed3;
    set imp_data;
    where _Imputation_ = 3;
run;

PROC REG DATA=imputed3;
   model apgar5catdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
   		conceptiondd / VIF;
RUN;


ods select none;
proc logistic data=imp_data;
   by _Imputation_;
   class apgar5catdd muraldd langdd relationshipdd educationdd miscarriage abortion 
   		conceptiondd / param=reference ref=first;
   model apgar5catdd = maternalAge muraldd langdd relationshipdd educationdd miscarriage abortion
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


