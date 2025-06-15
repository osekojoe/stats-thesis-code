libname vbov '/home/u63688618/uhasselt/thesis/combined';


FILENAME REFFILE '/home/u63688618/uhasselt/thesis/combined/vbov_comb2.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=vbov.vbovcomb2;
	GETNAMES=YES;
RUN;


/* FURTHER DATA CLEANING OF R DATA --------------------------------*/

data vbov.vbovsas;
set vbov.vbovcomb2;
	if perineum = "NA" then perineum="";
	if gender = "NA" then gender="";
	if fetalSurv = "NA" then fetalSurv="";
	if episiotomy = "NA" then episiotomy="";
	if bloodLoss = "NA" then bloodLoss="";
	if bathBirth = "NA" then bathBirth="";
	if birthingPosition = "NA" then birthingPosition="";
	if placentalPhase = "NA" then placentalPhase="";
	if apgar1cat = "NA" then apgar1cat="";
	if apgar5cat = "NA" then apgar5cat="";
	
	if apgar1min = "NA" then apgar1min="";
	if apgar5min = "NA" then apgar5min="";
	if resuscitation = "NA" then resuscitation="";
	if birthWeight = "NA" then birthWeight="";
	if birthingResponsibility = "NA" then birthWeight="";
	if pregnancyOnset = "NA" then pregnancyOnset="";
	if mothertongue = "" then mothertongue="";
	if abortion = "NA" then abortion="";
	if miscarriage = "NA" then miscarriage="";
	
	if relationship = "NA" then relationship="";
	if educLevel = "NA" then educLevel="";
	if mural = "extramural" then muralnum ="1";
	if mural = "intramural" then muralnum="0";
run;

data vbov.vbovsas;
set vbov.vbovsas;
	if mothertongue = "NA" then mothertongue="";
run;


*exclude NVT from episiotomy;
DATA vbov.episiotdat;
   SET vbov.vbovsas;
   WHERE episiotomy NE "NVTan";
RUN;

*exclude NVT and unknown from birthingPosition;
DATA vbov.birthdat;
   SET vbov.vbovsas;
   WHERE birthingPosition NE "NVTandSectio" and birthingPosition NE "unknown";
RUN;

*exclude NVT and unknown from placentalPhase;
DATA vbov.placentaldat;
   SET vbov.vbovsas;
   WHERE placentalPhase NE "NVTandSectio" and placentalPhase NE "unknown";
RUN;

*exclude unknown from GBS;
DATA vbov.gbsdat;
   SET vbov.vbovsas;
   WHERE GBS NE "unknown";
RUN;

*exclude unknown from bloodLoss;
DATA vbov.bloodlossdat;
   SET vbov.vbovsas;
   WHERE bloodLoss NE "unknown";
RUN;

*exclude PEEP CAAP from resuscitation;
DATA vbov.resuscdat;
   SET vbov.vbovsas;
   WHERE resuscitation NE "PEEP C";
RUN;


/* ----CHISQ TEST OF INDEPENDENCE -------------------------------------*/
/* Test for independence between each gestational outcome and */
/* covariate -- sociodemographic and obstetric factors --------------------*/

* delivery vs mural-setting;

PROC FREQ DATA=vbov.vbovsas;
    TABLE delivery*mural / CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT relrisk measures; 
RUN;


* perineum vs mural-setting;

PROC FREQ DATA=vbov.vbovsas;
    TABLE perineum*mural / CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT relrisk measures; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE perineum / CHISQ MISSING; 
RUN;

*episiotomy vs mural-setting;
PROC FREQ DATA=vbov.episiotdat;
    TABLE episiotomy*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.episiotdat;
    TABLE episiotomy/ CHISQ ; 
RUN;

*fetalSurv vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE fetalSurv*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE fetalSurv / CHISQ; 
RUN;

/*
*epidural vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE mural*epidural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;
*/

/*
*delivery vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE mural*delivery / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;
*/


* bloodLoss vs mural-setting -- trend test since ordinal;
PROC FREQ DATA=vbov.bloodlossdat;
    TABLE bloodLoss*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.bloodlossdat;
    TABLE bloodLoss / CHISQ; 
RUN;

PROC FREQ DATA=vbov.bloodlossdat;
    TABLE bloodLoss*muralnum / trend measures; 
RUN;

* GBS vs mural-setting;
PROC FREQ DATA=vbov.gbsdat;
    TABLE GBS*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

* resuscitation vs mural-setting;
PROC FREQ DATA=vbov.resuscdat;
    TABLE resuscitation*mural / CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.resuscdat2;
    TABLE resuscitation*mural / CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;


* waterbirth vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE bathBirth*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

*gender vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE gender*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

* birthingPosition vs mural-setting;
PROC FREQ DATA=vbov.birthdat;
    TABLE birthingPosition*mural / CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;


* placentalPhase vs mural-setting;
PROC FREQ DATA=vbov.placentaldat;
    TABLE placentalPhase*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

* apgar1min vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE apgar1cat*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE apgar1cat*muralnum / trend measures; 
RUN;

* apgar5min vs mural-setting;
PROC FREQ DATA=vbov.vbovsas;
    TABLE apgar5cat*mural / FISHER CHISQ EXPECTED DEVIATION 
    	NOROW NOCOL NOPERCENT measures; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE apgar5cat*muralnum / trend measures; 
RUN;


/* ---------- GLM UNIVARIATE ANALYSIS --------------------------*/
/* --- Considered descriptive or exploratory -------*/


*---- perineum ----- ;
proc glimmix data=vbov.vbovsas;
	class perineum mural practiceCode;
	model perineum = mural / dist=multinomial link=glogit solution;
run;

proc glimmix data=vbov.vbovsas;
	class perineum(ref="intact") mural(ref="intramural");
	model perineum = mural / dist=multinomial link=glogit solution oddsratio;
run;

*---- episiotomy - binary logit ----- ;
proc glimmix data=vbov.episiotdat;
	class mural(ref="intramural");
	model episiotomy(event="yes") = mural / dist=binary link=logit solution oddsratio;
run;



*resuscitation ;
proc glimmix data=vbov.resuscdat;
	class resuscitation(ref="none") mural(ref="intramural");
	model resuscitation = mural / dist=multinomial link=glogit solution oddsratio;
run;

proc glimmix data=vbov.vbovRQ3MI;
	class resuscdd mural(ref="intramural");
	model resuscdd(event="1") = mural / dist=multinomial link=glogit solution oddsratio;
run;


*fetalSurv;
proc glimmix data=vbov.vbovsas;
	class fetalSurv(ref="intermittent") mural(ref="intramural");
	model fetalSurv = mural / dist=multinomial link=glogit solution oddsratio;
run;

* -- delivery ;
/*
proc glimmix data=vbov.vbovsas;
	class delivery(ref="spontaneously") mural(ref="intramural");
	model delivery = mural / dist=multinomial link=glogit solution oddsratio;
run;
*/

* -- epidural - binary logit ;
/*
proc glimmix data=vbov.vbovsas;
	class mural(ref="intramural");
	model epidural(event="yes") = mural / dist=binary link=logit solution oddsratio;
run;
*/

* -- waterbirth - binary logit ;
proc glimmix data=vbov.vbovsas;
	class bathBirth(ref="no") mural(ref="intramural");
	model bathBirth = mural / dist=binary link=logit solution oddsratio;
run;

* -- gender - binary logit ;
proc glimmix data=vbov.vbovsas;
	class gender(ref="girl") mural(ref="intramural");
	model gender = mural / dist=binary link=logit solution oddsratio;
run;

* -- GBS ;
proc glimmix data=vbov.gbsdat;
	class GBS(ref="negative") mural(ref="intramural");
	model GBS = mural / dist=multinomial link=glogit solution oddsratio;
run;

proc glimmix data=vbov.gbsdat;
	class GBS(ref="negative") mural(ref="intramural");
	model GBS = mural / dist=binary link=logit solution oddsratio;
run;


*birth weight ;

ods graphics on;

proc mixed data = vbov.vbovsas plots(maxpoints=none)=all;
    class mural;
    model birthWeight = mural / solution 
          residual outp=pred_out outpm=marg_out; /* Residual and predicted values */
    *ods output FitStatistics=FitStats CovParms=CovParms; /* Save fit and covariance estimates */
run;


* -- birthingPosition ;
proc glimmix data=vbov.birthdat;
	class birthingPosition(ref="LithotomyInLegBraces") mural(ref="intramural");
	model birthingPosition = mural / dist=multinomial link=glogit solution oddsratio;
run;


*---- bloodLoss ----- ordinal -- ordinal cumlogit;

proc glimmix data=vbov.bloodlossdat;
	class bloodLoss(ref="<500ml") mural(ref="intramural");
	model bloodLoss = mural / dist=multinomial link=cumlogit solution oddsratio;
run;

* -- placentalPhase -- ;
proc glimmix data=vbov.placentaldat;
	class placentalPhase(ref="spontaneous") mural(ref="intramural");
	model placentalPhase = mural / dist=multinomial link=glogit solution oddsratio;
run;

* -- apgar1cat -- ordinal;
proc glimmix data=vbov.vbovsas;
	class apgar1cat(ref="low") mural(ref="intramural");
	model apgar1cat = mural / dist=multinomial link=cumlogit solution oddsratio;
run;


* -- apgar5cat -- ordinal ;
proc glimmix data=vbov.vbovsas;
	class apgar5cat(ref="low") mural(ref="intramural");
	model apgar5cat = mural / dist=multinomial link=cumlogit solution oddsratio;
run;



*** ------- BOWKER'S TEST OF SYMMETRY -------------------------------------------;

proc freq data=vbov.vbovsas;
  tables planned*actual / agree exact;
  *weight Count;
  title 'Bowker’s Test for Symmetry: Planned vs. Actual Delivery Location';
run;

proc freq data=vbov.vbovsas;
  tables planned*actual / agree measures;
  *weight Count;
  title 'Bowker’s Test for Symmetry: Planned vs. Actual Delivery Location';
run;


/* STUART-MAXWELL Test --------------------------------- */
proc freq data=vbov.vbovsas;
   tables planned*actual / agree cmh;
   *weight Count;
   title "Stuart-Maxwell Test: Marginal Homogeneity";
run;





/* --- research question 4 -- ----------------------------------------------*/
What effect do socio-demographic determinants have on the planned
 place of delivery, the place of delivery and on the person in charge?
*/

*1 - chi-sq tests of independence -- ----------------------------------------- ;

*--- plannned ---------;

proc freq data=vbov.vbovsas;
  tables planned*mothertongue / chisq measures;
  title 'chisq test of independence: Planned vs. mothertongue';
run;

proc freq data=vbov.vbovsas;
  tables planned*relationship / chisq measures;
  title 'chisq test of independence: Planned vs. relationship';
run;

proc freq data=vbov.vbovsas;
  tables planned*educLevel / fisher chisq measures;
  title 'chisq test of independence: Planned vs. educLevel';
run;

*---actual -----;

proc freq data=vbov.vbovsas;
  tables actual*mothertongue / chisq measures;
  title 'chisq test of independence: actual*mothertongue';
run;

proc freq data=vbov.vbovsas;
  tables actual*relationship / chisq measures;
  title 'chisq test of independence: actual vs. relationship';
run;

proc freq data=vbov.vbovsas;
  tables actual*educLevel / chisq measures;
  title 'chisq test of independence: Planned vs. educLevel';
run;

*-- birth responsibility -- ;

proc freq data=vbov.vbovsas;
  tables birthingResponsibility*mothertongue / chisq measures;
  title 'chisq test of independence: birthingResponsibility*mothertongue';
run;

proc freq data=vbov.vbovsas;
  tables birthingResponsibility*relationship / chisq measures;
  title 'chisq test of independence: birthingResponsibility vs. relationship';
run;

proc freq data=vbov.vbovsas;
  tables birthingResponsibility*educLevel / chisq measures;
  title 'chisq test of independence: birthingResponsibility vs. educLevel';
run;


/* frequency tables for variable description -----------------------------------*/

PROC FREQ DATA=vbov.vbovsas;
    TABLE mural / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE mothertongue / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE maternalAge / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE relationship / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE educLevel / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE miscarriage / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE abortion / CHISQ MISSING; 
RUN;

PROC FREQ DATA=vbov.vbovsas;
    TABLE pregnancyOnset / CHISQ MISSING; 
RUN;


