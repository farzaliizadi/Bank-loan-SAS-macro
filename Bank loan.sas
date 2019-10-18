libname home "D:\desktop\sas";
/*doing proc contents*/
proc contents data=home.hmeq;
run;

%macro get_var_dist (data_nm , categ_cut_off);
/* Taking list of variables in the dataset along with type*/
proc contents data=&data_nm. out=_content_;
run;
/* Note - how data_nm is getting resolved. */
/* Fiding number of variables to define loop */
proc sql noprint;
select count(*) into: nvars from _content_;
quit;
/* Note this is another way of defining macro variables */

%put "No of Variables is  " &nvars. ;
/* Note this can be be useful way to detect the value of macro variables.*/

%do i=1 %to &nvars. ;
data _null_;
set _content_;
if _n_ = &i. then do;
        call symput("variable", name);
        call symput("var_type",type);
end;
run;


/* Note - 
   1: One more way of defining variables.
   here information is being passed from general SAS dataset 
   to SAS macros using call symput
*/

%put "Variable is  " &variable. ;
%put "Variable Type is  " &var_type. ;

proc sql noprint;
        select count(distinct &variable) into: distinct_categories 
        from &data_nm.;
        quit;

%put "Number of distinct categories is  &distinct_categories." ;

/*syntax of writing if else condition */
/* %do ....  %end; defines the block */
/* Missing command below includes missing value as a category */
/* without %eval, it can throw errors */
/* if comparison involves floating numbers, you will need to use %sasevalf */

	%if %eval(&distinct_categories.) <=  %eval(&categ_cut_off.) %then %do;
	Title "Freq Dist for Variable &variable. as number of distinct 
	category is &distinct_categories.";  
	proc freq data=&data_nm. ;
	table &variable. / missing ; 
	run;
	%end;

	%if %eval(&var_type.) =  1 %then %do;
	Title "Univariate Dist for Variable = &variable. ";  
	proc univariate data=&data_nm. plot;
	var &variable.  ; 
	run;
	%end;
%end;
%mend get_var_dist;


%get_var_dist(data_nm=home.hmeq, categ_cut_off=20);

/* some plot*/

proc gchart data=home.hmeq;
vbar ninq/type =percent;
run;

proc gchart data=home.hmeq;
vbar bad/type =percent;
run;

proc gchart data=home.hmeq;
vbar reason/type =percent;
run;

proc gchart data=home.hmeq;
vbar derog/type =percent;
run;

proc gchart data=home.hmeq;
pie delinq/type = sum;
run;

proc gchart data=home.hmeq;
pie loan/type = sum;
run;

proc means data=home.hmeq;
class bad;
run;
/* Checking  for  missing value count*/

data chk;
set home.hmeq;
where clage=.;
run;

/*Find the significance of character variables on the dependent variable*/

proc freq data=home.hmeq;
tables yoj*bad/chisq;
run;

/*filling missing values*/


DATA nov;
set  home.hmeq;
run;

/*Creating Id column*/

data dec;
set nov;
IDNew=_n_;
run;

/* To check if IDnew was added*/
proc print data=dec;
run;
/* Kepping REASON JOB IDNew to create Indicator variables first step */

data jan;
keep REASON JOB IDNew;
set Dec;
run;
/*Designing indicator second step */
proc glmmod data=jan outdesign=GLMDesign outparm=GLMParm;
class JOB REASON;
model IDNew= JOB REASON;
run;

data feb(drop=JOB REASON);
set dec;
run;
 /* Creating IDNew column for indicator data in order to merge two data set*/
data glm;
set  Glmdesign;
IDNew=_n_;
run;

proc sort data=feb;
by IDNew;
run;

proc sort data=glm;
by IDNew;
run;

data july;
merge  feb glm;
by IDNew;
run; 


data aug(drop=Value_grp Value_grp2);
set july;
run;

proc print data=aug (obs=10);
run;

ods graphics on;
proc logistic data=aug  desc plots(only)=roc;
model bad= value debtinc mortdue yoj derog loan clage delinq ninq clno;
run;
quit;


data MODEL_DEV MODEL_VAL;
  set aug;
  if ranuni(1234567)<=0.6 THEN OUTPUT MODEL_DEV;
  ELSE                         OUTPUT MODEL_VAL;
run;

%macro get_preparation(data_nm);
 OPTIONS CENTER PAGENO=1 DATE;
proc contents data=&data_nm. out=_content_;
run;

proc sql noprint;
select count(*) into: nvars from _content_;
quit;
%put "No of Variables is  " &nvars. ;
%do i=1 %to &nvars. ;
data _null_;
set _content_;
if _n_ = &i. then do;
        call symput("variable", name);
        call symput("var_type",type);
end;
run;

%put "Variable is  " &variable. ;
%put "Variable Type is  " &var_type. ;

proc sql noprint;
select count(distinct &variable) into: distinct_categories 
from &data_nm.;
quit;
%put "Number of distinct categories is  &distinct_categories." ;

    
	%if %eval(&var_type.) = 1 %then %do;
       PROC RANK DATA =&data_nm. (KEEP=bad &variable.)
               GROUPS = 10
                  OUT = Junk_1;
            RANKS NEWVBLE;
            VAR &variable.;
       RUN;  
    
       PROC SUMMARY DATA =JUNK_1  NWAY ;
            CLASS NEWVBLE ;
            VAR bad &variable.  ;
            OUTPUT OUT = JUNK_2
                  MEAN =
                  MIN(&variable.)=MIN
                  MAX(&variable.)=MAX
                     N = NOBS;
         RUN;

           DATA JUNK_2;
            SET JUNK_2;
            IF bad NE 0 THEN
               LOGIT = LOG (bad/(1+ bad));
            ELSE IF bad = 0 THEN LOGIT = . ;
            RUN;                           

        PROC SQL NOPRINT;
        CREATE TABLE JUNK_3 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(bad) AS bad
        FROM &data_nm.
        WHERE &variable.=.;
        run;

        DATA JUNK_3;
        SET JUNK_3;
        LOGIT=LOG(bad/(1-bad));
       RUN;

       DATA JUNK_4;
        SET JUNK_2 JUNK_3;
       RUN;
            
       PROC PLOT DATA = JUNK_4;
            TITLE1 "Plot of Logit(Response) by &&variable" ;
            PLOT  LOGIT*bad;
       RUN;

        proc plot data=junk_4;
        plot bad*&variable.;
        plot _freq_*&variable.;
        TITLE2 "Plot of Response by &&variable." ;
        run;

       PROC PRINT DATA = JUNK_4 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped &&variable." ;
            VAR NEWVBLE NOBS &variable. MIN MAX bad ;
            LABEL NEWVBLE = "&variable. Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX';
                     run;
					 proc print data =junk_4;run;
                     %end;
 %end;
%mend get_preparation;

options symbolgen mlogic mprint;
%get_preparation(data_nm= MODEL_DEV);

DATA MODEL_DEV;
set MODEL_DEV;
if CLAGE=. then CLAGE=99.325;
if CLNO=. then CLNO=31.3878;
if DEBTINC=. then DEBTINC=45.4824;
if DELINQ=. then DELINQ=0.25;
if DEROG=. then DEROG=0.1;
if LOAN=. then LOAN=0;
if MORTDUE=. then MORTDUE=54175.95;
if NINQ=. then NINQ= 1;
if VALUE=. then VALUE=152898.99;
if YOJ=. then YOJ= 25.1988;
run;

/*value variable signifcanceto keep or not*/
proc freq data= model_dev;
tables value*bad/nocol nofreq nocum
nopercent chisq;
run;
/*Value is not significant*/
proc freq data= model_dev;
tables value*bad/ chisq;
run;

/*clage variable signifcance*/
proc freq data= model_dev;
tables yoj*bad/nocol nofreq nocum
nopercent chisq;
run;
/*clno variable signifcance*/
proc freq data= model_dev;
tables clno*bad/nocol nofreq nocum
nopercent chisq;
run;

proc contents data=model_dev;
run;


/* Knowing bi-variate strength of the independent variables in explaining the dependent variable*/
/*We should check square chi square the bigger the stronger effect to the dependent variable*/
proc logistic data = MODEL_DEV;
model bad = clage clno debtinc delinq derog loan mortdue ninq value yoj
            col1 col2 col3 col4 col5 col6 col7 col8 col9
/selection = stepwise maxstep=8 details;
ods output EffectNotInModel = log_data ;
run;
proc logistic data = MODEL_val;
model bad = clage clno debtinc delinq derog loan mortdue ninq value yoj
            col1 col2 col3 col4 col5 col6 col7 col8 col9
/selection = stepwise maxstep=8 details;
ods output EffectNotInModel = log_data ;
run;
/*Col   col9 no effect to the model automatically been removed*/

/* First cycle of VIF we removed promcntcardall*/
/* Close look at complete multi collinearity removal output*/

proc reg  data = MODEL_DEV; 
model bad = clage clno debtinc delinq derog loan mortdue ninq value yoj  
/ vif collin; ODS OUTPUT CollinDiag = collin_data (drop = intercept) ParameterEstimates = para_data; run;

/* First cycle of VIF we removed */
/* Close look at complete multi collinearity removal output*/

/*Model with 10 variables to show the significance of all the variables coming after multicollinearity test*/

/*final model variable selection using step wise regression */
proc logistic data = MODEL_DEV outmodel = model_1;
    model bad(event  = '1') = clage clno debtinc delinq derog loan
    mortdue ninq value yoj/
selection = stepwise maxstep=8 details;
ods output EffectNotInModel = log_data ;
run;

/*Developing final model variable coefficients on validation data set*/

proc logistic data =  MODEL_VAL;
    model bad(event  = '1') = clage clno debtinc delinq derog loan
    mortdue ninq value yoj;
run;
/*Dropping clno mortdue yoj  extra for me*/

proc logistic data =  MODEL_VAL;
    model bad(event  = '1') = clage  debtinc delinq derog loan
     ninq value;
run;
/*  drop value for me*/
proc logistic data =  MODEL_VAL;
    model bad(event  = '1') = clage  debtinc delinq derog loan
     ninq;
run;

/* Take a look at coefficient stability worksheet */


proc logistic data =  MODEL_DEV;
    model bad(event  = '1') = clage clno debtinc delinq derog loan    /*this is extra for me*/
    mortdue ninq value yoj;
run;
_________________________________________________________________
proc logistic data = MODEL_DEV outmodel = model_1;
    model bad(event  = '1') = clage clno debtinc delinq derog loan
    mortdue ninq value yoj/
selection = stepwise maxstep=8 details;
ods output EffectNotInModel = log_data ;
run;
Data crsl;
set aug;
rand_indiactor=ranuni(10);
rand_gp=floor(10*rand_indiactor);
run;
proc freq data=crsl;
table rand_gp;
run;
/*
 * 0- 1200
 * 1- 2400
 * 2- 3600
 * 3- 4800
 * 4- 5960
 */

%macro k_fold ;
proc datasets library = work nodetails nolist; 
delete kfold_val; 
run;
%do i = 0 %to 4; 
proc logistic data = crsl outmodel = model_1;
    model bad(event  = '1') =clage clno debtinc delinq derog loan
    mortdue ninq value yoj;
where rand_gp NE &i. ;
run;

/* Generating score in the test data */
proc logistic inmodel = model_1;
score data= crsl out = kfold_val_temp; 
where rand_gp=&i. ;
run;

proc append base = kfold_val data = kfold_val_temp force; run;
%end;
%mend k_fold;

option symbolgen mlogic mprint;
%k_fold;

data predicted2; 
set kfold_val;
P_0_D = round(P_0*1000,0.1);
run;

proc rank data=predicted2 out=practice2 group=10 ties=low ;
var P_0_D;
ranks P_Final;
run;

proc print data=practice2(obs=50);
var P_0_D P_Final ;
run;

proc sql ;
select P_final, min(P_0_D)as Min_score, max(P_0_D)as Max_score, sum(1*bad) as responder, count(bad) as population
from practice2
group by P_final
order by P_final
;
quit;

data lift;
input u t s;
cards;
0.05   5.0   26.1
0.1  10.0  43.5
0.15   15.0  55.0
0.2   20.0  63.9
0.25   25.1  71.0
0.3   30.1  76.9
0.35   35.2  82.1
0.4   40.3  85.7
0.45   45.6  88.7
0.5  49.6  90.9
0.55  55.9  93.3
0.60  60.7  94.8
0.65  63.8  95.5
0.7  71.2  97.1
0.75  74.7  97.7
0.8  78.6  98.1
0.85  84.2  98.7
0.9  92.0  99.4
 1  100.0 100.0
run;
proc print data=lift;
run;

proc gplot data = lift;
plot t*u
     s*u
      /overlay vaxis=axis1 haxis=axis2 nolegend;
run;



/*Keeping model coefficients in a data set*/

proc logistic data = model_dev outmodel = model_1;
    model bad(event  = '1') = clage clno debtinc delinq derog loan
    mortdue ninq value yoj;
run;

/* Generating score in the test data */

proc logistic inmodel = model_1;
score data=  model_dev out = predicted; 
run;

/* Proc contents of model_dev data just to see what extra fields were added */

proc contents data=predicted;
run;
/* Understand how proc logistic generates score in the dataset
And what is the score actually */

data predicted1; 
set predicted;
P_0_D = round(P_0*1000,0.1);
 1 -7.7401+  
CLAGE* -0.00623 +
CLNO*-0.0188 +
DEBTINC* 0.1846+ 
DELINQ* 0.7827+
DEROG* 0.6809+ 
LOAN*-0.00002+ 
MORTDUE* -7.58E-6 +
NINQ* 0.1203+ 
VALUE* 8.235E-6 + 
YOJ* -0.0201; 
prob=exp(log_odds)/(1+exp(log_odds));
run;

proc print data=predicted (obs=10);
run;

/*P_0=Probability of '0' in the model
P_1=Probability of 1
prob =e(logs_odds/(1+exp(log_odds) this is the derived probability value using equation which should be equal to P_1 in the predicted dataset(almost equal)
P_0_D=It is P_0 Multiplied by 1000 to make it easier to read for the user*/

proc print data=predicted (obs=10);
run;


proc print data=predicted (obs=10);
var  clage clno debtinc delinq derog loan
    mortdue ninq value yoj;
P_0
P_1
log_odds
Prob
P_0_D;
run;


/*Creates ten deciles for the score variable on the dataset.
The decile will be ascending(P_0) 
Please note lower value of P_0 is same as high value of P_1, hence more of outcome =1
*/

proc rank data=predicted  out=practice group=10 ties=low;
var P_0_D;
ranks P_Final;
run;

/*Check how does the actual score and ranked variable look like */
proc print data=practice(obs=50);
var P_0_D P_Final ;
run;


/*Getting figures to calculate KS and Gini in Development datset */
proc sql ;
select P_final, min(P_0_D)as Min_score, max(P_0_D)as Max_score, sum(1*bad) as responder, count(bas) as population
from practice
group by P_final
order by P_final
;
quit;

/*Scoring the validation dataset – using coefficients obtained on development data*/

proc logistic inmodel = model_1;
score data=model_val out = predicted2; 
run;

data predicted; 
set predicted;
P_0_D = round(P_0*1000,0.1);
run;

proc rank data=predicted out=practice group=10 ties=low ;
var P_0_D;
ranks P_Final;
run;

proc print data=practice(obs=10);
var P_0_D P_Final ;
run;


















