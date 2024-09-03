*  FILENAME IS  Chapter1728Jan2009.SAS   ;

*  INPUT YOUR LIBRARY (FOLDER) NAME HERE  ;
libname mylib "C:\Book3\Web\CsvData";run;

*  IMPORT NURSINGHOME DATA;

Data NurseDat01;set NHome;
if CRYEAR = 2001;
if SQRFoot > 5;
Rate = 100*TPY/Numbed;
if Rate >50;
logNUMBED=log(NUMBED);
logSQRFOOT=log(SQRFOOT);
run;run;


*  GLM MODEL - GAMMA;
proc genmod data=NurseDat01 ;
  model TPY = logNUMBED logSQRFOOT PRO TAXEXEMPT 
            SELFFUNDINS MCERT URBAN /dist=gamma link=log type3;run;

*  GLM MODEL - INVERSE GAUSSIAN;
proc genmod data=NurseDat01 ;
  model TPY = logNUMBED logSQRFOOT PRO TAXEXEMPT 
            SELFFUNDINS MCERT URBAN /dist=IG link=log type3;run;




