* Written by R;
*  foreign::write.foreign(df, "icd9toyllcause.txt", "import_icd9.sas",  ;

DATA  rdata ;
LENGTH
 icd_code $ 6
 icd_name $ 222
 yll_cause $ 23
 yll_cause_name $ 70
 cause_outline $ 10
 GUC $ 7
 male $ 1
 female $ 1
;

INFILE  "icd9toyllcause.txt" 
     DSD 
     LRECL= 323 ;
INPUT
 icd_code
 icd_name
 yll_cause
 yll_cause_name
 cause_outline
 GUC
 male
 female
 yll_amin
 yll_amax
;
RUN;
