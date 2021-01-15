* Written by R;
*  foreign::write.foreign(df, "icd10toyllcause.txt", "import_icd9.sas",  ;

DATA  rdata ;
LENGTH
 icd_code $ 7
 icd_name $ 198
 yll_cause $ 23
 yll_cause_name $ 70
 cause_outline $ 10
 GUC $ 7
 male $ 1
 female $ 1
;

INFILE  "icd10toyllcause.txt" 
     DSD 
     LRECL= 301 ;
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
