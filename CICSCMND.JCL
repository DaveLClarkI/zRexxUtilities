* $$ JOB JNM=CICSCMND,DISP=D,CLASS=C
* $$ LST LST=SYSLST,DISP=D,CLASS=Z
// JOB    CLOSE AND REOPEN A CICS FILE
// EXEC   REXX=CICSCMND,                                               X
               PARM='CICSPROD,CEMT SET FI(VTSTATE) CLO'
/* EOD
// IF     $RC > 0 THEN
// GOTO   EXIT
// EXEC   REXX=CICSCMND,                                               X
               PARM='CICSPROD,CEMT SET FI(VTSTATE) OPE'
/* EOD
/. EXIT
/* EOD
/& EOJ
* $$ EOJ
