* $$ JOB JNM=XXTEST,DISP=D,CLASS=C
* $$ LST LST=SYSLST,DISP=D,CLASS=Z
// JOB    TAPEPOSN   SEARCH FOR LABEL TO POSITION TAPE
// ASSGN  SYS000,201
// SETPARM LABEL='DAPACAT.BACKUP'
// EXEC REXX=TAPEPOSN,PARM='INPUT=SYS000,REW=YES,SYMBOL=LABEL',LABEL
/* EOD
/& EOJ
* $$ EOJ
