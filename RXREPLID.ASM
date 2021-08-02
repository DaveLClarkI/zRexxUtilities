REPL     TITLE 'RXREPLID - REXX SUB TO OBTAIN PARTITION REPLY ID'
         PRINT ON,GEN
RXREPLID AMODE 31
RXREPLID RMODE ANY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* Dapsco Information Systems                          Written by:     *
* 3110 Kettering Boulevard                         Dave Leonard Clark *
* Dayton, OH  45439                                  (937)294-5331    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This external REXX function  allows  a  REXX program to obtain *
* the z/VSE Reply Id for a single,  active z/VSE partition or for all *
* active partitions.   Meaning, if parameter information is supplied, *
* it is expected to be  a  valid  2-character SYSLOG identifier for a *
* single, active z/VSE partition.                                     *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      Compile this  program  as  a  standard  assembler batch phase. *
* However,  it will not,  then,  be available for just any program to *
* call -- only a REXX program will be able to call it.                *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX function  is  as easy as you would *
* invoke any other REXX function.  The following is an example:       *
*                                                                     *
*     rids = RXREPLID();          /* all outstanding reply ids   */   *
*                                                                     *
*     pid  = 'F4';                                                    *
*     rid  = RXREPLID(pid);       /* outstanding reply id for F4 */   *
*     Select                                                          *
*       When rc <> 0 Then                                             *
*         Say "RXREPLID error: rc = "rc", result = "result;           *
*       When rid == '' Then                                           *
*         Say pid "has no reply(s) outstanding.";                     *
*       Otherwise Do                                                  *
*         str = '';                                                   *
*         Do w = 1 To Words(rid)                                      *
*           Parse Value Word(rid,w) With pid '-' r .;                 *
*           Select                                                    *
*             When w == 1 Then                                        *
*               str = r;                                              *
*             When w == Words(rid) Then                               *
*               str = str', and' r;                                   *
*             Otherwise                                               *
*               str = str',' r;                                       *
*           End                                                       *
*         End                                                         *
*         Say pid "has outstanding reply(s) of" str;                  *
*       End                                                           *
*     End                                                             *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                INPUT                                *
*                                                                     *
*      On input,  the parameter value may either be omitted or may be *
* supplied as a literal or variable value which represents the SYSLOG *
* identifier of a single, active z/VSE partition.                     *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
*      On  output,   outstanding  reply  id(s)   are  returned  as  a *
* space-delimited word list  in  the  REXX "RESULT" special variable. *
* Each reply id has the following format:                             *
*                                                                     *
*   pp-rrrr   where: 'pp' is the partition identifier and             *
*                    'rrrr' is the reply identifier.                  *
*                                                                     *
*      In addition,  the RXREPLID subroutine  also sets the REXX "RC" *
* special variable with a value  indicating the success or failure of *
* the requested operation.   You  should  analyze the "RC" variable's *
* value for errors.  Those values are as follows:                     *
*                                                                     *
*    0   no errors occurred                                           *
*    4   argument error -- wrong length or invalid value              *
*    8   internal REXX function failed                                *
*          also, the failing REXX function's register 15 value        *
*          is returned in the REXX "RESULT" special variable          *
*   12   REXX environment not present                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* =================================================================== *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
PWORK8   DS    PL8                     PACKED DOUBLE-WORD
EWORK8   DS    CL8                     EDITED WORK FIELD
*
DWD      DS    D
REPLYID  DS    CL4
PID      DS    CL2
PARMDATA DS    CL8
*
SAYPARMB DS    3F                      ARXSAY PARAMETER BLOCK
SAYPARM1 DS    CL8                     'WRITE   '
SAYPARM2 DS    XL4                     ADDRESS OF SAY OUTPUT
SAYPARM3 DS    XL4                     LENGTH OF SAY OUTPUT
*
SHVPARMB DS    4F                      ARXEXCOM PARAMETER BLOCK
SHVPARM1 DS    CL8                     'ARXEXCOM'
SHVPARM2 DS    XL4
SHVCTLB  DS    XL32                    STORAGE FOR SHV CONTROL BLOCK
*
STEMLENG DS    F                       STEM NAME LENGTH
STEMNAME DS    CL9                     STEM NAME
*
VARNAME  DS    CL16                    VARIABLE NAME
VARDATA  DS    CL256                   VARIABLE NAME VALUE
*
RECBUF   DS    CL32765                 STORAGE FOR RECORD I/O AREA
*
DYNALENG EQU   *-DYNASTOR
* =================================================================== *
         EJECT
* =================================================================== *
         ARXENVB                       REXX ENVIRONMENT BLOCK
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXEFPL                       EXTERNAL FUNCTION PARAMETER LIST
         ARXEVALB                      REXX EVALUATION BLOCK
         ARXARGTB                      REXX PARSED ARGUMENT TABLE
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
         SGLOWC DSECT=YES
         MAPTIB ,
         MAPTCB DSECT=YES
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* =================================================================== *
R0       EQU   0                       ON ENTRY, REXX ENV BLOCK
R1       EQU   1                       ON ENTRY, REXX EFPL REG
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9                       REXX EXTERNAL ENTRY-POINT VECTOR
R10      EQU   10                      REXX EVALUATION BLOCK
R11      EQU   11                      SHARED VARIABLES CONTROL BLOCK
R12      EQU   12                      RXVSAMIO CSECT BASE REG
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* =================================================================== *
         PRINT OFF
         COPY  HEADER                  SUBROUTINE PROLOG
         COPY  TRAILER                 SUBROUTINE EPILOG
         COPY  IF                      STRUCTURED MACRO SET
         COPY  SELECT                  STRUCTURED MACRO SET
         COPY  WHILE                   STRUCTURED MACRO SET
         PRINT ON
* =================================================================== *
         EJECT
* =================================================================== *
RXREPLID HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)APR10',            X
               BASE=R12,SVAREA=SALEN
* =================================================================== *
         USING DYNASTOR,R13
*
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RXREPLID: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LA   R15,12                  SET ABNORMAL RETURN CODE
          B    RETR15                  GO RETURN ERROR
         ENDIF
         L     R9,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R9
         DROP  R5
*
         LR    R8,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R8
         L     R10,EFPLEVAL            REXX EVALUATION BLOCK POINTER
         L     R10,0(,R10)             REXX EVALUATION BLOCK
         USING EVALBLOCK,R10
         MVI   EVALBLOCK_EVDATA,X'00'  INIT FIRST BYTE OF RESULTS BUF
*
         LA    R11,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R11              SHARED VARIABLES CONTROL BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R0,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R1,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R3,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R0,R3,SHVPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
         OI    SHVPARMB+(4*4-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
         MVC   SHVPARM1,=C'ARXEXCOM'   SET PARM 1
         XC    SHVPARM2,SHVPARM2       CLEAR PARM 2
         XC    SHVBLOCK,SHVBLOCK       CLEAR SHARED VAR. CONTROL BLOCK
         XC    VARNAME,VARNAME         CLEAR BUFFER FOR VAR. NAME
         XC    VARDATA,VARDATA         CLEAR BUFFER FOR VAR. VALUE
         LA    R0,VARNAME              GET ADDRESS FOR SHVNAMA
         LA    R1,L'VARNAME            GET LENGTH  FOR SHVNAML
         LA    R2,VARDATA              GET ADDRESS FOR SHVVALA
         LA    R3,L'VARDATA            GET LENGTH  FOR SHVVALL
         STM   R0,R3,SHVNAMA           PUT IN SHV CONTROL BLOCK
         ST    R1,SHVUSER              COPY SHVNAML LENGTH IN SHVUSER
         ST    R3,SHVBUFL              COPY SHVVALL LENGTH IN SHVBUFL
*
* POINT TO ARGUMENT(S)
*
         L     R6,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R6
         DROP  R8
         LR    R7,R6                   REXX ARGUMENT STRING
         USING ARGSTRING,R7
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    STEMLENG,STEMLENG
*
         WHILE ARGTABLE_END,(NE,CLC),=XL8'FFFFFFFFFFFFFFFF'
          L    R2,ARGTABLE_ARGSTRING_LENGTH
          L    R4,ARGTABLE_ARGSTRING_PTR

          IF   STEMLENG,(NE,CLC),=F'0' ALREADY GOT THIS ARGUMENT?
           LA  R5,4                     SET RESULT CODE
           B   RETERR                   GO RETURN ERROR
          ENDIF                        ENDIF
          ST   R2,STEMLENG             SAVE STEM NAME LENGTH
          MVC  STEMNAME,0(R4)          SAVE STEM NAME
          TR   STEMNAME,TRTBL          TRANSLATE TO UPPER-CASE
          LA   R3,STEMNAME-1           POINT TO LAST CHARACTER
          AR   R3,R2                    OF STEM NAME VALUE
          IF   STEMLENG,(H,CLC),=F'9',OR,  IF LEN > 9                  X
               0(R3),NE,C'.'           OR LAST CHAR NOT A PERIOD
           LA  R5,4                     SET RESULT CODE
           B   RETERR                   GO RETURN ERROR
          ENDIF                        ENDIF

          LA   R6,L'ARGTABLE_END(,R6)  ADVANCE TO NEXT ARGUMENT
          LR   R7,R6
         WEND
*
         DROP  R6
         DROP  R7
*
* READY TO PROCEED?
*
         IF    STEMLENG,(EQ,CLC),=F'0' DIDN'T GET ANY ARGUMENTS?
          LA   R5,4                     SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
* OUTPUT TO SYSLST -- ARXSAY
*
*        LA    R0,SAYPARM1             GET ADDRESS OF PARM 1
*        LA    R1,SAYPARM2             GET ADDRESS OF PARM 2
*        LA    R2,SAYPARM3             GET ADDRESS OF PARM 3
*        STM   R0,R2,SAYPARMB          PUT IN PARM BLOCK
*                                      SET END OF PARM BLOCK AT
*        OI    SAYPARMB+(4*3-4),X'80'  PARM LEN X PARM CNT -ZERO OFFSET
*
*        MVC   SAYPARM1,=C'WRITE   '   SET PARM 1
*        LA    R0,VARDATA              GET ADDRESS FOR PARM 2
*        LA    R1,L'VARDATA            GET LENGTH FOR PARM 3
*        STM   R0,R1,SAYPARM2          PUT IN PARMS 2 AND 3
*
*        MVC   VARDATA(7),=CL7'Message'
*        LA    R3,7                    SET LENGTH OF OUTPUT
*        BAS   R5,SAYIT                GO SAY IT
*
* BEGIN PROCESSING
*
         XR    R5,R5                   POINT TO LOWCORE
         USING SYSS00,R5
         L     R6,ATIBATAB             POINT TO TIB
         LA    R6,128(,R6)             SKIP OVER SYSTEM TASKS
         DROP  R5
*        USING TIBADR,R3
*        USING TCBADR,R8
*        USING TCBXADR,R9
         WHILE 0(R6),NE,X'FF'          LOOP ON TIB ADDR TABLE
*
         L     R3,0(,R6)                GET TIB ADDRESS
         IF    R3,(NZ,LTR)R3            IF NOT ZERO
*         L    R8,TIBTCB                 GET TCB ADDRESS
          L    R8,8(,R3)                 GET TCB ADDRESS
*         L    R9,TCBATCBE               GET TCBX ADDRESS
          L    R9,X'1C0'(,R8)            GET TCBX ADDRESS
*         IF   =H'0',(NZ,CLC),TCSRBM     IF RBM NOT ZERO
          IF   =H'0',(NZ,CLC),X'158'(R9) IF RBM NOT ZERO
*          L   R4,TCSQHDR+8               GET WAITING MSG HEADER
           L   R4,X'13C'+8(R9)            GET WAITING MSG HEADER
           IF  R4,(NZ,LTR),R4             IF THERE IS A MESSAGE QUEUE
            IF PARMDATA(2),(EQ,CLC),298(R4) AND MATCHES REQUESTED PTN
             MVC REPLYID(4),301(R4)         SAVE REPLYID
             PACK DWD,REPLYID(4)            PACK IT
             CVB R15,DWD                    CONVERT TO BINARY
            ENDIF                          ENDIF
           ENDIF                          ENDIF
          ENDIF                          ENDIF
         ENDIF                          ENDIF
         LA    R6,4(,R6)                SKIP TO NEXT ENTRY
*
         WEND                          LOOP ON TIB ADDR TABLE
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   NI    QRYFLAG,X'FF'-SET_KLZ   REG2ZON3 NO LEADING ZEROES
         LR    R2,R15                  COPY R15 TO R2
         LA    R3,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         ST    R2,EVALBLOCK_EVLEN      LENGTH OF CONVERTED RESULT
*
         LR    R2,R5                   COPY R5 TO R2
         LA    R3,VARDATA              POINT TO VARIABLE DATA AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         LA    R1,VARDATA              POINT BACK TO VAR DATA AREA
         STM   R1,R2,SHVVALA           PUT IN SHV CONTROL BLOCK
*
         LA    R3,2                    SET LENGTH OF VARIABLE NAME
         MVC   VARNAME+0(3),=C'RC '    SET VARIABLE NAME
         BAS   R5,SETVAR               CREATE THE VARIABLE
* ------------------------------------------------------------------- *
         XR    R15,R15                 CLEAR REG15 FOR RETURN CODE
RETR15   TRAILER RETCODE=R15,SVAREA=SALEN
* =================================================================== *
         EJECT
* =================================================================== *
REG2ZON3 DS    0H    CONVERT REG2 TO LEFT-SIGN, ZONED-DECIMAL AT REG3
*
         CVD   R2,PWORK8               CONVERT REG2 TO PACKED-DECIMAL
         IF    QRYFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          MVC  EWORK8(7),=X'40212020202060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
         ELSE
          MVC  EWORK8(7),=X'40202020212060'  LOAD EDIT PATTERN
          LA   R1,EWORK8+5             POINT TO SIGNIFICANCE BYTE
         ENDIF
         LA    R2,EWORK8+6             POINT TO SIGN BYTE (& IGNORE IT)
         IF    EWORK8(7),(M,EDMK),PWORK8+5   IF NEGATIVE NUMBER
          BCTR R1,0                    DECR. R1 SO THAT WE CAN
          MVI  0(R1),C'-'                FLOAT A LEFT MINUS SIGN
         ENDIF
         IF    QRYFLAG,(ON,TM),SET_KLZ KEEP LEADING ZEROES?
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
          LA   R2,4
         ELSE
          SR   R2,R1                   CALC NO. OF CHARS TO MOVE
         ENDIF
         BCTR  R2,0                    DECR. LEN FOR ZERO REF.
         EX    R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
*
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
ZON2REG3 DS    0H    CONVERT ZONED-DECIMAL AT REG2 (L'REG3) INTO REG3
*
         BCTR  R3,0                    DECR. FOR ZERO OFFSET
         EX    R3,PKINPT               PACK 0(?,R2) FOR LENGTH OF R3
         CVB   R3,PWORK8               CONVERT PACKED-DECIMAL INTO REG3
*
         BR    R15
PKINPT   PACK  PWORK8,0(0,R2)          (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H    GET REXX VARIABLE VALUE
         XR    R5,R5                   ZERO RETURN CODE REGISTER
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          LA   R5,8                     SET RESULT CODE
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           IC  R15,SHVRET                SAVE ARXEXCOM CC
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R2
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H    SET REXX VARIABLE VALUE
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         LA    R1,SHVPARMB             POINT TO PARAMETER BLOCK
         L     R15,ARXEXCOM            POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXEXCOM
         IF    R15,(NZ,LTR),R15,OR,    IF NON-ZERO RETURN CODE         X
               SHVRET,NE,SHVCLEAN      OR NON-ZERO CONDITION CODE
          IF   VARNAME(3),(NE,CLC),=C'RC '  IF NOT 'RC', REPORT ERRORS
           IF  R15,(NZ,LTR),R15          IF NON-ZERO RETURN CODE
            LA R5,8                       SET RESULT CODE
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
           IF  SHVRET,NE,SHVNEWV         IF NOT NEW VARIABLE
            IC R15,SHVRET                 SAVE ARXEXCOM CC
            LA R5,8                       SET RESULT CODE
            B  RETERR                     GO RETURN ERROR
           ENDIF                         ENDIF
          ENDIF                         ENDIF
         ENDIF                         ENDIF
*
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* =================================================================== *
TRTABNUM DC    256X'FF'
         ORG   TRTABNUM+C'0'
         DC    10X'00'
         ORG
*
SALEN    DC    A(DYNALENG)
         LTORG
* =================================================================== *
         END
