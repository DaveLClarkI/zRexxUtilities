LIBR     TITLE 'RXLIBRIO - REXX SUBROUTINE FOR LIBR CALL INTERFACE'
         PRINT ON,GEN
RXLIBRIO AMODE 24
RXLIBRIO RMODE 24
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
*      This  external  REXX  function  allows  a  REXX  procedure  to *
* interact with VSE libraries  via LIBR's internal program interface. *
* This wouldn't be important  if  it  weren't  for the fact that REXX *
* provides  no  means  of  capturing  LIBR's  punched output.   Also, *
* calling LIBR directly,  and therefore using OUTTRAP to get the LIBR *
* listed output, only works from a main task.                         *
*                                                                     *
*      If someone were  to  start  REXX  in  a  subtask then the REXX *
* OUTTRAP function would  issue  an  error  message -- preventing the *
* effective use of a direct  call to utilities that aren't REXX-aware *
* (such as programs,  like LIBR,  that you  would normally add to the *
* REXX EOJ table [ARXEOJTB]).                                         *
*                                                                     *
* NOTE:  This external function  is  only  needed  for LIBR directory *
* services (including member RENAME, DELETE,  COPY, and MOVE).   LIBR *
* member-content-type access should continue to be performed via REXX *
* EXECIO (i.e., to READ or WRITE/CREATE members).  Although you could *
* PUNCH out the member source via this function and re-CATALOG it via *
* SYSIPT, but it is easier to just use REXX EXECIO for both.          *
*                                                                     *
* =================================================================== *
*                                                                     *
*                           IMPLEMENTATION                            *
*                                                                     *
*      Create the four object decks,  first, as standard 24-bit batch *
* subroutines (actually,  three subroutines  and  one main program -- *
* but as an object deck).  Then, link this program's object deck as a *
* standard,  24-bit batch main  program  (even  though it switches to *
* 31-bit addressing mode, internally).                                *
*                                                                     *
*      RXLIBRIX     RXLIBRIO -- SYSIPT exit routine                   *
*      RXLIBRLX     RXLIBRIO -- SYSLST exit routine                   *
*      RXLIBRPX     RXLIBRIO -- SYSPCH exit routine                   *
*      RXLIBRIO     REXX Function For LIBR I/O & Statistics           *
*                                                                     *
* This main program will not, however, then be available for just any *
* program to call.  Only a REXX procedure will be able to call it.    *
*                                                                     *
* =================================================================== *
*                                                                     *
*                             INVOCATION                              *
*                                                                     *
*      Invoking this external REXX subroutine is as easy as using the *
* REXX "CALL"  statement.   The  following  demonstrates the argument *
* syntax and sample error handling.                                   *
*                                                                     *
*   sysipt.  = '';           /* required for LIBR command input  */   *
*   sysipt.0 = 2;                                                     *
*   sysipt.1 = 'ACCESS  S=lib.sublib';                                *
*   sysipt.2 = 'LISTDIR *.*         ';                                *
*                                                                     *
*   Call RXLIBRIO 'sysipt.','syslst.'<,'syspch.'>;                    *
*   If rc <> 0 Then Do                                                *
*     Say "RXLIBRIO error: rc = "rc", result = "result;               *
*     Exit 4;                                                         *
*   End                                                               *
*                                                                     *
* NOTE:  The passed stem variable  names  must include the period and *
* have a maximum length of nine characters -- including the period.   *
*                                                                     *
* =================================================================== *
*                                                                     *
*                                INPUT                                *
*                                                                     *
*      On input,  from two to three stem names must be supplied (each *
* including the period).   The first stem  variable is to contain the *
* desired LIBR command input.  The second stem variable just needs to *
* be initialized (as below) and is for the LIBR listing output.   The *
* third stem variable is optional and,  if supplied, just needs to be *
* initialized for the LIBR punch output -- if any.                    *
*                                                                     *
*   syslst.  = '';           /* required for LIBR listing output */   *
*   syslst.0 = 0;                                                     *
*                                                                     *
* =================================================================== *
*                                                                     *
*                               OUTPUT                                *
*                                                                     *
*      On output,  any LIBR listing  output  is written to the second *
* stem variable supplied.   Any LIBR punch output is written,  to the *
* third stem variable supplied.  The format of this output is exactly *
* as it would have been output directly from LIBR.                    *
*                                                                     *
*      In addition,  the RXLIBRIO subroutine  also sets the REXX "RC" *
* special variable with a value  indicating the success or failure of *
* the requested operation.   You  should  analyze the "RC" variable's *
* value for errors.  Those values are as follows:                     *
*                                                                     *
*   -5   generic error (R15 returned in REXX "RESULT" variable --     *
*          this is usually a non-zero LIBR return code)               *
*   -4   syspch stem is invalid or not found                          *
*   -3   syslst stem is invalid or not found                          *
*   -2   sysipt stem entry is invalid or not found                    *
*          (REXX "RESULT" special variable points to stem entry)      *
*   -1   sysipt stem is invalid or not found                          *
*    0   no errors occurred                                           *
*    4   argument error -- missing, too many, too long, or no period  *
*    8   internal REXX or LIBR function failed                        *
*          also, the failing function's register 15 value             *
*          is returned in the REXX "RESULT" special variable          *
*   10   CDLOAD failure (R15 returned in REXX "RESULT" variable)      *
*   12   REXX environment not present                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* ------------------------------------------------------------------- *
SAVEAREA DS    9D                      STANDARD REGISTER SAVE AREA
         COPY  RXLIBRIO
*
DYNALENG EQU   *-DYNASTOR              LENGTH OF DYNAMIC STORAGE
* =================================================================== *
         EJECT
* =================================================================== *
         ARXENVB                       REXX ENVIRONMENT BLOCK
         ARXEXTE                       REXX EXTERNAL ENTRY-POINT VECTOR
         ARXEFPL                       EXTERNAL FUNCTION PARAMETER LIST
         ARXEVALB                      REXX EVALUATION BLOCK
         ARXARGTB                      REXX PARSED ARGUMENT TABLE
         ARXSHVB                       SHARED VARIABLES CONTROL BLOCK
* =================================================================== *
         EJECT
* =================================================================== *
*        REGISTER USAGES
* ------------------------------------------------------------------- *
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
R10      EQU   10                      SHARED VARIABLES CONTROL BLOCK
R11      EQU   11                      RXLIBRIO CSECT BASE REG 1
R12      EQU   12                      RXLIBRIO CSECT BASE REG 2
R13      EQU   13                      REGISTER SAVEAREA
R14      EQU   14                      RETURN REG
R15      EQU   15                      ENTRY REG
* =================================================================== *
         EJECT
* =================================================================== *
*        MACROS                                                       *
* ------------------------------------------------------------------- *
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
RXLIBRIO HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)NOV23',            X
               BASE=(R11),SVAREA=SALEN   ALLOC GETVIS DYNAMIC STG
* ------------------------------------------------------------------- *
         USING DYNASTOR,R13            POINT TO GETVIS DYNAMIC STORAGE
*
         SAM31                         SWITCH TO 31-BIT ADDRESSING
*                                       BECAUSE OF DEBUGGER: TRAPPER
         LR    R5,R0                   REXX ENVIRONMENT BLOCK
         USING ENVBLOCK,R5             ESTABLISH ADDRESSABILITY
         IF    ENVBLOCK_ID,(NE,CLC),=C'ENVBLOCK'
          WTO  'RXLIBRIO: REQUIRES THE REXX ENVIRONMENT BLOCK'
          LHI  R15,12                  SET ABNORMAL RETURN CODE
          B    RETR15                  GO RETURN ERROR
         ENDIF
         LR    R8,R1                   EXTERNAL FUNCTION PARAMETER LIST
         USING EFPL,R8                 ESTABLISH ADDRESSABILITY
         L     R9,ENVBLOCK_IRXEXTE     REXX EXTERNAL ENTRY-POINT VECTOR
         USING ARXEXTE,R9              ESTABLISH ADDRESSABILITY
         ST    R5,SAVEENVB             SAVE ADDR. OF ENV. BLOCK
         DROP  R5                      REMOVE ADDR. TO ENV. BLOCK
*
* ACCESS TO VARIABLES -- ARXEXCOM
*
         LA    R10,SHVCTLB             STORAGE FOR
         USING SHVBLOCK,R10              SHARED VARIABLES CONTROL BLOCK
*
         LA    R0,SHVPARM1             GET ADDRESS OF PARM 1
         LA    R1,SHVPARM2             GET ADDRESS OF PARM 2
         LA    R2,SHVPARM2             GET ADDRESS OF PARM 3
         LA    R3,SHVBLOCK             GET ADDRESS OF PARM 4
         STM   R0,R3,SHVPARMB          PUT IN PARM BLOCK
         OI    SHVPLAST,X'80'          SET END OF PARM BLOCK
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
* INITIALIZE A TRANSLATE TABLE
*
         XR    R1,R1
         LA    R2,TRTBL
         WHILE R1,(LT,CH),=H'256'
          STC  R1,0(,R2)
          LA   R1,1(,R1)
          LA   R2,1(,R2)
         WEND
         LA    R2,TRTBL+C'a'           Translate lower-case
         MVC   0(9,R2),=C'ABCDEFGHI'      to upper-case
         LA    R2,TRTBL+C'j'           Translate lower-case
         MVC   0(9,R2),=C'JKLMNOPQR'      to upper-case
         LA    R2,TRTBL+C's'           Translate lower-case
         MVC   0(8,R2),=C'STUVWXYZ'       to upper-case
         LA    R2,TRTBL+X'FA'          Translate unpacked data
         MVC   0(6,R2),=C'ABCDEFGHI'      to valid hex
*
* POINT TO ARGUMENT(S)
*
         L     R6,EFPLARG              REXX PARSED ARGUMENT TABLE
         USING ARGTABLE_ENTRY,R6
         L     R7,EFPLEVAL             REXX EVALUATION BLOCK POINTER
         DROP  R8                      EFP LIST NO LONGER NEEDED
         L     R7,0(,R7)               REXX EVALUATION BLOCK
         ST    R7,SAVEEVAL               SAVE FOR LATER
         LR    R7,R6                   REXX ARGUMENT STRING
         USING ARGSTRING,R7
*
* RETRIEVE AND VALIDATE ARGUMENT(S)
*
         XC    OPTFLAG,OPTFLAG         CLEAR ALL FLAGS
*        SF    DEBUG_F                 TURN ON DEBUG
         XC    COUNTERS,COUNTERS       ZERO ALL COUNTERS
*
         WHILE ARGTABLE_END,(NE,CLC),=XL8'FFFFFFFFFFFFFFFF'

          L    R2,ARGTABLE_ARGSTRING_LENGTH
          L    R4,ARGTABLE_ARGSTRING_PTR

          SELECT
          WHEN IPTSTEMN,EQ,X'00',AND,0(R4),HT,X'40'
           SF  SYSIPTF                  INDICATE SYSIPT PRESENT
           ST  R2,IPTSTEML              SAVE IPT STEM NAME LENGTH
           MVC IPTSTEMN,0(R4)           SAVE IPT STEM NAME
           TR  IPTSTEMN,TRTBL           TRANSLATE TO UPPER-CASE
           LA  R3,IPTSTEMN-1            POINT TO LAST CHARACTER
          WHEN LSTSTEMN,EQ,X'00',AND,0(R4),HT,X'40'
           SF  SYSLSTF                  INDICATE SYSLST PRESENT
           ST  R2,LSTSTEML              SAVE LST STEM NAME LENGTH
           MVC LSTSTEMN,0(R4)           SAVE LST STEM NAME
           TR  LSTSTEMN,TRTBL           TRANSLATE TO UPPER-CASE
           LA  R3,LSTSTEMN-1            POINT TO LAST CHARACTER
          WHEN PCHSTEMN,EQ,X'00',AND,0(R4),HT,X'40'
           SF  SYSPCHF                  INDICATE SYSPCH PRESENT
           ST  R2,PCHSTEML              SAVE PCH STEM NAME LENGTH
           MVC PCHSTEMN,0(R4)           SAVE PCH STEM NAME
           TR  PCHSTEMN,TRTBL           TRANSLATE TO UPPER-CASE
           LA  R3,PCHSTEMN-1            POINT TO LAST CHARACTER
          WHEN OTHER
           LHI R5,4                      SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDSL                         ENDIF
          AR   R3,R2                    ADJUST TO LAST CHARACTER
          IF   R2,(HT,C),=F'9',OR,      IF ARG LEN > 9                 X
               0(R3),NE,C'.'            OR LAST CHAR NOT A PERIOD
           LHI R5,4                      SET RESULT CODE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF

          LA   R6,L'ARGTABLE_END(,R6)  ADVANCE TO NEXT ARGUMENT
          LR   R7,R6
         WEND
*
         DROP  R6                      ARGUMENT TABLE NO LONGER NEEDED
         DROP  R7                      ARGUMENT STRING NO LONGER NEEDED
*
* READY TO PROCEED?
*
         IF    SYSIPTF,(NO,TF),$NOOP,OR,   IF MISSING IPT ARGUMENT     X
               SYSLSTF,(NO,TF),$NOOP       OR MISSING LST ARGUMENT
          LHI  R5,4                     SET RESULT CODE
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
* OUTPUT TO SYSLST -- ARXSAY
*
         LA    R0,SAYPARM1             GET ADDRESS OF PARM 1
         LA    R1,SAYPARM2             GET ADDRESS OF PARM 2
         LA    R2,SAYPARM3             GET ADDRESS OF PARM 3
         STM   R0,R2,SAYPARMB          PUT IN PARM BLOCK
         OI    SAYPLAST,X'80'          SET END OF PARM BLOCK
*
         MVC   SAYPARM1,=C'WRITE   '   SET PARM 1
         LA    R0,VARDATA              GET ADDRESS FOR PARM 2
         LA    R1,L'VARDATA            GET LENGTH FOR PARM 3
         STM   R0,R1,SAYPARM2          PUT IN PARMS 2 AND 3
*
* FOR PERFORMANCE REASONS, KEEP THIS ROUTINE IN MEMORY
*
         IF    ENTRY_P,(EQ,CLC),=F'0'  IF FIRST TIME CALL
          IF   DEBUG_F,(ON,TF),$NOOP    IF DEBUG IS ON
*
          MVC  VARDATA+0(26),=C'RXLIBRIO Load at: xxxxxxxx'
          ST   R11,AWORK4               STORE BASE ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+18(8),EWORK8     THEN PUT IN MSG AREA
          LHI  R3,26                    SET LENGTH OF OUTPUT
          BAS  R5,SAYIT                 GO SAY IT
*
          MVC  VARDATA+0(31),=C'RXLIBRIX Load at: xxxxxxxx xxxx'
          MVC  AWORK4,=V(RXLIBRIX)      STORE BASE ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+18(8),EWORK8     THEN PUT IN MSG AREA
          L    R3,=V(RXLIBRIX)          GET LOAD POINT OF EXIT RTN
          SR   R3,R11                   SUBTRACT BASE LOAD POINT
          ST   R3,AWORK4                STORE OFFSET ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK OFFSET ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+27(4),EWORK8+4   THEN ADD TO MSG AREA
          LHI  R3,31                    SET LENGTH OF OUTPUT
          BAS  R5,SAYIT                 GO SAY IT
*
          MVC  VARDATA+0(31),=C'RXLIBRLX Load at: xxxxxxxx xxxx'
          MVC  AWORK4,=V(RXLIBRLX)      STORE BASE ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+18(8),EWORK8     THEN PUT IN MSG AREA
          L    R3,=V(RXLIBRLX)          GET LOAD POINT OF EXIT RTN
          SR   R3,R11                   SUBTRACT BASE LOAD POINT
          ST   R3,AWORK4                STORE OFFSET ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK OFFSET ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+27(4),EWORK8+4   THEN ADD TO MSG AREA
          LHI  R3,31                    SET LENGTH OF OUTPUT
          BAS  R5,SAYIT                 GO SAY IT
*
          MVC  VARDATA+0(31),=C'RXLIBRPX Load at: xxxxxxxx xxxx'
          MVC  AWORK4,=V(RXLIBRPX)      STORE BASE ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+18(8),EWORK8     THEN PUT IN MSG AREA
          L    R3,=V(RXLIBRPX)          GET LOAD POINT OF EXIT RTN
          SR   R3,R11                   SUBTRACT BASE LOAD POINT
          ST   R3,AWORK4                STORE OFFSET ADDRESS
          UNPK EWORK8(9),AWORK4(5)      UNPACK OFFSET ADDRESS
          TR   EWORK8(8),TRTBL           AND TRANSLATE TO PRINTABLE
          MVC  VARDATA+27(4),EWORK8+4   THEN ADD TO MSG AREA
          LHI  R3,31                    SET LENGTH OF OUTPUT
          BAS  R5,SAYIT                 GO SAY IT
*
          ENDIF                         END OF DEBUG TEST
*
          CDLOAD RXLIBRIO               MAKE THIS RTN STAY IN MEMORY
          IF   R15,(NZ,LTR),R15         IF CDLOAD ERROR
           LHI R5,10                     INDICATE LOAD FAILURE
           B   RETERR                    GO RETURN ERROR
          ENDIF                         ENDIF
          ST   R1,ENTRY_P               SAVE RXLIBRIO ENTRY POINT       00033400
         ENDIF                         ENDIF                            00033600
*
* SETUP FOR LIBR UNDER-THE-COVERS
*
         IF    LIBR_EP,(EQ,CLC),=F'0'  LOAD LIBR ONE TIME, ONLY
          SAM24                         SWITCH TO 24-BIT MODE
          COMRG REG=(R1)                GET ADDRESS OF COMREG IN R1     00032400
          L    R2,36(R1)                GET NEXT FREE ADDRESS           00032500
          LA   R2,8(,R2)                ADD X'08' TO THAT ADDRESS       00032600
          SRL  R2,3                     AND TRUNCATE LOW 3 BITS         00032700
          SLL  R2,3                     TO POINT TO NEXT DOUBLE-WORD    00032800
          LOAD LIBR,(R2),SYS=YES,RET=YES                                00032900
          IF   R15,(NZ,LTR),R15         IF NOT LOADED OK                00033000
           LHI R5,10                     INDICATE LOAD ERROR            00033100
           B   RETERR                    GO RETURN ERROR                00033200
          ENDIF                         ENDIF                           00033300
          ST   R1,LIBR_EP               SAVE LIBR ENTRY POINT           00033400
*
          IF   DEBUG_F,(ON,TF),$NOOP    IF DEBUG IS ON
           SAM31                         SWITCH TO 31-BIT MODE FOR REXX
           MVC VARDATA+0(26),=C'LIBR     Load at: xxxxxxxx'
           MVC AWORK4,LIBR_EP            STORE BASE ADDRESS
           UNPK EWORK8(9),AWORK4(5)      UNPACK BASE ADDRESS
           TR  EWORK8(8),TRTBL            AND TRANSLATE TO PRINTABLE
           MVC VARDATA+18(8),EWORK8      THEN PUT IN MSG AREA
           LHI R3,26                     SET LENGTH OF OUTPUT
           BAS R5,SAYIT                  GO SAY IT
          ENDIF                         END OF DEBUG TEST
         ENDIF                         ENDIF                            00033600
*
         SAM24                         SWITCH TO 24-BIT MODE
         LA    R1,INLCPARB             GET ADDRESS OF PARM BLOCK        00033700
         ST    R1,INLCPARM             PUT ADDRESS IN PARM LIST         00033800
         OI    INLCPARM,X'80'          SET LAST ADDRESS IN LIST         00033900
*
         XC    INLCPARB(INLCPLEN),INLCPARB   CLEAR PARM BLOCK
         LA    R2,INLCPLEN             GET PARM BLOCK LENGTH            00026200
         STH   R2,INLCPARL             AND STORE IT IN PARM BLOCK       00026300
         XR    R2,R2                   SET SPF CALLER IDENTIFICATION    00026400
         ST    R2,INLCPCID             AND STORE IT IN PARM BLOCK       00026500
*        OI    INLCPFB1,X'20'   ???    SET "MSHP" BYPASS ON             00026600
*                                                                       00026700
         L     R2,=V(RXLIBRIX)         GET EXIT RTN ADDRESS             00026900
         LA    R3,IPTAREA              GET I/O AREA ADDRESS             00027000
         STM   R2,R3,INLCIPTB          PUT ADDRESSES IN PARM BLOCK      00027600
         L     R0,=V(RXLIBRLX)         GET EXIT RTN ADDRESS             00026900
         LA    R1,LSTAREA              GET I/O AREA ADDRESS             00027000
         L     R2,=V(RXLIBRPX)         GET EXIT RTN ADDRESS             00026900
         LA    R3,PCHAREA              GET I/O AREA ADDRESS             00027000
         STM   R0,R3,INLCLSTB          PUT ADDRESSES IN PARM BLOCK      00027600
*
         LHI   R1,IPTEND-IPTAREA       GET SYSIPT AREA LENGTH
         STH   R1,IPTLENG              PUT IN SYSIPT AREA
         LHI   R1,LSTEND-LSTAREA       GET SYSLST AREA LENGTH
         STH   R1,LSTLENG              PUT IN SYSLST AREA
         LHI   R1,PCHEND-PCHAREA       GET SYSPCH AREA LENGTH
         STH   R1,PCHLENG              PUT IN SYSPCH AREA
*
* SET STEM NAME PREFIX (FOR VARIABLE RETRIEVAL/CREATION)
*
         SAM31                         SWITCH TO 31-BIT MODE FOR REXX
         LA    R4,VARNAME              POINT TO VAR NAME BUFFER
         MVC   0(L'IPTSTEMN,R4),IPTSTEMN  MOVE IN STEM NAME
         A     R4,IPTSTEML                POINT TO AFTER STEM NAME
         CF    SET_KLZ                 REG2ZON3 DROP LEADING ZEROES
* GET SYSIPT.0 VALUE
         XR    R2,R2                   SET TO ZERO INDEX
         ST    R2,IPTSTEMX             SAVE CURRENT INDEX
         LA    R3,0(,R4)               POINT TO VARIABLE NAME AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         L     R3,IPTSTEML             COMPUTE LENGTH OF STEM NAME
         AR    R3,R2                    PLUS LENGTH OF COUNTER
         BAS   R2,GETVAR               RETRIEVE THE VARIABLE VALUE
         IF    R5,(NZ,LTR),R5,OR,      IF VARIABLE NOT FOUND           X
               SHVVALL,(NH,CLC),=F'0'  OR VALUE LENGTH NOT > ZERO
          LHI  R5,-1                    SET INVALID STEM.0
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
         LA    R2,VARDATA              POINT TO VAR VALUE
         L     R3,SHVVALL              GET VAR LENGTH
         BAS   R15,ZON2REG3            CONVERT THAT TO BINARY
         ST    R3,IPTSTEMC             SAVE IPT STEM COUNT
* GET SYSIPT.1 VALUE
         LHI   R2,1                    SET TO INDEX 1
         ST    R2,IPTSTEMX             SAVE CURRENT INDEX
         LA    R3,0(,R4)               POINT TO VARIABLE NAME AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         L     R3,IPTSTEML             COMPUTE LENGTH OF STEM NAME
         AR    R3,R2                    PLUS LENGTH OF COUNTER
         BAS   R2,GETVAR               RETRIEVE THE VARIABLE VALUE
         IF    R5,(NZ,LTR),R5,OR,      IF VARIABLE NOT FOUND           X
               SHVVALL,(NH,CLC),=F'0'  OR VALUE LENGTH NOT > ZERO
          LHI  R5,-2                    SET INVALID STEM.n
          IF   R15,(Z,LTR),R15          IF ZERO RETURN CODE
           L   R15,IPTSTEMX              USER STEM INDEX AS RESULT
          ENDIF                         ENDIF
          B    RETERR                   GO RETURN ERROR
IPTMOVE  MVC   IPTDATA(0),VARDATA      (SEE "EX" INSTRUCTION BELOW)
         ENDIF                         ENDIF
         MVI   IPTDATA,X'40'           CLEAR INPUT DATA
         MVC   IPTDATA+1(79),IPTDATA     TO ALL BLANKS
         L     R3,SHVVALL              GET VAR LENGTH
         SHI   R3,1                     MINUS 1 FOR EXECUTE
         EX    R3,IPTMOVE              MOVE TO LIBR PARM
*
         IF    DEBUG_F,(ON,TF),$NOOP   IF DEBUG IS ON
          MVC  VARDATA+0(7),=C'SYSIPT:' LET'S SEE THE CARD INPUT
          MVC  VARDATA+8(L'IPTDATA),IPTDATA
          LHI  R3,IPTEND-IPTAREA        SET LENGTH OF CARD INPUT
          BAS  R5,SAYIT                 GO SAY IT
         ENDIF                         END OF DEBUG TEST
*
* INVOKE LIBR UNDER-THE-COVERS
*
         SAM24                         SWITCH TO 24-BIT MODE
         LA    R1,INLCPARM             GET ADDRESS OF PARM LIST         00034000
         ST    R13,INLCUSER            PASS DYNASTOR ADDR TO EXITS
         L     R15,LIBR_EP             GET LIBR ENTRY POINT ADDR        00034000
         BASR  R14,R15                 INVOKE LIBR CALL INTERFACE
         L     R5,(11-1)*4(,R13)       GET "RC" VALUE IN REG 5 LOC
         IF    R15,(Z,LTR),R15         IF NO FUNCTION ERROR (YET)
          L    R15,(5-1)*4(,R13)        GET "RESULT"  IN REG 15 LOC
         ENDIF                         ENDIF
         IF    R5,(NZ,LTR),R5,OR,      IF FAILURE CODE                 X
               R15,(NZ,LTR),R15        OR FUNCTION ERROR
          IF   R5,(Z,LTR),R5            IF NO FAILURE CODE
           LHI R5,-5                     INDICATE R15 ERROR
          ENDIF                         ENDIF
          B    RETERR                   GO RETURN ERROR
         ENDIF                         ENDIF
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   SAM31                         MAKE SURE IT IS 31-BIT MODE
         L     R8,SAVEEVAL             GET EVALUATION BLOCK ADDRESS
         USING EVALBLOCK,R8            ESTABLISH ADDRESSABILITY
         LA    R1,EVALBLOCK_EVDATA     POINT TO "RESULT" AREA
         LA    R2,4                    CONVERTED LENGTH IS 4 BYTES
         ST    R2,EVALBLOCK_EVLEN            FOR HEX "RESULT" VALUE
         STH   R15,EWORK8              STORE 2 BYTES INTO WORK AREA
         UNPK  0(5,R1),EWORK8(3)       UNPACK CHAR INTO HEX && TRANSL.
         TR    0(4,R1),TRTBL             X'FAFBFCFDFEFF' TO C'ABCDEF'
         DROP  R8                      EVAL BLOCK NO LONGER NEEDED
*
         CF    SET_KLZ                 REG2ZON3 NO LEADING ZEROES
         LR    R2,R5                   COPY R5 TO R2
         LA    R3,VARDATA              POINT TO VARIABLE DATA AREA
         BAS   R15,REG2ZON3            CONVERT THAT TO ZONED-DECIMAL
         LA    R1,VARDATA              POINT BACK TO VAR DATA AREA
         STM   R1,R2,SHVVALA           PUT IN SHV CONTROL BLOCK
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
* ------------------------------------------------------------------- *
         CVD   R2,PWORK8               CONVERT REG2 TO PACKED-DECIMAL
         IF    SET_KLZ,(ON,TF),$NOOP   KEEP LEADING ZEROES?
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
         IF    SET_KLZ,(ON,TF),$NOOP   KEEP LEADING ZEROES?
          LA   R1,EWORK8+2             POINT TO SIGNIFICANCE BYTE
          LA   R2,4
         ELSE
          SR   R2,R1                   CALC NO. OF CHARS TO MOVE
         ENDIF
         BCTR  R2,0                    DECR. LEN FOR ZERO REF.
         EX    R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
* ------------------------------------------------------------------- *
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
ZON2REG3 DS    0H    CONVERT ZONED-DECIMAL AT REG2 (L'REG3) INTO REG3
* ------------------------------------------------------------------- *
         BCTR  R3,0                    DECR. FOR ZERO OFFSET
         EX    R3,PKINPT               PACK 0(?,R2) FOR LENGTH OF R3
         CVB   R3,PWORK8               CONVERT PACKED-DECIMAL INTO REG3
* ------------------------------------------------------------------- *
         BR    R15
PKINPT   PACK  PWORK8,0(0,R2)          (SEE "EX" INSTRUCTION ABOVE)
* =================================================================== *
         EJECT
* =================================================================== *
SAYIT    DS    0H    USE REXX "SAY" TO DISPLAY MESSAGE
* ------------------------------------------------------------------- *
         ST    R3,SAYPARM3             PUT BUFFER LENGTH IN SAY PARM 3
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
         LA    R1,SAYPARMB             GET ADDRESS OF SAY PARM BLOCK
         L     R15,ARXSAY              POINT TO PGM TO CALL
         BASR  R14,R15                 CALL ARXSAY
         IF    R15,(NZ,LTR),R15        IF NON-ZERO RETURN CODE
          LA   R5,8                      SET RESULT CODE
          B    RETERR                    GO RETURN ERROR
         ENDIF                         ENDIF
* ------------------------------------------------------------------- *
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
GETVAR   DS    0H    GET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         XR    R5,R5                   ZERO RETURN CODE REGISTER
         MVI   SHVCODE,SHVFETCH        SET REQUEST CODE TO 'F'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
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
* ------------------------------------------------------------------- *
         BR    R2
* =================================================================== *
         EJECT
* =================================================================== *
SETVAR   DS    0H    SET REXX VARIABLE VALUE
* ------------------------------------------------------------------- *
         MVI   SHVCODE,SHVSTORE        SET REQUEST CODE TO 'S'
         ST    R3,SHVNAML              PUT LENGTH OF VAR NAME IN SHVCB
*
         L     R0,SAVEENVB             GET ENVIRONMENT BLOCK ADDRESS
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
* ------------------------------------------------------------------- *
         BR    R5
* =================================================================== *
         EJECT
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SALEN    DC    A(DYNALENG)
ENTRY_P  DC    A(0)
LIBR_EP  DC    A(0)
*
         LTORG
*
* =================================================================== *
         END   RXLIBRIO
