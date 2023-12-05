LIBR     TITLE 'RXLIBRIX -- LIBR SYSIPT Exit Routine'
         PRINT ON,GEN
RXLIBRIX AMODE 24
RXLIBRIX RMODE 24
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   S Y S I P T   E X I T   R O U T I N E   f o r   R X L I B R I O   *
*                                                                     *
*                                                                     *
* No warranty is expressed or implied.       Written by Dave L Clark  *
* Neither the author nor the company is      Phone: (937)751-3300     *
* responsible for any loss or damage         Winsupply Group Services *
* resulting from the use of this code.       3110 Kettering Blvd.     *
* If provided, source on as-is basis.        Dayton, OH  45439        *
* =================================================================== *
*                                                                     *
*                               PURPOSE                               *
*                                                                     *
*      This subroutine is intended for use with the RXLIBRIO external *
* assembler REXX function and is  used  as a LIBR SYSIPT exit routine *
* by that program.   That program provides all of the dynamic storage *
* and REXX  control  blocks  that  this  subroutine  uses.   See that *
* program for more information.                                       *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* =================================================================== *
DYNASTOR DSECT
* ------------------------------------------------------------------- *
         COPY  RXLIBRIO
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
R11      EQU   11                      RXLIBRIX CSECT BASE REG 1
R12      EQU   12                      RXLIBRIX CSECT BASE REG 2
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
RXLIBRIX HEADER VERSION='1.0',AUTHOR='DLC',DATE='(C)NOV23',            X
               BASE=R11,SVAREA=SAVEAREA
* ------------------------------------------------------------------- *
         LR    R8,R1                   GET ADDRESS OF PARM BLOCK
         USING INLCPARB,R8             MAP PASSED PARM AREA             00034000
         L     R15,INLCUSER            GET ORIGINAL DYNASTOR ADDRESS
         DROP  R8                      RELEASE MAPPING
         LM    R9,R10,(15-1)*4(R15)    GET ORIGINAL R9, R10 VALUES
         USING ARXEXTE,R9              REXX EXTERNAL ENTRY-POINT VECTOR
         USING SHVBLOCK,R10            SHARED VARIABLES CONTROL BLOCK
*
         AHI   R15,18*4                POINT TO EXIT SAVEAREA
         MVC   0(18*4,R15),0(R13)      MOVE SAVEAREA TO DYNA STORAGE
         ST    R15,(3-1)*4(,R13)       SET NEXT SAVEAREA IN OLD AREA
         LR    R13,R15                 LOAD REG WITH NEW AREA ADDR
         USING DYNASTOR,R13            ESTABLISH DATA BASE REG
         XR    R15,R15                 CLEAR RETURN CODE REGISTER
*
         SAM31                         SWITCH TO 31-BIT FOR REXX
         IF    DEBUG_F,(ON,TF),$NOOP   IF DEBUG IS ON
          MVI  VARDATA+0,C'-'           BUILD A
          MVC  VARDATA+1(79),VARDATA     LINE OF DASHES
          LHI  R3,80                      80 COLUMNS LONG
          JAS  R5,SAYIT                 GO SAY IT
          LA   R4,INLCPARB              DIAGNOSTIC
          LA   R5,INLCLRTC+4              DUMP OF
          PDUMP (4),(5)                     LIBR PARAMETER BLOCK
         ENDIF                         END OF DEBUG TEST
*
         MVI   IPTDATA,X'40'           CLEAR INPUT DATA
         MVC   IPTDATA+1(79),IPTDATA     TO ALL BLANKS
         L     R2,IPTSTEMX             GET CURRENT INDEX
         AHI   R2,1                    INCREMENT INDEX
         ST    R2,IPTSTEMX             SAVE CURRENT INDEX
         IF    R2,(HT,C),IPTSTEMC      IF ALL INPUT SENT
          MVC  IPTDATA(2),=C'/*'        INDICATE END-OF-DATA
         ELSE                          ELSE
          LA   R4,VARNAME               POINT TO VAR NAME BUFFER
          MVC  0(L'IPTSTEMN,R4),IPTSTEMN  MOVE IN STEM NAME
          A    R4,IPTSTEML                POINT TO AFTER STEM NAME
          LA   R3,0(,R4)                POINT TO VARIABLE NAME AREA
          JAS  R15,REG2ZON3             CONVERT THAT TO ZONED-DECIMAL
          L    R3,IPTSTEML              COMPUTE LENGTH OF STEM NAME
          AR   R3,R2                     PLUS LENGTH OF COUNTER
          JAS  R2,GETVAR                RETRIEVE THE VARIABLE VALUE
          IF   R5,(NZ,LTR),R5,OR,       IF VARIABLE NOT FOUND          X
               SHVVALL,(NH,CLC),=F'0'   OR VALUE LENGTH NOT > ZERO
           LHI R5,-2                     SET INVALID STEM.n
           IF  R15,(Z,LTR),R15           IF ZERO RETURN CODE
            L  R15,IPTSTEMX               USER STEM INDEX AS RESULT
           ENDIF                         ENDIF
           J   RETERR                    GO RETURN ERROR
IPTMOVE  MVC   IPTDATA(0),VARDATA        (SEE "EX" INSTRUCTION BELOW)
          ENDIF                         ENDIF
          L    R3,SHVVALL               GET VAR LENGTH
          SHI  R3,1                      MINUS 1 FOR EXECUTE
          EXRL R3,IPTMOVE               MOVE TO LIBR PARM
         ENDIF                         ENDIF
*
         IF    DEBUG_F,(ON,TF),$NOOP   IF DEBUG IS ON
          MVC  VARDATA+0(7),=C'SYSIPT:' LET'S SEE THE CARD INPUT
          MVC  VARDATA+8(L'IPTDATA),IPTDATA
          LHI  R3,IPTEND-IPTAREA        SET LENGTH OF CARD INPUT
          JAS  R5,SAYIT                 GO SAY IT
         ENDIF                         END OF DEBUG TEST
*
* RETURN RESULT
*
RETURN   XR    R5,R5                   CLEAR REG5 FOR ZERO "RC"
         XR    R15,R15                 CLEAR REG15 FOR ZERO "RESULT"
* ------------------------------------------------------------------- *
RETERR   SAM24                         SWITCH TO 24-BIT FOR LIBR
         SHI   R13,18*4                POINT TO MAIN SAVEAREA
         ST    R5,(11-1)*4(,R13)       SAVE "RC" VALUE IN REG 5 LOC
         ST    R15,(5-1)*4(,R13)       SAVE "RESULT"   IN REG 15 LOC
         AHI   R13,18*4                POINT TO EXIT SAVEAREA
         L     R13,(2-1)*4(,R13)       GET ADDRESS OF CALLER'S SAVEAREA
         ST    R15,(5-1)*4(,R13)       SAVE RETURN CODE IN REG 15 LOC
         LM    R14,R12,(4-1)*4(R13)    RESTORE CALLER'S REGISTERS
         BR    R14                     RETURN TO CALLER
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
         EXRL  R2,MVRSLT               MOVE VALUE TO RESULT AREA
         LA    R2,1(,R2)               INCR. LEN FOR NORMAL REF.
* ------------------------------------------------------------------- *
         BR    R15
MVRSLT   MVC   0(0,R3),0(R1)           (SEE "EX" INSTRUCTION ABOVE)
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
          J    RETERR                    GO RETURN ERROR
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
         CORG  0,256
* =================================================================== *
         EJECT
DATA     LOCTR
* =================================================================== *
STORAGE  DS    0D
* ------------------------------------------------------------------- *
SAVEAREA DS    9D                      STANDARD REG. SAVEAREA
*
         LTORG
*
         CORG  0,256
* =================================================================== *
         END   RXLIBRIX
