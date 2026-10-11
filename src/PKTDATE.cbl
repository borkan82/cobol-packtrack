      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTDATE.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2019-12-03.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTDATE - POTPROGRAM (CALL 'PKTDATE' USING ...)
      * OPIS    : ZAJEDNIČKE OPERACIJE NAD DATUMIMA:
      *           CNV - SVAKI KURIR ŠALJE DATUM U SVOM FORMATU
      *                 (GGGGMMDD, GGGG-MM-DD, DD.MM.GGGG) I VRIJEME
      *                 SA ILI BEZ DVOTAČKE. POTPROGRAM PROVJERAVA
      *                 ISPRAVNOST I VRAĆA JEDINSTVEN GGGGMMDDHHMM.
      *           ADD - DODAJE N RADNIH DANA (BEZ SUBOTE I NEDJELJE)
      *                 - ZA OBEĆANI DATUM ISPORUKE.
      * PARAMETRI: VIDI COPYBOOK DATEPARM
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2020-40-13 BJ     PRVA VERZIJA
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATE                     PIC X(08).
       01  WS-DATE-R REDEFINES WS-DATE.
           05  WS-YYYY                 PIC X(04).
           05  WS-MM                   PIC X(02).
           05  WS-DD                   PIC X(02).
       01  WS-DATE-N REDEFINES WS-DATE PIC 9(08).
       01  WS-TIME.
           05  WS-HH                   PIC X(02).
           05  WS-MI                   PIC X(02).
       01  WS-TIME-N REDEFINES WS-TIME PIC 9(04).
       01  WS-INT                      PIC S9(09)  COMP.
       01  WS-ADDED                    PIC 9(03).
      *    DAN U SEDMICI: INTEGER-OF-DATE(1601-01-01)=1 JE PONEDJELJAK,
      *    PA JE MOD 7: 1=PON ... 5=PET, 6=SUB, 0=NED
       01  WS-DOW                      PIC 9(01).

       LINKAGE SECTION.
           COPY DATEPARM.

       PROCEDURE DIVISION USING DATE-PARM.
       0000-MAIN.
           MOVE 0 TO LP-RC
           EVALUATE LP-FUNC
               WHEN 'CNV'
                   PERFORM 1000-CONVERT
               WHEN 'ADD'
                   PERFORM 2000-ADD-WORKDAYS
               WHEN OTHER
                   MOVE 9 TO LP-RC
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
      * KONVERZIJA U GGGGMMDDHHMM SA PROVJEROM
      *----------------------------------------------------------------*
       1000-CONVERT.
           MOVE ZERO TO LP-TS-OUT
           EVALUATE LP-FORMAT
               WHEN 'NUM'
                   MOVE LP-DATE-IN (1:8) TO WS-DATE
               WHEN 'ISO'
                   MOVE LP-DATE-IN (1:4) TO WS-YYYY
                   MOVE LP-DATE-IN (6:2) TO WS-MM
                   MOVE LP-DATE-IN (9:2) TO WS-DD
               WHEN 'DMY'
                   MOVE LP-DATE-IN (7:4) TO WS-YYYY
                   MOVE LP-DATE-IN (4:2) TO WS-MM
                   MOVE LP-DATE-IN (1:2) TO WS-DD
               WHEN OTHER
                   MOVE 9 TO LP-RC
           END-EVALUATE
           IF LP-RC = 0
               IF WS-DATE IS NOT NUMERIC
                   MOVE 1 TO LP-RC
               ELSE
                   IF FUNCTION TEST-DATE-YYYYMMDD (WS-DATE-N)
                      NOT = ZERO
                       MOVE 1 TO LP-RC
                   END-IF
               END-IF
           END-IF
           IF LP-RC = 0
               PERFORM 1100-CONVERT-TIME
           END-IF
           IF LP-RC = 0
               COMPUTE LP-TS-OUT = WS-DATE-N * 10000 + WS-TIME-N
           END-IF.

      *    VRIJEME "HH:MM" ILI "HHMM"; PRAZNO = PONOĆ
       1100-CONVERT-TIME.
           EVALUATE TRUE
               WHEN LP-TIME-IN = SPACES
                   MOVE '0000' TO WS-TIME
               WHEN LP-TIME-IN (3:1) = ':'
                   MOVE LP-TIME-IN (1:2) TO WS-HH
                   MOVE LP-TIME-IN (4:2) TO WS-MI
               WHEN OTHER
                   MOVE LP-TIME-IN (1:4) TO WS-TIME
           END-EVALUATE
           IF WS-TIME IS NOT NUMERIC
               MOVE 2 TO LP-RC
           ELSE
               IF WS-HH > '23' OR WS-MI > '59'
                   MOVE 2 TO LP-RC
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * DODAVANJE RADNIH DANA
      *----------------------------------------------------------------*
       2000-ADD-WORKDAYS.
           MOVE LP-DATE-IN (1:8) TO WS-DATE
           IF WS-DATE IS NOT NUMERIC
               MOVE 1 TO LP-RC
           ELSE
               IF FUNCTION TEST-DATE-YYYYMMDD (WS-DATE-N) NOT = ZERO
                   MOVE 1 TO LP-RC
               END-IF
           END-IF
           IF LP-RC = 0
               COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE (WS-DATE-N)
               MOVE ZERO TO WS-ADDED
               PERFORM UNTIL WS-ADDED >= LP-DAYS
                   ADD 1 TO WS-INT
                   COMPUTE WS-DOW = FUNCTION MOD (WS-INT, 7)
                   IF WS-DOW NOT = 0 AND WS-DOW NOT = 6
                       ADD 1 TO WS-ADDED
                   END-IF
               END-PERFORM
               COMPUTE LP-DATE-OUT = FUNCTION DATE-OF-INTEGER (WS-INT)
           END-IF.
