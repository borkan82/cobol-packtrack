      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTRPT.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2019-12-08.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTRPT - KORAK 040 DNEVNE OBRADE
      * OPIS    : DNEVNI IZVJEŠTAJ ZA OPERATIVU I KLIJENTE:
      *           1. SLA PO KURIRU - ISPORUČENO NA VRIJEME / KASNO,
      *              VRAĆENO, U TOKU, PROCENAT TAČNOSTI I PROSJEČAN
      *              BROJ DANA OD PREDAJE DO ISPORUKE.
      *              MATIČNA SE ČITA PO ALTERNATIVNOM KLJUČU (KURIR),
      *              PA KONTROLNI PREKID NE TRAŽI SORTIRANJE.
      *           2. STANJE POŠILJAKA PO UNIVERZALNOM STATUSU.
      *           3. IZUZECI - POŠILJKE KOJIMA TREBA INTERVENCIJA:
      *              OŠTEĆENE, PREKORAČEN BROJ POKUŠAJA DOSTAVE,
      *              BEZ SKENIRANJA N DANA, PROBIJEN OBEĆANI ROK.
      * ULAZ    : SHIPMAST - MATIČNE POŠILJKE
      *           PKT_RUN_DATE (ENV, OPCIONO)
      * IZLAZ   : SLARPT   - IZVJEŠTAJ, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2019-12-09 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY SHPSEL.
           SELECT SLA-RPT      ASSIGN TO 'SLARPT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.

       DATA DIVISION.
       FILE SECTION.
       FD  SHIP-MASTER.
           COPY SHPREC.
       FD  SLA-RPT.
       01  RPT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-SHIP              PIC X(02).
               88  FS-SHIP-OK          VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.

           COPY PKTCONST.
           COPY STATPARM.
           COPY RUNDATE.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  SHIP-EOF            VALUE 'Y'.

      *    BROJAČI JEDNOG KURIRA (C) I UKUPNO (G):
      *    1=UKUPNO 2=ISPORUČENO 3=NA VRIJEME 4=KASNO 5=VRAĆENO 6=U TOKU
       01  WS-CAR-COUNTS.
           05  WS-CC                   PIC 9(05)   OCCURS 6 TIMES.
       01  WS-GRAND-COUNTS.
           05  WS-GC                   PIC 9(05)   OCCURS 6 TIMES.
       01  WS-CAR-DAYS-SUM             PIC 9(07).
       01  WS-GRAND-DAYS-SUM           PIC 9(07).
       01  WS-KX                       PIC 9(01).

      *    STANJE PO STATUSU - REDOSLIJED KAO U PKTSTAT
       01  WS-STATUS-LIST              PIC X(18)
                                       VALUE 'RGPUTRHBODDLFARTEX'.
       01  WS-STATUS-COUNTS.
           05  WS-SC                   PIC 9(05)   OCCURS 9 TIMES.
       01  WS-SX                       PIC 9(02).

      *    IZUZECI SE SKUPLJAJU TOKOM ČITANJA I ŠTAMPAJU NA KRAJU
       01  WS-EXC-TABLE.
           05  WS-EXC-COUNT            PIC 9(03)   VALUE ZERO.
           05  WS-EXC-ENTRY            OCCURS 100 TIMES.
               10  EX-TRACK-NO         PIC X(14).
               10  EX-CARRIER          PIC X(03).
               10  EX-ORDER-NO         PIC X(10).
               10  EX-RCPT-NAME        PIC X(22).
               10  EX-STATUS           PIC X(02).
               10  EX-STATUS-TS        PIC 9(12).
               10  EX-PROMISED         PIC 9(08).
               10  EX-NOTE             PIC X(45).
       01  WS-EX                       PIC 9(03).

       01  WS-WORK-FIELDS.
           05  WS-BRK-CARRIER          PIC X(03).
           05  WS-CAR-NAME             PIC X(15).
           05  WS-DATE                 PIC 9(08).
           05  WS-SHIP-INT             PIC S9(09)  COMP.
           05  WS-DATE-INT             PIC S9(09)  COMP.
           05  WS-DAYS                 PIC S9(05).
           05  WS-DAYS-ED              PIC ZZZ9.
           05  WS-NOTE                 PIC X(45).
           05  WS-PTR                  PIC 9(03).
           05  WS-PCT                  PIC 9(03)V9.
           05  WS-AVG                  PIC 9(03)V9.

      *----------------------------------------------------------------*
      * LINIJE IZVJEŠTAJA
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  HDR1-COMPANY            PIC X(40).
           05  HDR1-TITLE              PIC X(76).
           05  FILLER                  PIC X(07)   VALUE 'STRANA '.
           05  HDR1-PAGE               PIC ZZZ9.
           05  FILLER                  PIC X(04)   VALUE SPACES.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: PKTRPT'.
           05  FILLER                  PIC X(14)   VALUE
                   'STANJE NA DAN '.
           05  HDR2-DATE               PIC X(10).
           05  FILLER                  PIC X(67)   VALUE SPACES.
       01  WS-SECTION-HDR.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  SH-TEXT                 PIC X(60).
           05  FILLER                  PIC X(71)   VALUE SPACES.
       01  WS-DASH.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(130)  VALUE ALL '-'.
           05  FILLER                  PIC X(01)   VALUE SPACE.

       01  WS-SLA-HDR.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(19)  VALUE 'KURIRSKA SLUZBA'.
           05  FILLER  PIC X(11)  VALUE '     UKUPNO'.
           05  FILLER  PIC X(11)  VALUE ' ISPORUCENO'.
           05  FILLER  PIC X(11)  VALUE ' NA VRIJEME'.
           05  FILLER  PIC X(11)  VALUE '      KASNO'.
           05  FILLER  PIC X(11)  VALUE '    VRACENO'.
           05  FILLER  PIC X(11)  VALUE '     U TOKU'.
           05  FILLER  PIC X(11)  VALUE ' % NA VRIJ.'.
           05  FILLER  PIC X(11)  VALUE ' PROSJ.DANA'.
           05  FILLER  PIC X(24)  VALUE SPACES.
       01  WS-SLA-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  SL-CARRIER              PIC X(03).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  SL-NAME                 PIC X(15).
           05  SL-COUNT-GRP            OCCURS 6 TIMES.
               10  FILLER              PIC X(01).
               10  SL-COUNT            PIC ZZZZZZZZZ9.
           05  FILLER                  PIC X(03)   VALUE SPACES.
           05  SL-PCT                  PIC ZZZZ9.9.
           05  FILLER                  PIC X(01)   VALUE '%'.
           05  FILLER                  PIC X(04)   VALUE SPACES.
           05  SL-AVG                  PIC ZZZZ9.9.
           05  FILLER                  PIC X(24)   VALUE SPACES.

       01  WS-STAT-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  ST-CODE                 PIC X(02).
           05  FILLER                  PIC X(02)   VALUE SPACES.
           05  ST-TEXT                 PIC X(25).
           05  ST-COUNT                PIC ZZZZ9.
           05  FILLER                  PIC X(97)   VALUE SPACES.

       01  WS-EXC-HDR.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(14)  VALUE 'POSILJKA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(03)  VALUE 'KUR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'NARUDZBA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(22)  VALUE 'PRIMALAC'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(02)  VALUE 'ST'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(16)  VALUE 'ZADNJI DOGADJAJ'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(10)  VALUE 'ROK'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(45)  VALUE 'RAZLOG'.
           05  FILLER  PIC X(02)  VALUE SPACES.
       01  WS-EXC-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-TRACK-NO             PIC X(14).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-CARRIER              PIC X(03).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-ORDER-NO             PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-RCPT-NAME            PIC X(22).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-STATUS               PIC X(02).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-STATUS-TS            PIC X(16).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-PROMISED             PIC X(10).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  EL-NOTE                 PIC X(45).
           05  FILLER                  PIC X(02)   VALUE SPACES.

       01  WS-PAGE-NO                  PIC 9(04)   VALUE ZERO.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-CARRIER-GROUP
               UNTIL SHIP-EOF
           PERFORM 2900-PRINT-SLA-TOTAL
           PERFORM 5000-PRINT-STATUS-SUMMARY
           PERFORM 6000-PRINT-EXCEPTIONS
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * POZICIONIRANJE NA POČETAK ALTERNATIVNOG KLJUČA (KURIR)
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'PKTRPT   - START DNEVNOG IZVJESTAJA'
           PERFORM 8900-GET-RUN-DATE
           INITIALIZE WS-GRAND-COUNTS WS-STATUS-COUNTS
           MOVE ZERO TO WS-GRAND-DAYS-SUM
           OPEN INPUT  SHIP-MASTER
                OUTPUT SLA-RPT
           IF NOT FS-SHIP-OK OR NOT FS-RPT-OK
               DISPLAY 'PKTRPT   - GRESKA OTVARANJA DATOTEKA: '
                       WS-FS-SHIP ' ' WS-FS-RPT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           MOVE 'SLA PO KURIRSKIM SLUZBAMA' TO HDR1-TITLE
           PERFORM 7000-PRINT-PAGE-HEADER
           MOVE 'ISPORUKA U ODNOSU NA OBECANI ROK (RADNI DANI)'
             TO SH-TEXT
           WRITE RPT-LINE FROM WS-SECTION-HDR AFTER ADVANCING 2 LINES
           WRITE RPT-LINE FROM WS-DASH        AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-SLA-HDR     AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-DASH        AFTER ADVANCING 1 LINE
           MOVE LOW-VALUES TO SM-CARRIER
           START SHIP-MASTER KEY IS >= SM-CARRIER
               INVALID KEY
                   SET SHIP-EOF TO TRUE
           END-START
           IF NOT SHIP-EOF
               PERFORM 8100-READ-NEXT
           END-IF.

      *----------------------------------------------------------------*
      * KONTROLNI PREKID PO KURIRU (REDOSLIJED DAJE ALTERNATIVNI KLJUČ)
      *----------------------------------------------------------------*
       2000-CARRIER-GROUP.
           MOVE SM-CARRIER TO WS-BRK-CARRIER
           INITIALIZE WS-CAR-COUNTS
           MOVE ZERO TO WS-CAR-DAYS-SUM
           PERFORM UNTIL SHIP-EOF
                      OR SM-CARRIER NOT = WS-BRK-CARRIER
               PERFORM 3000-EVALUATE-SHIPMENT
               PERFORM 8100-READ-NEXT
           END-PERFORM
           PERFORM 2100-PRINT-CARRIER-LINE.

       2100-PRINT-CARRIER-LINE.
           MOVE SPACES TO WS-CAR-NAME
           SEARCH ALL PC-CARRIER
               WHEN PC-CAR-CODE (CX) = WS-BRK-CARRIER
                   MOVE PC-CAR-NAME (CX) TO WS-CAR-NAME
           END-SEARCH
           MOVE WS-BRK-CARRIER TO SL-CARRIER
           MOVE WS-CAR-NAME    TO SL-NAME
           PERFORM VARYING WS-KX FROM 1 BY 1 UNTIL WS-KX > 6
               MOVE WS-CC (WS-KX) TO SL-COUNT (WS-KX)
               ADD  WS-CC (WS-KX) TO WS-GC (WS-KX)
           END-PERFORM
           ADD WS-CAR-DAYS-SUM TO WS-GRAND-DAYS-SUM
           PERFORM 2200-CALC-RATIOS
           WRITE RPT-LINE FROM WS-SLA-LINE AFTER ADVANCING 1 LINE.

      *    % NA VRIJEME I PROSJEČNO TRAJANJE - SAMO ZA ISPORUČENE
       2200-CALC-RATIOS.
           IF WS-CC (2) > ZERO
               COMPUTE WS-PCT ROUNDED = WS-CC (3) * 100 / WS-CC (2)
               COMPUTE WS-AVG ROUNDED = WS-CAR-DAYS-SUM / WS-CC (2)
           ELSE
               MOVE ZERO TO WS-PCT WS-AVG
           END-IF
           MOVE WS-PCT TO SL-PCT
           MOVE WS-AVG TO SL-AVG.

       2900-PRINT-SLA-TOTAL.
           WRITE RPT-LINE FROM WS-DASH AFTER ADVANCING 1 LINE
           MOVE SPACES   TO SL-CARRIER
           MOVE 'UKUPNO' TO SL-NAME
           MOVE WS-GRAND-COUNTS   TO WS-CAR-COUNTS
           MOVE WS-GRAND-DAYS-SUM TO WS-CAR-DAYS-SUM
           PERFORM VARYING WS-KX FROM 1 BY 1 UNTIL WS-KX > 6
               MOVE WS-GC (WS-KX) TO SL-COUNT (WS-KX)
           END-PERFORM
           PERFORM 2200-CALC-RATIOS
           WRITE RPT-LINE FROM WS-SLA-LINE AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-DASH AFTER ADVANCING 1 LINE.

      *----------------------------------------------------------------*
      * JEDNA POŠILJKA: SLA BROJAČI, STATUS, PROVJERA IZUZETAKA
      *----------------------------------------------------------------*
       3000-EVALUATE-SHIPMENT.
           ADD 1 TO WS-CC (1)
           PERFORM 3100-COUNT-STATUS
           COMPUTE WS-SHIP-INT =
                   FUNCTION INTEGER-OF-DATE (SM-SHIP-DATE)
           EVALUATE TRUE
               WHEN SM-DELIVERED
                   ADD 1 TO WS-CC (2)
                   COMPUTE WS-DATE = SM-DELIVERED-TS / 10000
                   IF WS-DATE NOT > SM-PROMISED-DATE
                       ADD 1 TO WS-CC (3)
                   ELSE
                       ADD 1 TO WS-CC (4)
                   END-IF
                   COMPUTE WS-DAYS =
                           FUNCTION INTEGER-OF-DATE (WS-DATE)
                         - WS-SHIP-INT
                   ADD WS-DAYS TO WS-CAR-DAYS-SUM
               WHEN SM-RETURNED
                   ADD 1 TO WS-CC (5)
               WHEN OTHER
                   ADD 1 TO WS-CC (6)
                   PERFORM 4000-CHECK-EXCEPTIONS
           END-EVALUATE.

       3100-COUNT-STATUS.
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 9
               IF WS-STATUS-LIST (WS-SX * 2 - 1:2) = SM-STATUS
                   ADD 1 TO WS-SC (WS-SX)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
      * IZUZECI - SVI RAZLOZI SE NIŽU U JEDNU NAPOMENU
      *----------------------------------------------------------------*
       4000-CHECK-EXCEPTIONS.
           MOVE SPACES TO WS-NOTE
           MOVE 1 TO WS-PTR
           IF SM-EXCEPTION
               STRING 'OSTECENA/IZUZETAK; '
                      DELIMITED BY SIZE INTO WS-NOTE
                      WITH POINTER WS-PTR
               END-STRING
           END-IF
           IF SM-ATTEMPTS >= PC-MAX-ATTEMPTS
               STRING SM-ATTEMPTS ' POKUSAJA - VRATITI; '
                      DELIMITED BY SIZE INTO WS-NOTE
                      WITH POINTER WS-PTR
               END-STRING
           END-IF
      *    DANI OD ZADNJEG SKENIRANJA
           COMPUTE WS-DATE = SM-STATUS-TS / 10000
           COMPUTE WS-DAYS = WS-RUN-INT
                   - FUNCTION INTEGER-OF-DATE (WS-DATE)
           IF WS-DAYS >= PC-STUCK-DAYS
               MOVE WS-DAYS TO WS-DAYS-ED
               STRING 'BEZ SKENIRANJA '
                      FUNCTION TRIM (WS-DAYS-ED) ' D; '
                      DELIMITED BY SIZE INTO WS-NOTE
                      WITH POINTER WS-PTR
               END-STRING
           END-IF
      *    PROBIJEN OBEĆANI ROK
           COMPUTE WS-DAYS = WS-RUN-INT
                   - FUNCTION INTEGER-OF-DATE (SM-PROMISED-DATE)
           IF WS-DAYS > ZERO
               MOVE WS-DAYS TO WS-DAYS-ED
               STRING 'KASNI '
                      FUNCTION TRIM (WS-DAYS-ED) ' D; '
                      DELIMITED BY SIZE INTO WS-NOTE
                      WITH POINTER WS-PTR
               END-STRING
           END-IF
           IF WS-NOTE NOT = SPACES AND WS-EXC-COUNT < 100
               ADD 1 TO WS-EXC-COUNT
               MOVE SM-TRACK-NO      TO EX-TRACK-NO  (WS-EXC-COUNT)
               MOVE SM-CARRIER       TO EX-CARRIER   (WS-EXC-COUNT)
               MOVE SM-ORDER-NO      TO EX-ORDER-NO  (WS-EXC-COUNT)
               MOVE SM-RCPT-NAME     TO EX-RCPT-NAME (WS-EXC-COUNT)
               MOVE SM-STATUS        TO EX-STATUS    (WS-EXC-COUNT)
               MOVE SM-STATUS-TS     TO EX-STATUS-TS (WS-EXC-COUNT)
               MOVE SM-PROMISED-DATE TO EX-PROMISED  (WS-EXC-COUNT)
               MOVE WS-NOTE          TO EX-NOTE      (WS-EXC-COUNT)
           END-IF.

      *----------------------------------------------------------------*
      * STANJE PO STATUSU - TEKST IZ POTPROGRAMA PKTSTAT
      *----------------------------------------------------------------*
       5000-PRINT-STATUS-SUMMARY.
           MOVE 'POSILJKE PO TRENUTNOM STATUSU' TO SH-TEXT
           WRITE RPT-LINE FROM WS-SECTION-HDR AFTER ADVANCING 3 LINES
           WRITE RPT-LINE FROM WS-DASH        AFTER ADVANCING 1 LINE
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 9
               MOVE 'T' TO SP-FUNC
               MOVE WS-STATUS-LIST (WS-SX * 2 - 1:2) TO SP-STATUS
               CALL 'PKTSTAT' USING STAT-PARM
               MOVE SP-STATUS      TO ST-CODE
               MOVE SP-TEXT        TO ST-TEXT
               MOVE WS-SC (WS-SX)  TO ST-COUNT
               WRITE RPT-LINE FROM WS-STAT-LINE AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-LINE FROM WS-DASH AFTER ADVANCING 1 LINE.

      *----------------------------------------------------------------*
      * IZUZECI - NOVA STRANA
      *----------------------------------------------------------------*
       6000-PRINT-EXCEPTIONS.
           MOVE 'IZUZECI - POSILJKE ZA INTERVENCIJU' TO HDR1-TITLE
           PERFORM 7000-PRINT-PAGE-HEADER
           WRITE RPT-LINE FROM WS-DASH    AFTER ADVANCING 2 LINES
           WRITE RPT-LINE FROM WS-EXC-HDR AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-DASH    AFTER ADVANCING 1 LINE
           PERFORM VARYING WS-EX FROM 1 BY 1
                   UNTIL WS-EX > WS-EXC-COUNT
               MOVE EX-TRACK-NO  (WS-EX) TO EL-TRACK-NO
               MOVE EX-CARRIER   (WS-EX) TO EL-CARRIER
               MOVE EX-ORDER-NO  (WS-EX) TO EL-ORDER-NO
               MOVE EX-RCPT-NAME (WS-EX) TO EL-RCPT-NAME
               MOVE EX-STATUS    (WS-EX) TO EL-STATUS
               MOVE EX-STATUS-TS (WS-EX) TO WS-TS-IN
               PERFORM 8920-FORMAT-TS
               MOVE WS-TS-OUT            TO EL-STATUS-TS
               COMPUTE WS-TS-IN = EX-PROMISED (WS-EX) * 10000
               PERFORM 8920-FORMAT-TS
               MOVE WS-DATE-TXT          TO EL-PROMISED
               MOVE EX-NOTE      (WS-EX) TO EL-NOTE
               WRITE RPT-LINE FROM WS-EXC-LINE AFTER ADVANCING 1 LINE
           END-PERFORM
           WRITE RPT-LINE FROM WS-DASH AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-LINE
           MOVE WS-EXC-COUNT TO WS-DAYS-ED
           STRING ' POSILJAKA ZA INTERVENCIJU: '
                  FUNCTION TRIM (WS-DAYS-ED)
                  DELIMITED BY SIZE INTO RPT-LINE
           END-STRING
           WRITE RPT-LINE AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES.

       7000-PRINT-PAGE-HEADER.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO      TO HDR1-PAGE
           MOVE PC-COMPANY-1    TO HDR1-COMPANY
           MOVE WS-RUN-DATE-TXT TO HDR2-DATE
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 1 LINE.

       8100-READ-NEXT.
           READ SHIP-MASTER NEXT RECORD
               AT END SET SHIP-EOF TO TRUE
           END-READ.

           COPY GETDATE.

       9000-TERMINATE.
           CLOSE SHIP-MASTER SLA-RPT
           MOVE WS-GC (1) TO WS-DAYS-ED
           DISPLAY 'PKTRPT   - POSILJAKA UKUPNO      : ' WS-DAYS-ED
           MOVE WS-EXC-COUNT TO WS-DAYS-ED
           DISPLAY 'PKTRPT   - ZA INTERVENCIJU       : ' WS-DAYS-ED
           MOVE ZERO TO RETURN-CODE
           DISPLAY 'PKTRPT   - KRAJ, RC= 0'.
