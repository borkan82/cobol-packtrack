      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTIMP.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2019-12-18.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTIMP - KORAK 020 DNEVNE OBRADE (JEDNOM PO KURIRU)
      * OPIS    : UVOZ DOGAĐAJA PRAĆENJA OD KURIRSKE SLUŽBE.
      *           SVAKI KURIR ŠALJE DATOTEKU U SVOM FORMATU:
      *             BRZ - FIKSNA DUŽINA, ZAGLAVLJE/STAVKE/ZAVRŠNI SLOG
      *                   SA KONTROLNIM BROJEM STAVKI
      *             EUP - CSV SA ";" I ZAGLAVLJEM, DATUM GGGG-MM-DD
      *             CIT - POLJA ODVOJENA SA "|", DATUM DD.MM.GGGG
      *           SVAKI RED SE PREVODI U ZAJEDNIČKI FORMAT (EVTREC):
      *           DATUM PREKO CALL 'PKTDATE', STATUS PREKO
      *           CALL 'PKTSTAT'. NEISPRAVNI REDOVI SE ODBACUJU.
      *           KOD BRZ, AKO SE KONTROLNI ZBIR NE SLAŽE, ODBACUJE SE
      *           CIJELA SERIJA (IZLAZ OSTAJE PRAZAN, RC=8).
      * ULAZ    : CARRIN   - DATOTEKA KURIRA
      *           PKT_CARRIER (ENV) - KOD KURIRA: BRZ, EUP ILI CIT
      * IZLAZ   : EVTOUT   - NORMALIZOVANI DOGAĐAJI (EVTREC)
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - SVI REDOVI UVEZENI
      *            4 - NEKI REDOVI ODBAČENI
      *            8 - CIJELA SERIJA ODBAČENA (KONTROLNI ZBIR)
      *           12 - FATALNA GREŠKA (DATOTEKE, NEPOZNAT KURIR)
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2019-12-28 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARR-IN      ASSIGN TO 'CARRIN'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-IN.
           SELECT EVT-OUT      ASSIGN TO 'EVTOUT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-OUT.

       DATA DIVISION.
       FILE SECTION.
       FD  CARR-IN.
       01  CARR-REC                    PIC X(200).
       FD  EVT-OUT.
           COPY EVTREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-IN                PIC X(02).
               88  FS-IN-OK            VALUE '00'.
           05  WS-FS-OUT               PIC X(02).
               88  FS-OUT-OK           VALUE '00'.

           COPY PKTCONST.
           COPY DATEPARM.
           COPY STATPARM.
      *    SLOG KURIRA BRZ - RASPORED SA REDEFINES PO TIPU SLOGA
           COPY BRZREC.

       01  WS-CARRIER                  PIC X(03)   VALUE SPACES.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  IN-EOF              VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01)   VALUE 'N'.
               88  HEADER-SEEN         VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01)   VALUE 'N'.
               88  TRAILER-SEEN        VALUE 'Y'.
           05  WS-BATCH-SW             PIC X(01)   VALUE 'N'.
               88  BATCH-REJECTED      VALUE 'Y'.

      *    POLJA ZA UNSTRING (CSV I "|" FORMAT)
       01  WS-FIELDS.
           05  WS-F1                   PIC X(40).
           05  WS-F2                   PIC X(40).
           05  WS-F3                   PIC X(40).
           05  WS-F4                   PIC X(40).
           05  WS-F5                   PIC X(40).
           05  WS-F6                   PIC X(40).
       01  WS-FCNT                     PIC 9(02).

      *    ULAZNI PODACI JEDNOG DOGAĐAJA, PRIJE NORMALIZACIJE
       01  WS-IN-EVENT.
           05  WS-IN-TRACK             PIC X(14).
           05  WS-IN-DATE              PIC X(10).
           05  WS-IN-TIME              PIC X(05).
           05  WS-IN-FORMAT            PIC X(03).
           05  WS-IN-CODE              PIC X(08).
           05  WS-IN-LOCATION          PIC X(20).
           05  WS-IN-NOTE              PIC X(30).

       01  WS-WORK-FIELDS.
           05  WS-LINE-NO              PIC 9(06)   VALUE ZERO.
           05  WS-REASON               PIC X(40).
           05  WS-BATCH-REASON         PIC X(50)   VALUE SPACES.
           05  WS-DETAIL-CNT           PIC 9(06)   VALUE ZERO.
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-OK               PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-REJ              PIC 9(05)   COMP-3 VALUE ZERO.
       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-LINE                PIC ZZZZZ9.
       01  WS-DISP-RC                  PIC Z9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-LINE
               UNTIL IN-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * KURIR IZ PARAMETRA, OTVARANJE DATOTEKA
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           ACCEPT WS-CARRIER FROM ENVIRONMENT 'PKT_CARRIER'
           SEARCH ALL PC-CARRIER
               AT END
                   DISPLAY 'PKTIMP   - NEPOZNAT KURIR U PKT_CARRIER: '
                           WS-CARRIER
                   MOVE 12 TO RETURN-CODE
                   STOP RUN
               WHEN PC-CAR-CODE (CX) = WS-CARRIER
                   DISPLAY 'PKTIMP   - START UVOZA, KURIR '
                           WS-CARRIER ' - ' PC-CAR-NAME (CX)
           END-SEARCH
           OPEN INPUT  CARR-IN
                OUTPUT EVT-OUT
           IF NOT FS-IN-OK OR NOT FS-OUT-OK
               DISPLAY 'PKTIMP   - GRESKA OTVARANJA DATOTEKA: '
                       WS-FS-IN ' ' WS-FS-OUT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8000-READ-LINE.

      *----------------------------------------------------------------*
      * JEDAN RED - FORMAT ZAVISI OD KURIRA
      *----------------------------------------------------------------*
       2000-PROCESS-LINE.
           INITIALIZE WS-IN-EVENT
           MOVE SPACES TO WS-REASON
           EVALUATE WS-CARRIER
               WHEN 'BRZ'  PERFORM 3000-BRZ-LINE
               WHEN 'EUP'  PERFORM 4000-EUP-LINE
               WHEN 'CIT'  PERFORM 5000-CIT-LINE
           END-EVALUATE
           PERFORM 8000-READ-LINE.

      *----------------------------------------------------------------*
      * BRZ: FIKSNA DUŽINA, H / D / T SLOGOVI
      *----------------------------------------------------------------*
       3000-BRZ-LINE.
           MOVE CARR-REC TO BRZ-RECORD
           EVALUATE TRUE
               WHEN BZ-HEADER-REC
                   IF HEADER-SEEN OR BZ-H-CARRIER NOT = 'BRZ'
                       MOVE 'NEISPRAVNO ZAGLAVLJE SERIJE'
                         TO WS-BATCH-REASON
                       SET BATCH-REJECTED TO TRUE
                   END-IF
                   SET HEADER-SEEN TO TRUE
               WHEN BZ-DETAIL-REC
                   IF NOT HEADER-SEEN
                       MOVE 'STAVKA PRIJE ZAGLAVLJA SERIJE'
                         TO WS-BATCH-REASON
                       SET BATCH-REJECTED TO TRUE
                   END-IF
                   ADD 1 TO WS-DETAIL-CNT
                   MOVE BZ-D-TRACK-NO  TO WS-IN-TRACK
                   MOVE BZ-D-DATE      TO WS-IN-DATE
                   MOVE BZ-D-TIME      TO WS-IN-TIME
                   MOVE 'NUM'          TO WS-IN-FORMAT
                   MOVE BZ-D-CODE      TO WS-IN-CODE
                   MOVE BZ-D-LOCATION  TO WS-IN-LOCATION
                   MOVE BZ-D-NOTE      TO WS-IN-NOTE
                   PERFORM 6000-BUILD-EVENT
               WHEN BZ-TRAILER-REC
                   SET TRAILER-SEEN TO TRUE
      *            KONTROLNI ZBIR: BROJ STAVKI MORA SE SLAGATI
                   IF BZ-T-COUNT NOT = WS-DETAIL-CNT
                       STRING 'KONTROLNI ZBIR ' BZ-T-COUNT
                              ' <> STAVKI ' WS-DETAIL-CNT
                              DELIMITED BY SIZE INTO WS-BATCH-REASON
                       END-STRING
                       SET BATCH-REJECTED TO TRUE
                   END-IF
               WHEN OTHER
                   MOVE 'NEPOZNAT TIP SLOGA' TO WS-REASON
                   PERFORM 7000-REJECT-LINE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EUP: CSV, PRVI RED JE ZAGLAVLJE KOLONA
      *----------------------------------------------------------------*
       4000-EUP-LINE.
           IF WS-LINE-NO = 1 AND CARR-REC (1:8) = 'TRACKING'
               CONTINUE
           ELSE
               MOVE SPACES TO WS-FIELDS
               MOVE ZERO   TO WS-FCNT
               UNSTRING CARR-REC DELIMITED BY ';'
                   INTO WS-F1 WS-F2 WS-F3 WS-F4 WS-F5
                   TALLYING IN WS-FCNT
               END-UNSTRING
               IF WS-FCNT < 4
                   MOVE 'NEPOTPUN CSV SLOG' TO WS-REASON
                   PERFORM 7000-REJECT-LINE
               ELSE
                   MOVE WS-F1          TO WS-IN-TRACK
                   MOVE WS-F2 (1:10)   TO WS-IN-DATE
                   MOVE WS-F2 (12:5)   TO WS-IN-TIME
                   MOVE 'ISO'          TO WS-IN-FORMAT
                   MOVE WS-F3          TO WS-IN-CODE
                   MOVE WS-F4          TO WS-IN-LOCATION
                   MOVE WS-F5          TO WS-IN-NOTE
                   PERFORM 6000-BUILD-EVENT
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * CIT: DATUM|VRIJEME|POSILJKA|KOD|MJESTO|NAPOMENA
      *----------------------------------------------------------------*
       5000-CIT-LINE.
           MOVE SPACES TO WS-FIELDS
           MOVE ZERO   TO WS-FCNT
           UNSTRING CARR-REC DELIMITED BY '|'
               INTO WS-F1 WS-F2 WS-F3 WS-F4 WS-F5 WS-F6
               TALLYING IN WS-FCNT
           END-UNSTRING
           IF WS-FCNT < 5
               MOVE 'NEPOTPUN SLOG (MANJE OD 5 POLJA)' TO WS-REASON
               PERFORM 7000-REJECT-LINE
           ELSE
               MOVE WS-F1          TO WS-IN-DATE
               MOVE WS-F2          TO WS-IN-TIME
               MOVE WS-F3          TO WS-IN-TRACK
               MOVE 'DMY'          TO WS-IN-FORMAT
               MOVE WS-F4          TO WS-IN-CODE
               MOVE WS-F5          TO WS-IN-LOCATION
               MOVE WS-F6          TO WS-IN-NOTE
               PERFORM 6000-BUILD-EVENT
           END-IF.

      *----------------------------------------------------------------*
      * ZAJEDNIČKO ZA SVE KURIRE: PROVJERA I NORMALIZACIJA
      *----------------------------------------------------------------*
       6000-BUILD-EVENT.
           IF WS-IN-TRACK = SPACES
               MOVE 'BROJ POSILJKE JE PRAZAN' TO WS-REASON
               PERFORM 7000-REJECT-LINE
           ELSE
               MOVE 'CNV'          TO LP-FUNC
               MOVE WS-IN-FORMAT   TO LP-FORMAT
               MOVE WS-IN-DATE     TO LP-DATE-IN
               MOVE WS-IN-TIME     TO LP-TIME-IN
               CALL 'PKTDATE' USING DATE-PARM
               IF LP-RC NOT = 0
                   MOVE 'NEISPRAVAN DATUM ILI VRIJEME' TO WS-REASON
                   PERFORM 7000-REJECT-LINE
               ELSE
                   MOVE 'M'        TO SP-FUNC
                   MOVE WS-CARRIER TO SP-CARRIER
                   MOVE WS-IN-CODE TO SP-CODE
                   CALL 'PKTSTAT' USING STAT-PARM
                   IF SP-RC NOT = 0
                       STRING 'NEPOZNAT STATUSNI KOD: ' WS-IN-CODE
                              DELIMITED BY SIZE INTO WS-REASON
                       END-STRING
                       PERFORM 7000-REJECT-LINE
                   ELSE
                       PERFORM 6100-WRITE-EVENT
                   END-IF
               END-IF
           END-IF.

       6100-WRITE-EVENT.
           MOVE SPACES          TO EVT-RECORD
           MOVE WS-IN-TRACK     TO EV-TRACK-NO
           MOVE LP-TS-OUT       TO EV-TS
           MOVE WS-CARRIER      TO EV-CARRIER
           MOVE WS-IN-CODE      TO EV-CARRIER-CODE
           MOVE SP-STATUS       TO EV-STATUS
           MOVE WS-IN-LOCATION  TO EV-LOCATION
           MOVE WS-IN-NOTE      TO EV-NOTE
           WRITE EVT-RECORD
           ADD 1 TO WS-CNT-OK.

       7000-REJECT-LINE.
           ADD 1 TO WS-CNT-REJ
           MOVE 4 TO WS-RC
           MOVE WS-LINE-NO TO WS-DISP-LINE
           DISPLAY 'PKTIMP   - [' WS-CARRIER '] ODBIJEN RED'
                   WS-DISP-LINE ': ' WS-REASON.

       8000-READ-LINE.
           READ CARR-IN
               AT END
                   SET IN-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-LINE-NO
           END-READ.

      *----------------------------------------------------------------*
      * KRAJ: KOD BRZ PROVJERA KOMPLETNOSTI SERIJE
      *----------------------------------------------------------------*
       9000-TERMINATE.
           IF WS-CARRIER = 'BRZ'
               IF NOT HEADER-SEEN OR NOT TRAILER-SEEN
                   MOVE 'NEDOSTAJE ZAGLAVLJE ILI ZAVRSNI SLOG'
                     TO WS-BATCH-REASON
                   SET BATCH-REJECTED TO TRUE
               END-IF
           END-IF
           CLOSE CARR-IN EVT-OUT
           IF BATCH-REJECTED
      *        SERIJA SE ODBACUJE U CJELINI - IZLAZ SE PRAZNI
               OPEN OUTPUT EVT-OUT
               CLOSE EVT-OUT
               DISPLAY 'PKTIMP   - *** SERIJA ODBACENA: '
                       WS-BATCH-REASON
               MOVE ZERO TO WS-CNT-OK
               MOVE 8 TO WS-RC
           END-IF
           MOVE WS-CNT-OK  TO WS-DISP-CNT
           DISPLAY 'PKTIMP   - UVEZENO DOGADJAJA  : ' WS-DISP-CNT
           MOVE WS-CNT-REJ TO WS-DISP-CNT
           DISPLAY 'PKTIMP   - ODBACENO REDOVA    : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'PKTIMP   - KRAJ, RC=' WS-DISP-RC.
