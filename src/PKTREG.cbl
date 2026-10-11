      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTREG.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2019-12-20.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTREG - KORAK 010 DNEVNE OBRADE
      * OPIS    : REGISTRACIJA NOVIH POŠILJAKA IZ MANIFESTA KOJI
      *           ŠALJU WEB SHOPOVI. ZA SVAKU POŠILJKU:
      *             - PROVJERA KURIRA (SEARCH ALL U TABELI KURIRA)
      *             - PROVJERA DATUMA PREDAJE (CALL 'PKTDATE')
      *             - OBEĆANI DATUM = PREDAJA + N RADNIH DANA KURIRA
      *             - UPIS U INDEKSNU MATIČNU DATOTEKU SA STATUSOM RG
      *           DUPLI BROJ POŠILJKE SE ODBIJA (INVALID KEY).
      *           AKO MATIČNA DATOTEKA JOŠ NE POSTOJI (STATUS 35),
      *           KREIRA SE - PRVI DAN RADA SISTEMA.
      * ULAZ    : MANIN    - NOVE POŠILJKE (MANREC)
      * I-O     : SHIPMAST - MATIČNE POŠILJKE (SHPREC, INDEKSNA)
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - SVE POŠILJKE REGISTROVANE
      *            4 - NEKE POŠILJKE ODBIJENE
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2020-03-09 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MANIFEST     ASSIGN TO 'MANIN'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-MAN.
           COPY SHPSEL.

       DATA DIVISION.
       FILE SECTION.
       FD  MANIFEST.
           COPY MANREC.
       FD  SHIP-MASTER.
           COPY SHPREC.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-MAN               PIC X(02).
               88  FS-MAN-OK           VALUE '00'.
           05  WS-FS-SHIP              PIC X(02).
               88  FS-SHIP-OK          VALUE '00'.
               88  FS-SHIP-NOT-FOUND   VALUE '35'.

           COPY PKTCONST.
           COPY DATEPARM.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  MAN-EOF             VALUE 'Y'.
           05  WS-VALID-SW             PIC X(01).
               88  REC-VALID           VALUE 'Y'.
               88  REC-INVALID         VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-REASON               PIC X(35).
           05  WS-CAR-DAYS             PIC 9(02).
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-READ             PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-OK               PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-REJ              PIC 9(05)   COMP-3 VALUE ZERO.
       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-SHIPMENT
               UNTIL MAN-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * OTVARANJE - MATIČNA SE KREIRA AKO NE POSTOJI
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'PKTREG   - START REGISTRACIJE POSILJAKA'
           OPEN INPUT MANIFEST
           IF NOT FS-MAN-OK
               DISPLAY 'PKTREG   - GRESKA OTVARANJA MANIFEST, STATUS '
                       WS-FS-MAN
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN I-O SHIP-MASTER
           IF FS-SHIP-NOT-FOUND
               DISPLAY 'PKTREG   - SHIPMAST NE POSTOJI, KREIRA SE'
               OPEN OUTPUT SHIP-MASTER
               CLOSE SHIP-MASTER
               OPEN I-O SHIP-MASTER
           END-IF
           IF NOT FS-SHIP-OK
               DISPLAY 'PKTREG   - GRESKA OTVARANJA SHIPMAST, STATUS '
                       WS-FS-SHIP
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           PERFORM 8000-READ-MANIFEST.

      *----------------------------------------------------------------*
      * JEDNA POŠILJKA
      *----------------------------------------------------------------*
       2000-PROCESS-SHIPMENT.
           PERFORM 2100-VALIDATE
           IF REC-VALID
               PERFORM 2200-BUILD-MASTER
               WRITE SHIP-RECORD
                   INVALID KEY
                       MOVE 'POSILJKA VEC POSTOJI' TO WS-REASON
                       SET REC-INVALID TO TRUE
                   NOT INVALID KEY
                       ADD 1 TO WS-CNT-OK
               END-WRITE
           END-IF
           IF REC-INVALID
               ADD 1 TO WS-CNT-REJ
               MOVE 4 TO WS-RC
               DISPLAY 'PKTREG   - ODBIJENA ' MN-TRACK-NO
                       ' (' MN-ORDER-NO '): ' WS-REASON
           END-IF
           PERFORM 8000-READ-MANIFEST.

       2100-VALIDATE.
           SET REC-VALID TO TRUE
           EVALUATE TRUE
               WHEN MN-TRACK-NO = SPACES
                   MOVE 'BROJ POSILJKE JE PRAZAN' TO WS-REASON
                   SET REC-INVALID TO TRUE
               WHEN MN-RCPT-NAME = SPACES
                   MOVE 'PRIMALAC NIJE NAVEDEN' TO WS-REASON
                   SET REC-INVALID TO TRUE
               WHEN OTHER
                   PERFORM 2110-CHECK-CARRIER
           END-EVALUATE
           IF REC-VALID
               PERFORM 2120-CALC-PROMISED-DATE
           END-IF.

      *    KURIR MORA POSTOJATI U TABELI - BINARNA PRETRAGA
       2110-CHECK-CARRIER.
           SEARCH ALL PC-CARRIER
               AT END
                   MOVE 'NEPOZNATA KURIRSKA SLUZBA' TO WS-REASON
                   SET REC-INVALID TO TRUE
               WHEN PC-CAR-CODE (CX) = MN-CARRIER
                   MOVE PC-CAR-DAYS (CX) TO WS-CAR-DAYS
           END-SEARCH.

      *    OBEĆANI DATUM ISPORUKE: PREDAJA + ROK KURIRA U RADNIM DANIMA
       2120-CALC-PROMISED-DATE.
           MOVE 'ADD'        TO LP-FUNC
           MOVE SPACES       TO LP-FORMAT LP-TIME-IN
           MOVE MN-SHIP-DATE TO LP-DATE-IN
           MOVE WS-CAR-DAYS  TO LP-DAYS
           CALL 'PKTDATE' USING DATE-PARM
           IF LP-RC NOT = 0
               MOVE 'NEISPRAVAN DATUM PREDAJE' TO WS-REASON
               SET REC-INVALID TO TRUE
           END-IF.

       2200-BUILD-MASTER.
           MOVE SPACES          TO SHIP-RECORD
           MOVE MN-TRACK-NO     TO SM-TRACK-NO
           MOVE MN-CARRIER      TO SM-CARRIER
           MOVE MN-ORDER-NO     TO SM-ORDER-NO
           MOVE MN-CLIENT       TO SM-CLIENT
           MOVE MN-RCPT-NAME    TO SM-RCPT-NAME
           MOVE MN-RCPT-CITY    TO SM-RCPT-CITY
           MOVE MN-RCPT-CONTACT TO SM-RCPT-CONTACT
           MOVE MN-SHIP-DATE    TO SM-SHIP-DATE
           MOVE LP-DATE-OUT     TO SM-PROMISED-DATE
           MOVE 'RG'            TO SM-STATUS
           COMPUTE SM-STATUS-TS = SM-SHIP-DATE * 10000
           MOVE 'PRIJEM POSILJKE' TO SM-LOCATION
           MOVE ZERO            TO SM-ATTEMPTS SM-DELIVERED-TS
                                   SM-EVENT-CNT.

       8000-READ-MANIFEST.
           READ MANIFEST
               AT END
                   SET MAN-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-READ
           END-READ.

       9000-TERMINATE.
           CLOSE MANIFEST SHIP-MASTER
           MOVE WS-CNT-READ TO WS-DISP-CNT
           DISPLAY 'PKTREG   - PROCITANO IZ MANIFESTA: ' WS-DISP-CNT
           MOVE WS-CNT-OK   TO WS-DISP-CNT
           DISPLAY 'PKTREG   - REGISTROVANO         : ' WS-DISP-CNT
           MOVE WS-CNT-REJ  TO WS-DISP-CNT
           DISPLAY 'PKTREG   - ODBIJENO             : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'PKTREG   - KRAJ, RC=' WS-DISP-RC.
