      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTUPD.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2020-01-11.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTUPD - KORAK 030 DNEVNE OBRADE
      * OPIS    : AŽURIRANJE STATUSA POŠILJAKA.
      *           1. SORT DOGAĐAJA SVIH KURIRA (TRI DATOTEKE) PO
      *              POŠILJCI I VREMENU - JEDNA HRONOLOGIJA.
      *           2. ZA SVAKI DOGAĐAJ: SLUČAJNO ČITANJE POŠILJKE PO
      *              KLJUČU I AŽURIRANJE NA MJESTU (REWRITE), KAO
      *              KOD VSAM KSDS DATOTEKE.
      *           PRAVILA:
      *             - POŠILJKA U ZAVRŠNOM STATUSU (DL, RT) SE VIŠE
      *               NE MIJENJA (KASNO SKENIRANJE SE IGNORIŠE)
      *             - DOGAĐAJ NE NOVIJI OD ZADNJEG (DUPLIKAT) SE
      *               IGNORIŠE
      *             - DOGAĐAJ DRUGOG KURIRA SE IGNORIŠE
      *             - FA POVEĆAVA BROJAČ NEUSPJELIH POKUŠAJA
      *           PRIMIJENJENI DOGAĐAJI IDU U ISTORIJU (OPEN EXTEND),
      *           A ZA BITNE STATUSE SE PRAVI OBAVJEŠTENJE ZA KUPCA.
      * ULAZ    : EVTBRZ, EVTEUP, EVTCIT - DOGAĐAJI (IZ PKTIMP)
      * I-O     : SHIPMAST - MATIČNE POŠILJKE
      * IZLAZ   : EVTSRT   - SORTIRANI DOGAĐAJI (RADNA DATOTEKA)
      *           EVTHIST  - ISTORIJA DOGAĐAJA (DOPISUJE SE)
      *           NOTIFY   - OBAVJEŠTENJA ZA SLANJE
      *           UPDRPT   - DNEVNIK AŽURIRANJA, 132 KOLONE
      * POVRATNI KODOVI (RETURN-CODE):
      *            0 - OBRADA USPJEŠNA
      *            4 - DOGAĐAJI ZA NEPOZNATE POŠILJKE
      *           12 - FATALNA GREŠKA
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2020-02-17 BJ     PRVA VERZIJA
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EVT-BRZ      ASSIGN TO 'EVTBRZ'
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EVT-EUP      ASSIGN TO 'EVTEUP'
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EVT-CIT      ASSIGN TO 'EVTCIT'
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-EVT     ASSIGN TO 'SORTWK'.
           SELECT EVT-SRT      ASSIGN TO 'EVTSRT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-SRT.
           COPY SHPSEL.
      *    OPTIONAL: PRVI DAN ISTORIJA NE POSTOJI, EXTEND JE KREIRA
           SELECT OPTIONAL HIST-FILE ASSIGN TO 'EVTHIST'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-HIST.
           SELECT NOTIFY-FILE  ASSIGN TO 'NOTIFY'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-NTF.
           SELECT UPD-RPT      ASSIGN TO 'UPDRPT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-RPT.

       DATA DIVISION.
       FILE SECTION.
       FD  EVT-BRZ.
       01  EVT-BRZ-REC                 PIC X(100).
       FD  EVT-EUP.
       01  EVT-EUP-REC                 PIC X(100).
       FD  EVT-CIT.
       01  EVT-CIT-REC                 PIC X(100).
       SD  SORT-EVT.
       01  SE-RECORD.
           05  SE-TRACK-NO             PIC X(14).
           05  SE-TS                   PIC 9(12).
           05  FILLER                  PIC X(74).
       FD  EVT-SRT.
           COPY EVTREC.
       FD  SHIP-MASTER.
           COPY SHPREC.
       FD  HIST-FILE.
       01  HIST-REC                    PIC X(100).
       FD  NOTIFY-FILE.
           COPY NTFREC.
       FD  UPD-RPT.
       01  RPT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-SRT               PIC X(02).
               88  FS-SRT-OK           VALUE '00'.
           05  WS-FS-SHIP              PIC X(02).
               88  FS-SHIP-OK          VALUE '00'.
           05  WS-FS-HIST              PIC X(02).
               88  FS-HIST-OK          VALUE '00' '05'.
           05  WS-FS-NTF               PIC X(02).
               88  FS-NTF-OK           VALUE '00'.
           05  WS-FS-RPT               PIC X(02).
               88  FS-RPT-OK           VALUE '00'.

           COPY PKTCONST.
           COPY RUNDATE.

       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)   VALUE 'N'.
               88  EVT-EOF             VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01).
               88  SHIP-FOUND          VALUE 'Y'.
               88  SHIP-NOT-FOUND      VALUE 'N'.

       01  WS-WORK-FIELDS.
           05  WS-RESULT               PIC X(32).
           05  WS-NOTIFY-FLAG          PIC X(05).
           05  WS-CAR-NAME             PIC X(15).
           05  WS-ATT-ED               PIC 9.
           05  WS-RC                   PIC 9(02)   VALUE ZERO.

       01  WS-COUNTERS.
           05  WS-CNT-READ             PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-APPLIED          PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-DUP              PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-CLOSED           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-WRONG-CAR        PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-ORPHAN           PIC 9(05)   COMP-3 VALUE ZERO.
           05  WS-CNT-NOTIFY           PIC 9(05)   COMP-3 VALUE ZERO.
       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-RC                  PIC Z9.

      *----------------------------------------------------------------*
      * DNEVNIK AŽURIRANJA - 132 KOLONE
      *----------------------------------------------------------------*
       01  WS-HDR1.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  HDR1-COMPANY            PIC X(40).
           05  FILLER                  PIC X(91)   VALUE
                   'PACKTRACK - DNEVNIK AZURIRANJA STATUSA'.
       01  WS-HDR2.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(40)   VALUE
                   'PROGRAM: PKTUPD'.
           05  FILLER                  PIC X(14)   VALUE
                   'DATUM OBRADE: '.
           05  HDR2-DATE               PIC X(10).
           05  FILLER                  PIC X(67)   VALUE SPACES.
       01  WS-HDR3.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(14)  VALUE 'POSILJKA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(03)  VALUE 'KUR'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(16)  VALUE 'VRIJEME'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(08)  VALUE 'KOD KUR.'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(02)  VALUE 'ST'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(20)  VALUE 'LOKACIJA'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(32)  VALUE 'REZULTAT'.
           05  FILLER  PIC X(01)  VALUE SPACE.
           05  FILLER  PIC X(05)  VALUE 'OBAV.'.
           05  FILLER  PIC X(24)  VALUE SPACES.
       01  WS-HDR4.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  FILLER                  PIC X(107)  VALUE ALL '-'.
           05  FILLER                  PIC X(24)   VALUE SPACES.
       01  WS-DETAIL.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-TRACK-NO             PIC X(14).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-CARRIER              PIC X(03).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-TS                   PIC X(16).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-CODE                 PIC X(08).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-STATUS               PIC X(02).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-LOCATION             PIC X(20).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-RESULT               PIC X(32).
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  DL-NOTIFY               PIC X(05).
           05  FILLER                  PIC X(24)   VALUE SPACES.
       01  WS-TOTAL-LINE.
           05  FILLER                  PIC X(01)   VALUE SPACE.
           05  TL-LABEL                PIC X(40).
           05  TL-COUNT                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(85)   VALUE SPACES.

       PROCEDURE DIVISION.
      *================================================================*
      * GLAVNA PETLJA
      *================================================================*
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-EVENT
               UNTIL EVT-EOF
           PERFORM 9000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
      * SORT SVIH DOGAĐAJA U JEDNU HRONOLOGIJU, OTVARANJE DATOTEKA
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           DISPLAY 'PKTUPD   - START AZURIRANJA STATUSA'
           PERFORM 8900-GET-RUN-DATE
           MOVE PC-COMPANY-1    TO HDR1-COMPANY
           MOVE WS-RUN-DATE-TXT TO HDR2-DATE
           SORT SORT-EVT
               ON ASCENDING KEY SE-TRACK-NO
               ON ASCENDING KEY SE-TS
               USING  EVT-BRZ EVT-EUP EVT-CIT
               GIVING EVT-SRT
           IF SORT-RETURN NOT = ZERO
               DISPLAY 'PKTUPD   - GRESKA U SORTU DOGADJAJA'
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           OPEN INPUT  EVT-SRT
                I-O    SHIP-MASTER
                EXTEND HIST-FILE
                OUTPUT NOTIFY-FILE UPD-RPT
           IF NOT FS-SRT-OK OR NOT FS-SHIP-OK OR NOT FS-HIST-OK
              OR NOT FS-NTF-OK OR NOT FS-RPT-OK
               DISPLAY 'PKTUPD   - GRESKA OTVARANJA DATOTEKA: '
                       WS-FS-SRT ' ' WS-FS-SHIP ' ' WS-FS-HIST ' '
                       WS-FS-NTF ' ' WS-FS-RPT
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           WRITE RPT-LINE FROM WS-HDR1 AFTER ADVANCING PAGE
           WRITE RPT-LINE FROM WS-HDR2 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR3 AFTER ADVANCING 1 LINE
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           PERFORM 8000-READ-EVENT.

      *----------------------------------------------------------------*
      * JEDAN DOGAĐAJ: SLUČAJNO ČITANJE POŠILJKE I PRAVILA
      *----------------------------------------------------------------*
       2000-PROCESS-EVENT.
           MOVE SPACES TO WS-RESULT WS-NOTIFY-FLAG
           MOVE EV-TRACK-NO TO SM-TRACK-NO
           READ SHIP-MASTER KEY IS SM-TRACK-NO
               INVALID KEY
                   SET SHIP-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET SHIP-FOUND TO TRUE
           END-READ
           EVALUATE TRUE
               WHEN SHIP-NOT-FOUND
                   MOVE 'NEPOZNATA POSILJKA' TO WS-RESULT
                   ADD 1 TO WS-CNT-ORPHAN
                   MOVE 4 TO WS-RC
               WHEN SM-FINAL
                   MOVE 'IGNORISAN - POSILJKA ZATVORENA'
                     TO WS-RESULT
                   ADD 1 TO WS-CNT-CLOSED
               WHEN EV-TS NOT > SM-STATUS-TS
                   MOVE 'IGNORISAN - DUPLI/STARIJI DOGADJ'
                     TO WS-RESULT
                   ADD 1 TO WS-CNT-DUP
               WHEN EV-CARRIER NOT = SM-CARRIER
                   MOVE 'IGNORISAN - DRUGI KURIR' TO WS-RESULT
                   ADD 1 TO WS-CNT-WRONG-CAR
               WHEN OTHER
                   PERFORM 3000-APPLY-EVENT
           END-EVALUATE
           PERFORM 4000-PRINT-LINE
           PERFORM 8000-READ-EVENT.

      *----------------------------------------------------------------*
      * PRIMJENA: NOVI STATUS, AŽURIRANJE NA MJESTU, ISTORIJA
      *----------------------------------------------------------------*
       3000-APPLY-EVENT.
           MOVE EV-STATUS   TO SM-STATUS
           MOVE EV-TS       TO SM-STATUS-TS
           MOVE EV-LOCATION TO SM-LOCATION
           ADD 1 TO SM-EVENT-CNT
           EVALUATE EV-STATUS
               WHEN 'FA'
                   IF SM-ATTEMPTS < 9
                       ADD 1 TO SM-ATTEMPTS
                   END-IF
               WHEN 'DL'
                   MOVE EV-TS TO SM-DELIVERED-TS
           END-EVALUATE
           REWRITE SHIP-RECORD
               INVALID KEY
                   DISPLAY 'PKTUPD   - *** REWRITE NIJE USPIO: '
                           SM-TRACK-NO ' STATUS ' WS-FS-SHIP
                   MOVE 12 TO RETURN-CODE
                   STOP RUN
           END-REWRITE
           WRITE HIST-REC FROM EVT-RECORD
           ADD 1 TO WS-CNT-APPLIED
           MOVE 'PRIMIJENJEN' TO WS-RESULT
           IF EV-STATUS = 'OD' OR 'DL' OR 'FA' OR 'RT' OR 'EX'
               PERFORM 3100-CREATE-NOTIFICATION
           END-IF.

      *----------------------------------------------------------------*
      * OBAVJEŠTENJE ZA KUPCA - TEKST ZAVISI OD STATUSA
      *----------------------------------------------------------------*
       3100-CREATE-NOTIFICATION.
           MOVE SPACES TO NTF-RECORD WS-CAR-NAME
           SEARCH ALL PC-CARRIER
               WHEN PC-CAR-CODE (CX) = SM-CARRIER
                   MOVE PC-CAR-NAME (CX) TO WS-CAR-NAME
           END-SEARCH
           MOVE SM-STATUS-TS TO WS-TS-IN
           PERFORM 8920-FORMAT-TS
           MOVE 'EMAIL'         TO NT-CHANNEL
           MOVE SM-RCPT-CONTACT TO NT-CONTACT
           MOVE SM-TRACK-NO     TO NT-TRACK-NO
           MOVE SM-ORDER-NO     TO NT-ORDER-NO
           MOVE SM-STATUS       TO NT-STATUS
           MOVE SM-STATUS-TS    TO NT-TS
           EVALUATE SM-STATUS
               WHEN 'OD'
                   STRING 'POSILJKA ' SM-TRACK-NO
                          ' JE DANAS NA DOSTAVI. KURIR: '
                          FUNCTION TRIM (WS-CAR-NAME) '.'
                          DELIMITED BY SIZE INTO NT-MESSAGE
                   END-STRING
               WHEN 'DL'
                   STRING 'POSILJKA ' SM-TRACK-NO
                          ' JE ISPORUCENA ' WS-TS-OUT '.'
                          DELIMITED BY SIZE INTO NT-MESSAGE
                   END-STRING
               WHEN 'FA'
                   MOVE SM-ATTEMPTS TO WS-ATT-ED
                   STRING 'NEUSPJESAN POKUSAJ DOSTAVE POSILJKE '
                          SM-TRACK-NO ' (POKUSAJ ' WS-ATT-ED ').'
                          DELIMITED BY SIZE INTO NT-MESSAGE
                   END-STRING
               WHEN 'RT'
                   STRING 'POSILJKA ' SM-TRACK-NO
                          ' SE VRACA POSILJAOCU.'
                          DELIMITED BY SIZE INTO NT-MESSAGE
                   END-STRING
               WHEN OTHER
                   STRING 'PROBLEM SA POSILJKOM ' SM-TRACK-NO
                          '. KONTAKTIRACEMO VAS.'
                          DELIMITED BY SIZE INTO NT-MESSAGE
                   END-STRING
           END-EVALUATE
           WRITE NTF-RECORD
           ADD 1 TO WS-CNT-NOTIFY
           MOVE 'EMAIL' TO WS-NOTIFY-FLAG.

       4000-PRINT-LINE.
           MOVE EV-TRACK-NO     TO DL-TRACK-NO
           MOVE EV-CARRIER      TO DL-CARRIER
           MOVE EV-TS           TO WS-TS-IN
           PERFORM 8920-FORMAT-TS
           MOVE WS-TS-OUT       TO DL-TS
           MOVE EV-CARRIER-CODE TO DL-CODE
           MOVE EV-STATUS       TO DL-STATUS
           MOVE EV-LOCATION     TO DL-LOCATION
           MOVE WS-RESULT       TO DL-RESULT
           MOVE WS-NOTIFY-FLAG  TO DL-NOTIFY
           WRITE RPT-LINE FROM WS-DETAIL AFTER ADVANCING 1 LINE.

       8000-READ-EVENT.
           READ EVT-SRT
               AT END
                   SET EVT-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-CNT-READ
           END-READ.

           COPY GETDATE.

       9000-TERMINATE.
           WRITE RPT-LINE FROM WS-HDR4 AFTER ADVANCING 1 LINE
           MOVE 'DOGADJAJA UKUPNO ......................:' TO TL-LABEL
           MOVE WS-CNT-READ TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  PRIMIJENJENO ........................:' TO TL-LABEL
           MOVE WS-CNT-APPLIED TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  IGNORISANO - DUPLI ILI STARIJI ......:' TO TL-LABEL
           MOVE WS-CNT-DUP TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  IGNORISANO - POSILJKA ZATVORENA .....:' TO TL-LABEL
           MOVE WS-CNT-CLOSED TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  IGNORISANO - DRUGI KURIR ............:' TO TL-LABEL
           MOVE WS-CNT-WRONG-CAR TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE '  NEPOZNATA POSILJKA ..................:' TO TL-LABEL
           MOVE WS-CNT-ORPHAN TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE 'PRIPREMLJENO OBAVJESTENJA .............:' TO TL-LABEL
           MOVE WS-CNT-NOTIFY TO TL-COUNT
           WRITE RPT-LINE FROM WS-TOTAL-LINE AFTER ADVANCING 1 LINE
           MOVE ' *** KRAJ IZVJESTAJA ***' TO RPT-LINE
           WRITE RPT-LINE AFTER ADVANCING 2 LINES
           CLOSE EVT-SRT SHIP-MASTER HIST-FILE NOTIFY-FILE UPD-RPT
           MOVE WS-CNT-READ    TO WS-DISP-CNT
           DISPLAY 'PKTUPD   - DOGADJAJA UKUPNO     : ' WS-DISP-CNT
           MOVE WS-CNT-APPLIED TO WS-DISP-CNT
           DISPLAY 'PKTUPD   - PRIMIJENJENO         : ' WS-DISP-CNT
           MOVE WS-CNT-ORPHAN  TO WS-DISP-CNT
           DISPLAY 'PKTUPD   - NEPOZNATIH POSILJAKA : ' WS-DISP-CNT
           MOVE WS-CNT-NOTIFY  TO WS-DISP-CNT
           DISPLAY 'PKTUPD   - OBAVJESTENJA         : ' WS-DISP-CNT
           MOVE WS-RC TO RETURN-CODE
           MOVE WS-RC TO WS-DISP-RC
           DISPLAY 'PKTUPD   - KRAJ, RC=' WS-DISP-RC.
