      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PKTSTAT.
       AUTHOR.        BORIS JANJETOVIC.
       INSTALLATION.  TAIPANSTUDIO.
       DATE-WRITTEN.  2019-12-18.
      *================================================================*
      * SISTEM  : PACKTRACK - UNIVERZALNO PRAĆENJE POŠILJAKA
      * PROGRAM : PKTSTAT - POTPROGRAM (CALL 'PKTSTAT' USING ...)
      * OPIS    : PREVOĐENJE STATUSNIH KODOVA KURIRA U UNIVERZALNE
      *           STATUSE PACKTRACK SISTEMA I TEKSTOVI STATUSA.
      *           SVAKI KURIR IMA SVOJE KODOVE ("40", "DELIV", "D"),
      *           A WEB SHOP I KUPAC VIDE UVIJEK ISTI STATUS ("DL").
      *           TABELA JE SORTIRANA PO KURIRU I KODU, PA SE
      *           PRETRAŽUJE BINARNO (SEARCH ALL).
      * PARAMETRI: VIDI COPYBOOK STATPARM
      *----------------------------------------------------------------*
      * UNIVERZALNI STATUSI:
      *   RG REGISTROVANA       PU PREUZETA          TR U TRANZITU
      *   HB SORTIRNI CENTAR    OD NA DOSTAVI        DL ISPORUČENA *
      *   FA NEUSPJELA DOSTAVA  RT VRAĆENA *         EX IZUZETAK
      *   (* = ZAVRŠNI STATUS, POŠILJKA SE VIŠE NE MIJENJA)
      *----------------------------------------------------------------*
      * ISTORIJA IZMJENA:
      * DATUM      AUTOR  OPIS
      * 2019-12-20 BJ     PRVA VERZIJA
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    KURIR(3) + KOD KURIRA(8) + UNIVERZALNI STATUS(2)
      *    MORA OSTATI SORTIRANO RASTUĆE PO PRVIH 11 ZNAKOVA!
       01  WS-MAP-DATA.
           05  FILLER  PIC X(13)  VALUE 'BRZ01      PU'.
           05  FILLER  PIC X(13)  VALUE 'BRZ10      TR'.
           05  FILLER  PIC X(13)  VALUE 'BRZ20      HB'.
           05  FILLER  PIC X(13)  VALUE 'BRZ30      OD'.
           05  FILLER  PIC X(13)  VALUE 'BRZ40      DL'.
           05  FILLER  PIC X(13)  VALUE 'BRZ50      FA'.
           05  FILLER  PIC X(13)  VALUE 'BRZ60      RT'.
           05  FILLER  PIC X(13)  VALUE 'BRZ90      EX'.
           05  FILLER  PIC X(13)  VALUE 'CITD       DL'.
           05  FILLER  PIC X(13)  VALUE 'CITF       FA'.
           05  FILLER  PIC X(13)  VALUE 'CITO       OD'.
           05  FILLER  PIC X(13)  VALUE 'CITP       PU'.
           05  FILLER  PIC X(13)  VALUE 'CITR       RT'.
           05  FILLER  PIC X(13)  VALUE 'CITS       HB'.
           05  FILLER  PIC X(13)  VALUE 'CITT       TR'.
           05  FILLER  PIC X(13)  VALUE 'CITX       EX'.
           05  FILLER  PIC X(13)  VALUE 'EUPDAMAGE  EX'.
           05  FILLER  PIC X(13)  VALUE 'EUPDELIV   DL'.
           05  FILLER  PIC X(13)  VALUE 'EUPHUB_IN  HB'.
           05  FILLER  PIC X(13)  VALUE 'EUPNO_ONE  FA'.
           05  FILLER  PIC X(13)  VALUE 'EUPOUT_DEL OD'.
           05  FILLER  PIC X(13)  VALUE 'EUPPICKUP  PU'.
           05  FILLER  PIC X(13)  VALUE 'EUPRETURN  RT'.
           05  FILLER  PIC X(13)  VALUE 'EUPTRANSIT TR'.
       01  WS-MAP-TABLE REDEFINES WS-MAP-DATA.
           05  WS-MAP-ENTRY            OCCURS 24 TIMES
                                       ASCENDING KEY IS MT-KEY
                                       INDEXED BY MX.
               10  MT-KEY.
                   15  MT-CARRIER      PIC X(03).
                   15  MT-CODE         PIC X(08).
               10  MT-STATUS           PIC X(02).

      *    STATUS(2) + ZAVRŠNI(1) + TEKST(25)
       01  WS-TEXT-DATA.
           05  FILLER PIC X(28) VALUE 'RGNREGISTROVANA'.
           05  FILLER PIC X(28) VALUE 'PUNPREUZETA OD POSILJAOCA'.
           05  FILLER PIC X(28) VALUE 'TRNU TRANZITU'.
           05  FILLER PIC X(28) VALUE 'HBNU SORTIRNOM CENTRU'.
           05  FILLER PIC X(28) VALUE 'ODNNA DOSTAVI'.
           05  FILLER PIC X(28) VALUE 'DLYISPORUCENA'.
           05  FILLER PIC X(28) VALUE 'FANNEUSPJELA DOSTAVA'.
           05  FILLER PIC X(28) VALUE 'RTYVRACENA POSILJAOCU'.
           05  FILLER PIC X(28) VALUE 'EXNOSTECENA / IZUZETAK'.
       01  WS-TEXT-TABLE REDEFINES WS-TEXT-DATA.
           05  WS-TEXT-ENTRY           OCCURS 9 TIMES
                                       INDEXED BY TX.
               10  TT-STATUS           PIC X(02).
               10  TT-FINAL            PIC X(01).
               10  TT-TEXT             PIC X(25).

       01  WS-SEARCH-KEY.
           05  WS-SK-CARRIER           PIC X(03).
           05  WS-SK-CODE              PIC X(08).

       LINKAGE SECTION.
           COPY STATPARM.

       PROCEDURE DIVISION USING STAT-PARM.
       0000-MAIN.
           MOVE 0 TO SP-RC
           EVALUATE SP-FUNC
               WHEN 'M'
                   PERFORM 1000-MAP-CODE
               WHEN 'T'
                   PERFORM 2000-STATUS-TEXT
               WHEN OTHER
                   MOVE 9 TO SP-RC
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------*
      * KOD KURIRA -> UNIVERZALNI STATUS (BINARNA PRETRAGA)
      *----------------------------------------------------------------*
       1000-MAP-CODE.
           MOVE SP-CARRIER TO WS-SK-CARRIER
           MOVE SP-CODE    TO WS-SK-CODE
           MOVE SPACES     TO SP-STATUS SP-TEXT
           SEARCH ALL WS-MAP-ENTRY
               AT END
                   MOVE 1 TO SP-RC
               WHEN MT-KEY (MX) = WS-SEARCH-KEY
                   MOVE MT-STATUS (MX) TO SP-STATUS
                   PERFORM 2000-STATUS-TEXT
           END-SEARCH.

      *----------------------------------------------------------------*
      * UNIVERZALNI STATUS -> TEKST I OZNAKA ZAVRŠNOG STATUSA
      * (MALA TABELA - DOVOLJNA JE SEKVENCIJALNA PRETRAGA)
      *----------------------------------------------------------------*
       2000-STATUS-TEXT.
           SET TX TO 1
           SEARCH WS-TEXT-ENTRY
               AT END
                   MOVE 1 TO SP-RC
                   MOVE '*** NEPOZNAT STATUS ***' TO SP-TEXT
                   MOVE 'N' TO SP-FINAL
               WHEN TT-STATUS (TX) = SP-STATUS
                   MOVE TT-TEXT  (TX) TO SP-TEXT
                   MOVE TT-FINAL (TX) TO SP-FINAL
           END-SEARCH.
