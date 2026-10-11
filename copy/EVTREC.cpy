      *================================================================*
      * EVTREC   - NORMALIZOVANI DOGAĐAJ PRAĆENJA
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * DUŽINA   : 100 BAJTOVA
      *----------------------------------------------------------------*
      * ZAJEDNIČKI FORMAT ZA SVE KURIRE. PIŠE GA PKTIMP (PO JEDNA
      * DATOTEKA ZA SVAKOG KURIRA), ČITA GA PKTUPD, A ISTI SLOG SE
      * ČUVA U ISTORIJI DOGAĐAJA.
      *================================================================*
       01  EVT-RECORD.
           05  EV-TRACK-NO             PIC X(14).
      *    VRIJEME DOGAĐAJA GGGGMMDDHHMM
           05  EV-TS                   PIC 9(12).
           05  EV-CARRIER              PIC X(03).
      *    ORIGINALNI KOD KURIRA - ČUVA SE RADI REKLAMACIJA
           05  EV-CARRIER-CODE         PIC X(08).
      *    UNIVERZALNI STATUS
           05  EV-STATUS               PIC X(02).
           05  EV-LOCATION             PIC X(20).
           05  EV-NOTE                 PIC X(30).
           05  FILLER                  PIC X(11).
