      *================================================================*
      * NTFREC   - SLOG OBAVJEŠTENJA ZA KUPCA (RED ZA SLANJE)
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * DUŽINA   : 200 BAJTOVA
      *----------------------------------------------------------------*
      * PKTUPD SAMO PRIPREMA PORUKE; SLANJE (E-MAIL GATEWAY) RADI
      * POSEBAN SERVIS KOJI ČITA OVU DATOTEKU.
      *================================================================*
       01  NTF-RECORD.
           05  NT-CHANNEL              PIC X(05).
           05  NT-CONTACT              PIC X(40).
           05  NT-TRACK-NO             PIC X(14).
           05  NT-ORDER-NO             PIC X(10).
           05  NT-STATUS               PIC X(02).
           05  NT-TS                   PIC 9(12).
           05  NT-MESSAGE              PIC X(100).
           05  FILLER                  PIC X(17).
