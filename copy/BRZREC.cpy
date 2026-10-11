      *================================================================*
      * BRZREC   - DATOTEKA KURIRA "BRZA POSTA" (FIKSNA DUŽINA)
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * DUŽINA   : 60 BAJTOVA
      *----------------------------------------------------------------*
      * KLASIČNA SERIJA: ZAGLAVLJE (H), STAVKE (D), ZAVRŠNI SLOG (T).
      * ZAVRŠNI SLOG NOSI BROJ STAVKI - AKO SE NE SLAŽE, CIJELA
      * SERIJA SE ODBACUJE (DATOTEKA JE OŠTEĆENA ILI NEPOTPUNA).
      *================================================================*
       01  BRZ-RECORD.
           05  BZ-REC-TYPE             PIC X(01).
               88  BZ-HEADER-REC       VALUE 'H'.
               88  BZ-DETAIL-REC       VALUE 'D'.
               88  BZ-TRAILER-REC      VALUE 'T'.
           05  BZ-DATA                 PIC X(59).
      *    H - ZAGLAVLJE SERIJE
           05  BZ-HEADER REDEFINES BZ-DATA.
               10  BZ-H-CARRIER        PIC X(03).
               10  BZ-H-FILE-DATE      PIC 9(08).
               10  BZ-H-SEQ            PIC 9(04).
               10  FILLER              PIC X(44).
      *    D - JEDAN DOGAĐAJ
           05  BZ-DETAIL REDEFINES BZ-DATA.
               10  BZ-D-TRACK-NO       PIC X(14).
               10  BZ-D-DATE           PIC X(08).
               10  BZ-D-TIME           PIC X(04).
               10  BZ-D-CODE           PIC X(02).
               10  BZ-D-LOCATION       PIC X(20).
               10  BZ-D-NOTE           PIC X(11).
      *    T - KONTROLNI ZBIR SERIJE
           05  BZ-TRAILER REDEFINES BZ-DATA.
               10  BZ-T-COUNT          PIC 9(06).
               10  FILLER              PIC X(53).
