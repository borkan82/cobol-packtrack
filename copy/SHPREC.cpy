      *================================================================*
      * SHPREC   - MATIČNI SLOG POŠILJKE (INDEKSNA DATOTEKA SHIPMAST)
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * DUŽINA   : 200 BAJTOVA
      * KLJUČ    : SM-TRACK-NO (PRIMARNI)
      *            SM-CARRIER  (ALTERNATIVNI, SA DUPLIKATIMA)
      *----------------------------------------------------------------*
      * JEDAN SLOG = JEDNA POŠILJKA, BEZ OBZIRA NA KURIRSKU SLUŽBU.
      * STATUS JE UVIJEK UNIVERZALNI KOD (VIDI PKTSTAT), A NE KOD
      * KURIRA - TAKO WEB SHOP VIDI ISTE STATUSE ZA SVE KURIRE.
      *================================================================*
       01  SHIP-RECORD.
           05  SM-TRACK-NO             PIC X(14).
           05  SM-CARRIER              PIC X(03).
      *    BROJ NARUDŽBE U SISTEMU KLIJENTA (WEB SHOPA)
           05  SM-ORDER-NO             PIC X(10).
           05  SM-CLIENT               PIC X(10).
           05  SM-RCPT-NAME            PIC X(30).
           05  SM-RCPT-CITY            PIC X(20).
      *    E-MAIL PRIMAOCA ZA OBAVJEŠTENJA
           05  SM-RCPT-CONTACT         PIC X(40).
      *    DATUM PREDAJE KURIRU I OBEĆANI DATUM ISPORUKE (SLA)
           05  SM-SHIP-DATE            PIC 9(08).
           05  SM-PROMISED-DATE        PIC 9(08).
      *    TRENUTNI STATUS I VRIJEME ZADNJEG DOGAĐAJA GGGGMMDDHHMM
           05  SM-STATUS               PIC X(02).
               88  SM-FINAL            VALUE 'DL' 'RT'.
               88  SM-DELIVERED        VALUE 'DL'.
               88  SM-RETURNED         VALUE 'RT'.
               88  SM-EXCEPTION        VALUE 'EX'.
           05  SM-STATUS-TS            PIC 9(12).
           05  SM-LOCATION             PIC X(20).
      *    BROJ NEUSPJELIH POKUŠAJA DOSTAVE
           05  SM-ATTEMPTS             PIC 9(01).
           05  SM-DELIVERED-TS         PIC 9(12).
           05  SM-EVENT-CNT            PIC 9(03).
           05  FILLER                  PIC X(07).
