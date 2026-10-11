      *================================================================*
      * PKTCONST - KONSTANTE I TABELA KURIRSKIH SLUŽBI
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      *----------------------------------------------------------------*
      * NOVI KURIR = NOVI RED U TABELI (SORTIRANO PO KODU, ZBOG
      * SEARCH ALL), NOVI FORMAT U PKTIMP I NOVI KODOVI U PKTSTAT.
      *================================================================*
       01  PKT-CONSTANTS.
           05  PC-COMPANY-1            PIC X(40)
               VALUE 'TAIPANSTUDIO - PACKTRACK'.
           05  PC-COMPANY-2            PIC X(40)
               VALUE 'UNIVERZALNO PRACENJE POSILJAKA'.
      *    POŠILJKA BEZ SKENIRANJA OVOLIKO DANA JE "ZAGLAVLJENA"
           05  PC-STUCK-DAYS           PIC 9(02)   VALUE 3.
      *    POSLIJE OVOLIKO NEUSPJELIH POKUŠAJA - POVRAT POŠILJAOCU
           05  PC-MAX-ATTEMPTS         PIC 9(01)   VALUE 3.

      *    KOD(3) + NAZIV(15) + ROK DOSTAVE U RADNIM DANIMA(2)
       01  PKT-CARRIER-DATA.
           05  FILLER  PIC X(20)  VALUE 'BRZBRZA POSTA     02'.
           05  FILLER  PIC X(20)  VALUE 'CITCITY KURIR     01'.
           05  FILLER  PIC X(20)  VALUE 'EUPEUROPAKET      03'.
       01  PKT-CARRIER-TABLE REDEFINES PKT-CARRIER-DATA.
           05  PC-CARRIER              OCCURS 3 TIMES
                                       ASCENDING KEY IS PC-CAR-CODE
                                       INDEXED BY CX.
               10  PC-CAR-CODE         PIC X(03).
               10  PC-CAR-NAME         PIC X(15).
               10  PC-CAR-DAYS         PIC 9(02).
