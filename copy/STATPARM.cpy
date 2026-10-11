      *================================================================*
      * STATPARM - PARAMETRI ZA POTPROGRAM PKTSTAT
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      *----------------------------------------------------------------*
      *   SP-FUNC 'M' - KURIR + KOD KURIRA -> UNIVERZALNI STATUS
      *           'T' - UNIVERZALNI STATUS -> TEKST STATUSA
      *   OBJE FUNKCIJE VRAĆAJU I SP-TEXT I SP-FINAL.
      *   SP-RC   0 = OK, 1 = NEPOZNAT KOD, 9 = NEPOZNATA FUNKCIJA
      *================================================================*
       01  STAT-PARM.
           05  SP-FUNC                 PIC X(01).
           05  SP-CARRIER              PIC X(03).
           05  SP-CODE                 PIC X(08).
           05  SP-STATUS               PIC X(02).
           05  SP-TEXT                 PIC X(25).
           05  SP-FINAL                PIC X(01).
           05  SP-RC                   PIC 9(01).
