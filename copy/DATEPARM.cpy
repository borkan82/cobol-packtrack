      *================================================================*
      * DATEPARM - PARAMETRI ZA POTPROGRAM PKTDATE
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      *----------------------------------------------------------------*
      * ISTI COPYBOOK KORISTI POZIVAOC (WORKING-STORAGE) I PKTDATE
      * (LINKAGE SECTION) - TAKO SE RASPORED NE MOŽE RAZIĆI.
      *
      *   LP-FUNC 'CNV' - DATUM + VRIJEME U FORMATU LP-FORMAT
      *                   -> LP-TS-OUT GGGGMMDDHHMM (SA PROVJEROM)
      *           'ADD' - LP-DATE-IN (GGGGMMDD) + LP-DAYS RADNIH DANA
      *                   -> LP-DATE-OUT (PRESKAČE SUBOTU I NEDJELJU)
      *   LP-FORMAT 'ISO' = GGGG-MM-DD, 'DMY' = DD.MM.GGGG,
      *             'NUM' = GGGGMMDD
      *   LP-RC   0 = OK, 1 = LOŠ DATUM, 2 = LOŠE VRIJEME,
      *           9 = NEPOZNATA FUNKCIJA ILI FORMAT
      *================================================================*
       01  DATE-PARM.
           05  LP-FUNC                 PIC X(03).
           05  LP-FORMAT               PIC X(03).
           05  LP-DATE-IN              PIC X(10).
           05  LP-TIME-IN              PIC X(05).
           05  LP-DAYS                 PIC 9(03).
           05  LP-DATE-OUT             PIC 9(08).
           05  LP-TS-OUT               PIC 9(12).
           05  LP-RC                   PIC 9(01).
