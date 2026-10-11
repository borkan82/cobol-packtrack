      *================================================================*
      * SHPSEL   - SELECT ZA MATIČNU DATOTEKU POŠILJAKA
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      *----------------------------------------------------------------*
      * DEFINICIJA KLJUČEVA MORA BITI ISTA U SVIM PROGRAMIMA, PA JE
      * SELECT IZDVOJEN U COPYBOOK (KAO DEFINICIJA VSAM KLASTERA).
      *================================================================*
           SELECT SHIP-MASTER  ASSIGN TO 'SHIPMAST'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS SM-TRACK-NO
                  ALTERNATE RECORD KEY IS SM-CARRIER
                                       WITH DUPLICATES
                  FILE STATUS  IS WS-FS-SHIP.
