      *================================================================*
      * GETDATE  - DATUM OBRADE I FORMATIRANJE VREMENA
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * PREDUSLOV: COPY RUNDATE U WORKING-STORAGE.
      *----------------------------------------------------------------*
      * DATUM OBRADE IZ VARIJABLE PKT_RUN_DATE (GGGGMMDD), INAČE
      * SISTEMSKI DATUM - DA SE OBRADA MOŽE PONOVITI ZA BILO KOJI DAN.
      *================================================================*
       8900-GET-RUN-DATE.
           ACCEPT WS-ENV-DATE FROM ENVIRONMENT 'PKT_RUN_DATE'
           IF WS-ENV-DATE IS NUMERIC
               MOVE WS-ENV-DATE TO WS-RUN-DATE
           ELSE
               MOVE FUNCTION CURRENT-DATE (1:8) TO WS-RUN-DATE
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD (WS-RUN-DATE) NOT = ZERO
               DISPLAY '*** NEISPRAVAN DATUM OBRADE: ' WS-RUN-DATE
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF
           COMPUTE WS-RUN-INT = FUNCTION INTEGER-OF-DATE (WS-RUN-DATE)
           COMPUTE WS-TS-IN = WS-RUN-DATE * 10000
           PERFORM 8920-FORMAT-TS
           MOVE WS-DATE-TXT TO WS-RUN-DATE-TXT.

      *    WS-TS-IN (GGGGMMDDHHMM) -> WS-TS-OUT "DD.MM.GGGG HH:MM"
       8920-FORMAT-TS.
           MOVE SPACES TO WS-TS-OUT
           IF WS-TS-IN NOT = ZERO
               STRING WS-TS-DD '.' WS-TS-MM '.' WS-TS-YYYY ' '
                      WS-TS-HH ':' WS-TS-MI
                      DELIMITED BY SIZE INTO WS-TS-OUT
               END-STRING
           END-IF.
