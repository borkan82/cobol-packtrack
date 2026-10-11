      *================================================================*
      * RUNDATE  - DATUM OBRADE I FORMATIRANJE VREMENA
      * SISTEM   : PACKTRACK - TAIPANSTUDIO
      * KORISTI SE ZAJEDNO SA PARAGRAFIMA IZ GETDATE.
      *================================================================*
       01  WS-RUN-DATE-AREA.
           05  WS-ENV-DATE             PIC X(08)   VALUE SPACES.
           05  WS-RUN-DATE             PIC 9(08).
           05  WS-RUN-INT              PIC S9(09)  COMP.
           05  WS-RUN-DATE-TXT         PIC X(10).
      *    GGGGMMDDHHMM -> DD.MM.GGGG HH:MM (ILI SAMO DATUM)
       01  WS-TS-AREA.
           05  WS-TS-IN                PIC 9(12).
           05  WS-TS-IN-R REDEFINES WS-TS-IN.
               10  WS-TS-YYYY          PIC X(04).
               10  WS-TS-MM            PIC X(02).
               10  WS-TS-DD            PIC X(02).
               10  WS-TS-HH            PIC X(02).
               10  WS-TS-MI            PIC X(02).
           05  WS-TS-OUT               PIC X(16).
           05  WS-DATE-OUT REDEFINES WS-TS-OUT.
               10  WS-DATE-TXT         PIC X(10).
               10  FILLER              PIC X(06).
