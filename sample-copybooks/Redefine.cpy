       01  GENERIC-DATE.
           05 RAW-DATE              PIC X(8).
           05 YYYYMMDD REDEFINES RAW-DATE.
              10 YYYY               PIC 9(4).
              10 MM                 PIC 9(2).
              10 DD                 PIC 9(2).
           05 MMDDYYYY REDEFINES RAW-DATE.
              10 MM2                PIC 9(2).
              10 DD2                PIC 9(2).
              10 YYYY2              PIC 9(4).
