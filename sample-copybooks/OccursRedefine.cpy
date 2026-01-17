       01  MESSAGE-BLOCK.
           05 RAW-MESSAGE           PIC X(100).
           05 MESSAGE-TABLE REDEFINES RAW-MESSAGE.
              10 MESSAGE-LINE OCCURS 10 TIMES.
                 15 LINE-TEXT       PIC X(10).
