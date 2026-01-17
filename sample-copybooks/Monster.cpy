       01  NIGHTMARE-STRUCT.
           05 HEADER.
              10 RECORD-TYPE        PIC X.
                 88 TYPE-A          VALUE 'A'.
                 88 TYPE-B          VALUE 'B'.
              10 RECORD-LENGTH      PIC 9(4) COMP.
           05 BODY REDEFINES HEADER.
              10 COUNTER            PIC 9(3).
              10 ITEMS OCCURS 1 TO 20 DEPENDING ON COUNTER.
                 15 ITEM-ID         PIC X(5).
                 15 ITEM-VALUE      PIC S9(7)V9(2) COMP-3.
