       01  TRANSACTION-RECORD.
           05 TXN-COUNT             PIC 9(3).
           05 TXNS OCCURS 0 TO 100
                DEPENDING ON TXN-COUNT.
              10 TXN-ID             PIC X(12).
              10 TXN-DATE           PIC 9(8).
              10 TXN-AMOUNT         PIC S9(9)V99 COMP-3.
