       01  CUSTOMER-RECORD.
           05 CUSTOMER-ID           PIC 9(9).
           05 CUSTOMER-DATA.
              10 ADDRESS            PIC X(50).
              10 CONTACT-INFO REDEFINES ADDRESS.
                 15 PHONE-NUMBER    PIC X(10).
                 15 EMAIL           PIC X(40).
           05 ORDERS OCCURS 5 TIMES.
              10 ORDER-ID           PIC 9(7).
              10 ORDER-AMOUNT       PIC S9(7)V99 COMP-3.
