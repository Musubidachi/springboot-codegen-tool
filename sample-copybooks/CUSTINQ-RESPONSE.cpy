      ******************************************************************
      * SAMPLE RESPONSE COPYBOOK FOR CUSTOMER INQUIRY
      * Program: CUSTINQ
      ******************************************************************
       01  CUSTINQ-RESPONSE.
           05  RESP-HEADER.
               10  TRANSACTION-ID          PIC X(8).
               10  RESPONSE-CODE           PIC X(2).
                   88  RESP-SUCCESS        VALUE '00'.
                   88  RESP-NOT-FOUND      VALUE '01'.
                   88  RESP-INVALID-REQ    VALUE '02'.
                   88  RESP-SYSTEM-ERROR   VALUE '99'.
               10  RESPONSE-MESSAGE        PIC X(50).
               10  RESPONSE-TIMESTAMP      PIC 9(14).
           05  RESP-CUSTOMER-DATA.
               10  CUSTOMER-ID             PIC 9(10).
               10  CUSTOMER-NAME           PIC X(30).
               10  CUSTOMER-STATUS         PIC X(1).
                   88  CUST-ACTIVE         VALUE 'A'.
                   88  CUST-INACTIVE       VALUE 'I'.
                   88  CUST-SUSPENDED      VALUE 'S'.
               10  CUSTOMER-TYPE           PIC X(2).
                   88  TYPE-INDIVIDUAL     VALUE 'IN'.
                   88  TYPE-BUSINESS       VALUE 'BU'.
                   88  TYPE-GOVERNMENT     VALUE 'GV'.
               10  DATE-OPENED.
                   15  OPEN-YEAR           PIC 9(4).
                   15  OPEN-MONTH          PIC 9(2).
                   15  OPEN-DAY            PIC 9(2).
               10  TOTAL-BALANCE           PIC S9(11)V99 COMP-3.
               10  CREDIT-LIMIT            PIC S9(9)V99 COMP-3.
               10  LAST-ACTIVITY-DATE.
                   15  LAST-ACT-YEAR       PIC 9(4).
                   15  LAST-ACT-MONTH      PIC 9(2).
                   15  LAST-ACT-DAY        PIC 9(2).
           05  RESP-ACCOUNT-SUMMARY.
               10  TOTAL-ACCOUNTS          PIC 9(3) COMP.
               10  ACCOUNT-LIST OCCURS 10 TIMES.
                   15  ACCOUNT-NUMBER      PIC X(12).
                   15  ACCOUNT-TYPE        PIC X(3).
                   15  ACCOUNT-STATUS      PIC X(1).
                   15  ACCOUNT-BALANCE     PIC S9(9)V99 COMP-3.
                   15  INTEREST-RATE       PIC 9(2)V9(4) COMP-3.
           05  RESP-CONTACT-INFO.
               10  PRIMARY-PHONE           PIC X(15).
               10  EMAIL-ADDRESS           PIC X(50).
               10  MAILING-ADDRESS.
                   15  STREET-LINE-1       PIC X(40).
                   15  STREET-LINE-2       PIC X(40).
                   15  CITY                PIC X(25).
                   15  STATE-CODE          PIC X(2).
                   15  POSTAL-CODE         PIC X(10).
                   15  COUNTRY-CODE        PIC X(3).
           05  RESP-PAGINATION.
               10  TOTAL-RECORDS           PIC 9(6) COMP.
               10  CURRENT-PAGE            PIC 9(4) COMP.
               10  TOTAL-PAGES             PIC 9(4) COMP.
               10  HAS-MORE-DATA           PIC X(1).
                   88  MORE-DATA-YES       VALUE 'Y'.
                   88  MORE-DATA-NO        VALUE 'N'.
           05  FILLER                      PIC X(50).
