      ******************************************************************
      * SAMPLE REQUEST COPYBOOK FOR CUSTOMER INQUIRY
      * Program: CUSTINQ
      ******************************************************************
       01  CUSTINQ-REQUEST.
           05  CUST-HEADER.
               10  TRANSACTION-ID          PIC X(8).
               10  REQUEST-DATE.
                   15  REQUEST-YEAR        PIC 9(4).
                   15  REQUEST-MONTH       PIC 9(2).
                   15  REQUEST-DAY         PIC 9(2).
               10  REQUEST-TIME            PIC 9(6).
               10  TERMINAL-ID             PIC X(4).
           05  CUST-SEARCH-CRITERIA.
               10  CUSTOMER-ID             PIC 9(10).
               10  CUSTOMER-NAME           PIC X(30).
               10  SEARCH-TYPE             PIC X(1).
                   88  SEARCH-BY-ID        VALUE 'I'.
                   88  SEARCH-BY-NAME      VALUE 'N'.
                   88  SEARCH-BY-BOTH      VALUE 'B'.
               10  INCLUDE-HISTORY         PIC X(1).
                   88  INCLUDE-HIST-YES    VALUE 'Y'.
                   88  INCLUDE-HIST-NO     VALUE 'N'.
           05  CUST-FILTER-OPTIONS.
               10  STATUS-FILTER           PIC X(1).
                   88  STATUS-ACTIVE       VALUE 'A'.
                   88  STATUS-INACTIVE     VALUE 'I'.
                   88  STATUS-ALL          VALUE '*'.
               10  MIN-BALANCE             PIC S9(9)V99 COMP-3.
               10  MAX-BALANCE             PIC S9(9)V99 COMP-3.
               10  ACCOUNT-TYPE-COUNT      PIC 9(2) COMP.
               10  ACCOUNT-TYPES OCCURS 5 TIMES.
                   15  ACCT-TYPE-CODE      PIC X(3).
           05  CUST-PAGINATION.
               10  PAGE-NUMBER             PIC 9(4) COMP.
               10  PAGE-SIZE               PIC 9(4) COMP.
               10  SORT-FIELD              PIC X(10).
               10  SORT-ORDER              PIC X(1).
                   88  SORT-ASCENDING      VALUE 'A'.
                   88  SORT-DESCENDING     VALUE 'D'.
           05  FILLER                      PIC X(20).
