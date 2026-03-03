      *================================================================*
      * PROGRAM-ID: CUSTMGMT
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Customer management program demonstrating file I/O,
      *          record processing, error handling, and VSAM-style
      *          indexed file operations common in mainframe COBOL.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMGMT.
       AUTHOR. XMAINFRAME-TEST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO 'CUSTFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               ALTERNATE RECORD KEY IS CUST-NAME
                   WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO 'CUSTRPT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUST-ID            PIC 9(8).
           05 CUST-NAME          PIC X(40).
           05 CUST-ADDRESS.
              10 CUST-STREET     PIC X(30).
              10 CUST-CITY       PIC X(20).
              10 CUST-STATE      PIC X(2).
              10 CUST-ZIP        PIC 9(5).
           05 CUST-PHONE         PIC 9(10).
           05 CUST-BALANCE       PIC S9(7)V99 COMP-3.
           05 CUST-CREDIT-LIMIT  PIC S9(7)V99 COMP-3.
           05 CUST-STATUS        PIC X(1).
              88 CUST-ACTIVE     VALUE 'A'.
              88 CUST-INACTIVE   VALUE 'I'.
              88 CUST-SUSPENDED  VALUE 'S'.
           05 CUST-OPEN-DATE     PIC 9(8).
           05 CUST-LAST-ACTIVITY PIC 9(8).

       FD  REPORT-FILE.
       01  REPORT-RECORD         PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS        PIC X(2).
           88 WS-SUCCESS         VALUE '00'.
           88 WS-EOF             VALUE '10'.
           88 WS-DUP-KEY         VALUE '22'.
           88 WS-NOT-FOUND       VALUE '23'.

       01  WS-RPT-STATUS         PIC X(2).

       01  WS-SWITCHES.
           05 WS-EOF-SW          PIC X(1) VALUE 'N'.
              88 END-OF-FILE     VALUE 'Y'.
              88 NOT-END-OF-FILE VALUE 'N'.

       01  WS-COUNTERS.
           05 WS-READ-COUNT      PIC 9(6) VALUE ZEROS.
           05 WS-ACTIVE-COUNT    PIC 9(6) VALUE ZEROS.
           05 WS-INACTIVE-COUNT  PIC 9(6) VALUE ZEROS.
           05 WS-SUSPENDED-COUNT PIC 9(6) VALUE ZEROS.
           05 WS-HIGH-BAL-COUNT  PIC 9(6) VALUE ZEROS.
           05 WS-ERROR-COUNT     PIC 9(6) VALUE ZEROS.

       01  WS-TOTALS.
           05 WS-TOTAL-BALANCE   PIC S9(11)V99 VALUE ZEROS.
           05 WS-AVG-BALANCE     PIC S9(7)V99  VALUE ZEROS.
           05 WS-MAX-BALANCE     PIC S9(7)V99  VALUE ZEROS.
           05 WS-MIN-BALANCE     PIC S9(7)V99  VALUE +9999999.99.

       01  WS-CREDIT-THRESHOLD   PIC S9(7)V99 VALUE +5000.00.

       01  WS-REPORT-HEADER.
           05 FILLER             PIC X(20) VALUE
                                 'CUSTOMER STATUS RPT '.
           05 WS-RPT-DATE        PIC X(10).
           05 FILLER             PIC X(102) VALUE SPACES.

       01  WS-DETAIL-LINE.
           05 WS-DET-ID          PIC 9(8).
           05 FILLER             PIC X(2) VALUE SPACES.
           05 WS-DET-NAME        PIC X(40).
           05 FILLER             PIC X(2) VALUE SPACES.
           05 WS-DET-STATUS      PIC X(10).
           05 FILLER             PIC X(2) VALUE SPACES.
           05 WS-DET-BALANCE     PIC Z,ZZZ,ZZ9.99-.
           05 FILLER             PIC X(2) VALUE SPACES.
           05 WS-DET-CREDIT      PIC Z,ZZZ,ZZ9.99-.
           05 FILLER             PIC X(2) VALUE SPACES.
           05 WS-DET-FLAG        PIC X(15).

       01  WS-CURRENT-DATE-DATA.
           05 WS-CURR-DATE.
              10 WS-CURR-YEAR    PIC 9(4).
              10 WS-CURR-MONTH   PIC 9(2).
              10 WS-CURR-DAY     PIC 9(2).
           05 WS-CURR-TIME.
              10 WS-CURR-HOUR    PIC 9(2).
              10 WS-CURR-MIN     PIC 9(2).
              10 WS-CURR-SEC     PIC 9(2).
              10 WS-CURR-HUND    PIC 9(2).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CUSTOMERS
           PERFORM 3000-GENERATE-SUMMARY
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           STRING WS-CURR-YEAR DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-CURR-MONTH DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-CURR-DAY DELIMITED BY SIZE
                  INTO WS-RPT-DATE
           END-STRING
           OPEN INPUT CUSTOMER-FILE
           IF NOT WS-SUCCESS
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' WS-FILE-STATUS
               PERFORM 9999-ABORT
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF NOT WS-SUCCESS
               DISPLAY 'ERROR OPENING REPORT FILE: ' WS-RPT-STATUS
               PERFORM 9999-ABORT
           END-IF
           WRITE REPORT-RECORD FROM WS-REPORT-HEADER.

       2000-PROCESS-CUSTOMERS.
           READ CUSTOMER-FILE NEXT RECORD
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-READ-COUNT
               PERFORM 2100-EVALUATE-CUSTOMER
               PERFORM 2200-UPDATE-STATISTICS
               PERFORM 2300-WRITE-DETAIL
               READ CUSTOMER-FILE NEXT RECORD
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

       2100-EVALUATE-CUSTOMER.
           EVALUATE TRUE
               WHEN CUST-ACTIVE
                   ADD 1 TO WS-ACTIVE-COUNT
                   MOVE 'ACTIVE' TO WS-DET-STATUS
               WHEN CUST-INACTIVE
                   ADD 1 TO WS-INACTIVE-COUNT
                   MOVE 'INACTIVE' TO WS-DET-STATUS
               WHEN CUST-SUSPENDED
                   ADD 1 TO WS-SUSPENDED-COUNT
                   MOVE 'SUSPENDED' TO WS-DET-STATUS
               WHEN OTHER
                   ADD 1 TO WS-ERROR-COUNT
                   MOVE 'UNKNOWN' TO WS-DET-STATUS
           END-EVALUATE.

       2200-UPDATE-STATISTICS.
           ADD CUST-BALANCE TO WS-TOTAL-BALANCE
           IF CUST-BALANCE > WS-MAX-BALANCE
               MOVE CUST-BALANCE TO WS-MAX-BALANCE
           END-IF
           IF CUST-BALANCE < WS-MIN-BALANCE
               MOVE CUST-BALANCE TO WS-MIN-BALANCE
           END-IF
           IF CUST-BALANCE > WS-CREDIT-THRESHOLD
               ADD 1 TO WS-HIGH-BAL-COUNT
           END-IF.

       2300-WRITE-DETAIL.
           MOVE CUST-ID      TO WS-DET-ID
           MOVE CUST-NAME    TO WS-DET-NAME
           MOVE CUST-BALANCE TO WS-DET-BALANCE
           MOVE CUST-CREDIT-LIMIT TO WS-DET-CREDIT
           IF CUST-BALANCE > CUST-CREDIT-LIMIT
               MOVE 'OVER LIMIT' TO WS-DET-FLAG
           ELSE IF CUST-BALANCE > WS-CREDIT-THRESHOLD
               MOVE 'HIGH BALANCE' TO WS-DET-FLAG
           ELSE
               MOVE SPACES TO WS-DET-FLAG
           END-IF
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE.

       3000-GENERATE-SUMMARY.
           IF WS-READ-COUNT > ZEROS
               COMPUTE WS-AVG-BALANCE =
                   WS-TOTAL-BALANCE / WS-READ-COUNT
           END-IF
           DISPLAY '========================================='
           DISPLAY 'CUSTOMER PROCESSING SUMMARY'
           DISPLAY '========================================='
           DISPLAY 'Total Records Read:    ' WS-READ-COUNT
           DISPLAY 'Active Customers:      ' WS-ACTIVE-COUNT
           DISPLAY 'Inactive Customers:    ' WS-INACTIVE-COUNT
           DISPLAY 'Suspended Customers:   ' WS-SUSPENDED-COUNT
           DISPLAY 'High Balance Count:    ' WS-HIGH-BAL-COUNT
           DISPLAY 'Errors:                ' WS-ERROR-COUNT
           DISPLAY '-----------------------------------------'
           DISPLAY 'Total Balance:    ' WS-TOTAL-BALANCE
           DISPLAY 'Average Balance:  ' WS-AVG-BALANCE
           DISPLAY 'Maximum Balance:  ' WS-MAX-BALANCE
           DISPLAY 'Minimum Balance:  ' WS-MIN-BALANCE
           DISPLAY '========================================='.

       9000-CLEANUP.
           CLOSE CUSTOMER-FILE
           CLOSE REPORT-FILE.

       9999-ABORT.
           DISPLAY 'PROGRAM ABNORMALLY TERMINATED'
           CLOSE CUSTOMER-FILE
           CLOSE REPORT-FILE
           STOP RUN.
