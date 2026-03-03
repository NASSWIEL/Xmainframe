      *================================================================*
      * PROGRAM-ID: PAYROLL
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Payroll calculation program demonstrating complex
      *          business logic, PERFORM VARYING, tables/arrays,
      *          nested COPY-like structures, and arithmetic ops
      *          commonly found in mainframe batch processing.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. XMAINFRAME-TEST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO 'EMPFILE'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EMP-STATUS.

           SELECT PAYROLL-FILE
               ASSIGN TO 'PAYFILE'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PAY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05 EMP-ID              PIC 9(6).
           05 EMP-NAME            PIC X(30).
           05 EMP-DEPARTMENT      PIC X(4).
           05 EMP-PAY-TYPE        PIC X(1).
              88 EMP-HOURLY       VALUE 'H'.
              88 EMP-SALARIED     VALUE 'S'.
              88 EMP-COMMISSION   VALUE 'C'.
           05 EMP-HOURLY-RATE     PIC S9(3)V99 COMP-3.
           05 EMP-ANNUAL-SALARY   PIC S9(7)V99 COMP-3.
           05 EMP-COMMISSION-PCT  PIC S9V99     COMP-3.
           05 EMP-SALES-AMOUNT    PIC S9(7)V99 COMP-3.
           05 EMP-HOURS-WORKED    PIC S9(3)V99 COMP-3.
           05 EMP-DEDUCTIONS.
              10 EMP-HEALTH-INS   PIC S9(5)V99 COMP-3.
              10 EMP-401K-PCT     PIC S9V99     COMP-3.
              10 EMP-OTHER-DEDUCT PIC S9(5)V99 COMP-3.

       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD         PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-EMP-STATUS          PIC X(2).
       01  WS-PAY-STATUS          PIC X(2).

       01  WS-EOF-FLAG            PIC X(1) VALUE 'N'.
           88 WS-END-OF-FILE      VALUE 'Y'.
           88 WS-NOT-EOF          VALUE 'N'.

      *--- Tax brackets table (Federal) ---
       01  WS-TAX-TABLE.
           05 WS-TAX-BRACKETS.
              10 FILLER PIC X(20) VALUE '0000000000001000010'.
              10 FILLER PIC X(20) VALUE '0000100000004100012'.
              10 FILLER PIC X(20) VALUE '0000410000008950022'.
              10 FILLER PIC X(20) VALUE '0000895000017150024'.
              10 FILLER PIC X(20) VALUE '0001715000032900032'.
              10 FILLER PIC X(20) VALUE '0003290000041850035'.
              10 FILLER PIC X(20) VALUE '0004185000099999037'.
           05 WS-TAX-BRACKET-TABLE REDEFINES WS-TAX-BRACKETS.
              10 WS-BRACKET OCCURS 7 TIMES.
                 15 WS-BRACKET-LOW    PIC 9(9)V99.
                 15 WS-BRACKET-HIGH   PIC 9(9)V99.
                 15 WS-BRACKET-RATE   PIC 9V99.

      *--- Calculation fields ---
       01  WS-CALC-FIELDS.
           05 WS-GROSS-PAY        PIC S9(7)V99 VALUE ZEROS.
           05 WS-OVERTIME-PAY     PIC S9(7)V99 VALUE ZEROS.
           05 WS-REGULAR-PAY      PIC S9(7)V99 VALUE ZEROS.
           05 WS-FEDERAL-TAX      PIC S9(7)V99 VALUE ZEROS.
           05 WS-STATE-TAX        PIC S9(7)V99 VALUE ZEROS.
           05 WS-FICA-TAX         PIC S9(7)V99 VALUE ZEROS.
           05 WS-MEDICARE-TAX     PIC S9(7)V99 VALUE ZEROS.
           05 WS-TOTAL-DEDUCTIONS PIC S9(7)V99 VALUE ZEROS.
           05 WS-NET-PAY          PIC S9(7)V99 VALUE ZEROS.
           05 WS-401K-AMOUNT      PIC S9(7)V99 VALUE ZEROS.
           05 WS-TAXABLE-INCOME   PIC S9(9)V99 VALUE ZEROS.

       01  WS-CONSTANTS.
           05 WS-OVERTIME-RATE    PIC S9V99 VALUE 1.50.
           05 WS-STANDARD-HOURS   PIC S9(3)V99 VALUE 40.00.
           05 WS-FICA-RATE        PIC SV9999 VALUE .0620.
           05 WS-MEDICARE-RATE    PIC SV9999 VALUE .0145.
           05 WS-STATE-TAX-RATE   PIC SV9999 VALUE .0500.
           05 WS-FICA-WAGE-BASE   PIC S9(9)V99 VALUE 160200.00.
           05 WS-PAY-PERIODS      PIC 9(2)   VALUE 26.

      *--- Accumulators ---
       01  WS-TOTALS.
           05 WS-TOTAL-GROSS      PIC S9(9)V99 VALUE ZEROS.
           05 WS-TOTAL-NET        PIC S9(9)V99 VALUE ZEROS.
           05 WS-TOTAL-FED-TAX    PIC S9(9)V99 VALUE ZEROS.
           05 WS-TOTAL-STATE-TAX  PIC S9(9)V99 VALUE ZEROS.
           05 WS-EMP-COUNT        PIC 9(6)     VALUE ZEROS.

      *--- Department summary table ---
       01  WS-DEPT-TABLE.
           05 WS-DEPT-ENTRY OCCURS 20 TIMES.
              10 WS-DEPT-CODE     PIC X(4).
              10 WS-DEPT-EMP-CNT  PIC 9(4)     VALUE ZEROS.
              10 WS-DEPT-TOTAL    PIC S9(9)V99 VALUE ZEROS.
       01  WS-DEPT-COUNT          PIC 9(2) VALUE ZEROS.
       01  WS-DEPT-IDX            PIC 9(2).
       01  WS-DEPT-FOUND          PIC X(1).

      *--- Output detail record ---
       01  WS-PAY-DETAIL.
           05 WS-PD-EMP-ID        PIC 9(6).
           05 WS-PD-EMP-NAME      PIC X(30).
           05 WS-PD-DEPT          PIC X(4).
           05 WS-PD-GROSS         PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-FED-TAX       PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-STATE-TAX     PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-FICA          PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-MEDICARE      PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-DEDUCTIONS    PIC Z,ZZZ,ZZ9.99.
           05 WS-PD-NET           PIC Z,ZZZ,ZZ9.99.

       01  WS-SUB                 PIC 9(2).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-EMPLOYEES
               UNTIL WS-END-OF-FILE
           PERFORM 3000-PRINT-DEPARTMENT-SUMMARY
           PERFORM 4000-PRINT-GRAND-TOTALS
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-FILE
           READ EMPLOYEE-FILE
               AT END SET WS-END-OF-FILE TO TRUE
           END-READ.

       2000-PROCESS-EMPLOYEES.
           INITIALIZE WS-CALC-FIELDS
           ADD 1 TO WS-EMP-COUNT
           PERFORM 2100-CALCULATE-GROSS-PAY
           PERFORM 2200-CALCULATE-TAXES
           PERFORM 2300-CALCULATE-DEDUCTIONS
           PERFORM 2400-CALCULATE-NET-PAY
           PERFORM 2500-UPDATE-DEPT-TOTALS
           PERFORM 2600-WRITE-PAY-DETAIL
           READ EMPLOYEE-FILE
               AT END SET WS-END-OF-FILE TO TRUE
           END-READ.

       2100-CALCULATE-GROSS-PAY.
           EVALUATE TRUE
               WHEN EMP-HOURLY
                   IF EMP-HOURS-WORKED > WS-STANDARD-HOURS
                       COMPUTE WS-REGULAR-PAY =
                           WS-STANDARD-HOURS * EMP-HOURLY-RATE
                       COMPUTE WS-OVERTIME-PAY =
                           (EMP-HOURS-WORKED - WS-STANDARD-HOURS)
                           * EMP-HOURLY-RATE * WS-OVERTIME-RATE
                       COMPUTE WS-GROSS-PAY =
                           WS-REGULAR-PAY + WS-OVERTIME-PAY
                   ELSE
                       COMPUTE WS-GROSS-PAY =
                           EMP-HOURS-WORKED * EMP-HOURLY-RATE
                   END-IF
               WHEN EMP-SALARIED
                   COMPUTE WS-GROSS-PAY =
                       EMP-ANNUAL-SALARY / WS-PAY-PERIODS
               WHEN EMP-COMMISSION
                   COMPUTE WS-GROSS-PAY =
                       (EMP-ANNUAL-SALARY / WS-PAY-PERIODS)
                       + (EMP-SALES-AMOUNT * EMP-COMMISSION-PCT)
               WHEN OTHER
                   DISPLAY 'UNKNOWN PAY TYPE FOR EMP: ' EMP-ID
                   MOVE ZEROS TO WS-GROSS-PAY
           END-EVALUATE.

       2200-CALCULATE-TAXES.
      *--- Annualize for bracket calculation ---
           COMPUTE WS-TAXABLE-INCOME =
               WS-GROSS-PAY * WS-PAY-PERIODS
      *--- Federal tax using bracket table ---
           MOVE ZEROS TO WS-FEDERAL-TAX
           PERFORM VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > 7
               IF WS-TAXABLE-INCOME > WS-BRACKET-LOW(WS-SUB)
                   IF WS-TAXABLE-INCOME >= WS-BRACKET-HIGH(WS-SUB)
                       COMPUTE WS-FEDERAL-TAX =
                           WS-FEDERAL-TAX +
                           (WS-BRACKET-HIGH(WS-SUB) -
                            WS-BRACKET-LOW(WS-SUB))
                           * WS-BRACKET-RATE(WS-SUB)
                   ELSE
                       COMPUTE WS-FEDERAL-TAX =
                           WS-FEDERAL-TAX +
                           (WS-TAXABLE-INCOME -
                            WS-BRACKET-LOW(WS-SUB))
                           * WS-BRACKET-RATE(WS-SUB)
                   END-IF
               END-IF
           END-PERFORM
      *--- Convert annual tax to per-period ---
           COMPUTE WS-FEDERAL-TAX =
               WS-FEDERAL-TAX / WS-PAY-PERIODS
      *--- State tax (flat rate) ---
           COMPUTE WS-STATE-TAX =
               WS-GROSS-PAY * WS-STATE-TAX-RATE
      *--- FICA tax ---
           IF WS-TAXABLE-INCOME <= WS-FICA-WAGE-BASE
               COMPUTE WS-FICA-TAX =
                   WS-GROSS-PAY * WS-FICA-RATE
           ELSE
               MOVE ZEROS TO WS-FICA-TAX
           END-IF
      *--- Medicare tax ---
           COMPUTE WS-MEDICARE-TAX =
               WS-GROSS-PAY * WS-MEDICARE-RATE.

       2300-CALCULATE-DEDUCTIONS.
           COMPUTE WS-401K-AMOUNT =
               WS-GROSS-PAY * EMP-401K-PCT
           COMPUTE WS-TOTAL-DEDUCTIONS =
               EMP-HEALTH-INS + WS-401K-AMOUNT + EMP-OTHER-DEDUCT
               + WS-FEDERAL-TAX + WS-STATE-TAX
               + WS-FICA-TAX + WS-MEDICARE-TAX.

       2400-CALCULATE-NET-PAY.
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TOTAL-DEDUCTIONS
           ADD WS-GROSS-PAY    TO WS-TOTAL-GROSS
           ADD WS-NET-PAY      TO WS-TOTAL-NET
           ADD WS-FEDERAL-TAX  TO WS-TOTAL-FED-TAX
           ADD WS-STATE-TAX    TO WS-TOTAL-STATE-TAX.

       2500-UPDATE-DEPT-TOTALS.
           MOVE 'N' TO WS-DEPT-FOUND
           PERFORM VARYING WS-DEPT-IDX FROM 1 BY 1
               UNTIL WS-DEPT-IDX > WS-DEPT-COUNT
               OR WS-DEPT-FOUND = 'Y'
               IF WS-DEPT-CODE(WS-DEPT-IDX) = EMP-DEPARTMENT
                   MOVE 'Y' TO WS-DEPT-FOUND
                   ADD 1 TO WS-DEPT-EMP-CNT(WS-DEPT-IDX)
                   ADD WS-GROSS-PAY
                       TO WS-DEPT-TOTAL(WS-DEPT-IDX)
               END-IF
           END-PERFORM
           IF WS-DEPT-FOUND = 'N'
               ADD 1 TO WS-DEPT-COUNT
               MOVE EMP-DEPARTMENT TO WS-DEPT-CODE(WS-DEPT-COUNT)
               MOVE 1 TO WS-DEPT-EMP-CNT(WS-DEPT-COUNT)
               MOVE WS-GROSS-PAY TO WS-DEPT-TOTAL(WS-DEPT-COUNT)
           END-IF.

       2600-WRITE-PAY-DETAIL.
           MOVE EMP-ID         TO WS-PD-EMP-ID
           MOVE EMP-NAME       TO WS-PD-EMP-NAME
           MOVE EMP-DEPARTMENT TO WS-PD-DEPT
           MOVE WS-GROSS-PAY   TO WS-PD-GROSS
           MOVE WS-FEDERAL-TAX TO WS-PD-FED-TAX
           MOVE WS-STATE-TAX   TO WS-PD-STATE-TAX
           MOVE WS-FICA-TAX    TO WS-PD-FICA
           MOVE WS-MEDICARE-TAX TO WS-PD-MEDICARE
           MOVE WS-TOTAL-DEDUCTIONS TO WS-PD-DEDUCTIONS
           MOVE WS-NET-PAY     TO WS-PD-NET
           WRITE PAYROLL-RECORD FROM WS-PAY-DETAIL.

       3000-PRINT-DEPARTMENT-SUMMARY.
           DISPLAY '===== DEPARTMENT SUMMARY ====='
           PERFORM VARYING WS-DEPT-IDX FROM 1 BY 1
               UNTIL WS-DEPT-IDX > WS-DEPT-COUNT
               DISPLAY 'Dept: ' WS-DEPT-CODE(WS-DEPT-IDX)
                       ' Employees: ' WS-DEPT-EMP-CNT(WS-DEPT-IDX)
                       ' Total Gross: ' WS-DEPT-TOTAL(WS-DEPT-IDX)
           END-PERFORM.

       4000-PRINT-GRAND-TOTALS.
           DISPLAY '===== GRAND TOTALS ====='
           DISPLAY 'Total Employees: ' WS-EMP-COUNT
           DISPLAY 'Total Gross Pay: ' WS-TOTAL-GROSS
           DISPLAY 'Total Net Pay:   ' WS-TOTAL-NET
           DISPLAY 'Total Fed Tax:   ' WS-TOTAL-FED-TAX
           DISPLAY 'Total State Tax: ' WS-TOTAL-STATE-TAX.

       9000-CLEANUP.
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-FILE.
