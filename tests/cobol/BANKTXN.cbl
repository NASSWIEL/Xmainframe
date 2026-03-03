      *================================================================*
      * PROGRAM-ID: BANKTXN
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Banking transaction processor demonstrating
      *          CICS-style transaction handling, DB2-like SQL
      *          embedded operations, COPY book usage patterns,
      *          and error handling typical in financial mainframes.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKTXN.
       AUTHOR. XMAINFRAME-TEST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Transaction request area ---
       01  WS-TRANSACTION-REQUEST.
           05 WS-TXN-TYPE         PIC X(3).
              88 TXN-DEPOSIT      VALUE 'DEP'.
              88 TXN-WITHDRAW     VALUE 'WDR'.
              88 TXN-TRANSFER     VALUE 'TRF'.
              88 TXN-INQUIRY      VALUE 'INQ'.
              88 TXN-CLOSE-ACCT   VALUE 'CLS'.
           05 WS-TXN-ACCOUNT-FROM PIC 9(10).
           05 WS-TXN-ACCOUNT-TO   PIC 9(10).
           05 WS-TXN-AMOUNT       PIC S9(11)V99 COMP-3.
           05 WS-TXN-TIMESTAMP    PIC X(26).
           05 WS-TXN-TELLER-ID    PIC X(8).
           05 WS-TXN-BRANCH       PIC 9(4).

      *--- Account record area ---
       01  WS-ACCOUNT-RECORD.
           05 WS-ACCT-NUMBER      PIC 9(10).
           05 WS-ACCT-NAME        PIC X(40).
           05 WS-ACCT-TYPE        PIC X(2).
              88 ACCT-CHECKING    VALUE 'CK'.
              88 ACCT-SAVINGS     VALUE 'SV'.
              88 ACCT-MONEY-MKT   VALUE 'MM'.
           05 WS-ACCT-BALANCE     PIC S9(11)V99 COMP-3.
           05 WS-ACCT-AVAIL-BAL   PIC S9(11)V99 COMP-3.
           05 WS-ACCT-STATUS      PIC X(1).
              88 ACCT-OPEN        VALUE 'O'.
              88 ACCT-CLOSED      VALUE 'C'.
              88 ACCT-FROZEN      VALUE 'F'.
           05 WS-ACCT-OPEN-DATE   PIC 9(8).
           05 WS-ACCT-LAST-TXN    PIC 9(8).
           05 WS-ACCT-OD-LIMIT    PIC S9(7)V99 COMP-3.
           05 WS-ACCT-INTEREST-RT PIC SV9(4)   COMP-3.
           05 WS-ACCT-DAILY-LIMIT PIC S9(7)V99 COMP-3.
           05 WS-ACCT-DAILY-USED  PIC S9(7)V99 COMP-3.

      *--- Response area ---
       01  WS-RESPONSE.
           05 WS-RESP-CODE        PIC 9(4).
              88 RESP-SUCCESS     VALUE 0000.
              88 RESP-INSUF-FUNDS VALUE 1001.
              88 RESP-ACCT-FROZEN VALUE 1002.
              88 RESP-ACCT-CLOSED VALUE 1003.
              88 RESP-DAILY-LIMIT VALUE 1004.
              88 RESP-INVALID-TXN VALUE 2001.
              88 RESP-ACCT-NOT-FND VALUE 2002.
              88 RESP-DB-ERROR    VALUE 9001.
              88 RESP-SYSTEM-ERR  VALUE 9999.
           05 WS-RESP-MESSAGE     PIC X(80).
           05 WS-RESP-NEW-BALANCE PIC S9(11)V99 COMP-3.

      *--- Audit trail record ---
       01  WS-AUDIT-RECORD.
           05 WS-AUD-TIMESTAMP    PIC X(26).
           05 WS-AUD-TXN-TYPE     PIC X(3).
           05 WS-AUD-ACCT-FROM    PIC 9(10).
           05 WS-AUD-ACCT-TO      PIC 9(10).
           05 WS-AUD-AMOUNT       PIC S9(11)V99 COMP-3.
           05 WS-AUD-RESULT       PIC 9(4).
           05 WS-AUD-TELLER       PIC X(8).
           05 WS-AUD-BRANCH       PIC 9(4).
           05 WS-AUD-BEFORE-BAL   PIC S9(11)V99 COMP-3.
           05 WS-AUD-AFTER-BAL    PIC S9(11)V99 COMP-3.

      *--- Working fields ---
       01  WS-WORK-FIELDS.
           05 WS-BEFORE-BALANCE   PIC S9(11)V99 VALUE ZEROS.
           05 WS-AFTER-BALANCE    PIC S9(11)V99 VALUE ZEROS.
           05 WS-TRANSFER-AMT     PIC S9(11)V99 VALUE ZEROS.
           05 WS-SQLCODE          PIC S9(4)     COMP VALUE ZEROS.
           05 WS-DB-OPERATION     PIC X(10)     VALUE SPACES.

       01  WS-DAILY-TXN-COUNTERS.
           05 WS-DEPOSIT-COUNT    PIC 9(6) VALUE ZEROS.
           05 WS-WITHDRAW-COUNT   PIC 9(6) VALUE ZEROS.
           05 WS-TRANSFER-COUNT   PIC 9(6) VALUE ZEROS.
           05 WS-INQUIRY-COUNT    PIC 9(6) VALUE ZEROS.
           05 WS-FAILED-COUNT     PIC 9(6) VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-PROCESS-TRANSACTION
           GOBACK.

       1000-PROCESS-TRANSACTION.
           PERFORM 1100-VALIDATE-REQUEST
           IF RESP-SUCCESS
               EVALUATE TRUE
                   WHEN TXN-DEPOSIT
                       PERFORM 2000-PROCESS-DEPOSIT
                   WHEN TXN-WITHDRAW
                       PERFORM 3000-PROCESS-WITHDRAWAL
                   WHEN TXN-TRANSFER
                       PERFORM 4000-PROCESS-TRANSFER
                   WHEN TXN-INQUIRY
                       PERFORM 5000-PROCESS-INQUIRY
                   WHEN TXN-CLOSE-ACCT
                       PERFORM 6000-PROCESS-CLOSE-ACCOUNT
               END-EVALUATE
           END-IF
           PERFORM 8000-WRITE-AUDIT-TRAIL.

       1100-VALIDATE-REQUEST.
           SET RESP-SUCCESS TO TRUE
           MOVE SPACES TO WS-RESP-MESSAGE
           IF WS-TXN-TYPE NOT = 'DEP' AND 'WDR'
                          AND 'TRF' AND 'INQ' AND 'CLS'
               SET RESP-INVALID-TXN TO TRUE
               MOVE 'INVALID TRANSACTION TYPE' TO WS-RESP-MESSAGE
           END-IF
           IF RESP-SUCCESS
               PERFORM 1200-RETRIEVE-ACCOUNT
           END-IF.

       1200-RETRIEVE-ACCOUNT.
      *--- Simulate DB2 read ---
           MOVE 'SELECT' TO WS-DB-OPERATION
           PERFORM 7000-DB-ACCESS
           IF WS-SQLCODE NOT = 0
               SET RESP-ACCT-NOT-FND TO TRUE
               STRING 'ACCOUNT NOT FOUND: '
                      WS-TXN-ACCOUNT-FROM
                      DELIMITED BY SIZE
                      INTO WS-RESP-MESSAGE
               END-STRING
           ELSE
               IF ACCT-CLOSED
                   SET RESP-ACCT-CLOSED TO TRUE
                   MOVE 'ACCOUNT IS CLOSED' TO WS-RESP-MESSAGE
               ELSE IF ACCT-FROZEN
                   SET RESP-ACCT-FROZEN TO TRUE
                   MOVE 'ACCOUNT IS FROZEN' TO WS-RESP-MESSAGE
               END-IF
           END-IF
           MOVE WS-ACCT-BALANCE TO WS-BEFORE-BALANCE.

       2000-PROCESS-DEPOSIT.
           ADD WS-TXN-AMOUNT TO WS-ACCT-BALANCE
           MOVE WS-ACCT-BALANCE TO WS-ACCT-AVAIL-BAL
           MOVE WS-ACCT-BALANCE TO WS-AFTER-BALANCE
           MOVE 'UPDATE' TO WS-DB-OPERATION
           PERFORM 7000-DB-ACCESS
           IF WS-SQLCODE = 0
               SET RESP-SUCCESS TO TRUE
               MOVE WS-ACCT-BALANCE TO WS-RESP-NEW-BALANCE
               MOVE 'DEPOSIT SUCCESSFUL' TO WS-RESP-MESSAGE
               ADD 1 TO WS-DEPOSIT-COUNT
           ELSE
               SET RESP-DB-ERROR TO TRUE
               MOVE 'DATABASE ERROR ON DEPOSIT' TO WS-RESP-MESSAGE
               ADD 1 TO WS-FAILED-COUNT
           END-IF.

       3000-PROCESS-WITHDRAWAL.
      *--- Check daily withdrawal limit ---
           ADD WS-TXN-AMOUNT TO WS-ACCT-DAILY-USED
           IF WS-ACCT-DAILY-USED > WS-ACCT-DAILY-LIMIT
               SET RESP-DAILY-LIMIT TO TRUE
               MOVE 'DAILY WITHDRAWAL LIMIT EXCEEDED'
                   TO WS-RESP-MESSAGE
               SUBTRACT WS-TXN-AMOUNT FROM WS-ACCT-DAILY-USED
               ADD 1 TO WS-FAILED-COUNT
           ELSE
      *--- Check sufficient funds with overdraft ---
               COMPUTE WS-AFTER-BALANCE =
                   WS-ACCT-BALANCE - WS-TXN-AMOUNT
               IF WS-AFTER-BALANCE < (0 - WS-ACCT-OD-LIMIT)
                   SET RESP-INSUF-FUNDS TO TRUE
                   MOVE 'INSUFFICIENT FUNDS' TO WS-RESP-MESSAGE
                   SUBTRACT WS-TXN-AMOUNT FROM WS-ACCT-DAILY-USED
                   ADD 1 TO WS-FAILED-COUNT
               ELSE
                   MOVE WS-AFTER-BALANCE TO WS-ACCT-BALANCE
                   COMPUTE WS-ACCT-AVAIL-BAL =
                       WS-ACCT-BALANCE + WS-ACCT-OD-LIMIT
                   MOVE 'UPDATE' TO WS-DB-OPERATION
                   PERFORM 7000-DB-ACCESS
                   IF WS-SQLCODE = 0
                       SET RESP-SUCCESS TO TRUE
                       MOVE WS-ACCT-BALANCE TO WS-RESP-NEW-BALANCE
                       MOVE 'WITHDRAWAL SUCCESSFUL'
                           TO WS-RESP-MESSAGE
                       ADD 1 TO WS-WITHDRAW-COUNT
                   ELSE
                       SET RESP-DB-ERROR TO TRUE
                       MOVE 'DB ERROR ON WITHDRAWAL'
                           TO WS-RESP-MESSAGE
                       ADD 1 TO WS-FAILED-COUNT
                   END-IF
               END-IF
           END-IF.

       4000-PROCESS-TRANSFER.
      *--- Validate source account (already loaded) ---
           COMPUTE WS-AFTER-BALANCE =
               WS-ACCT-BALANCE - WS-TXN-AMOUNT
           IF WS-AFTER-BALANCE < (0 - WS-ACCT-OD-LIMIT)
               SET RESP-INSUF-FUNDS TO TRUE
               MOVE 'INSUFFICIENT FUNDS FOR TRANSFER'
                   TO WS-RESP-MESSAGE
               ADD 1 TO WS-FAILED-COUNT
           ELSE
      *--- Debit source account ---
               MOVE WS-AFTER-BALANCE TO WS-ACCT-BALANCE
               MOVE 'UPDATE' TO WS-DB-OPERATION
               PERFORM 7000-DB-ACCESS
               IF WS-SQLCODE = 0
      *--- Credit destination account ---
                   MOVE WS-TXN-ACCOUNT-TO TO WS-ACCT-NUMBER
                   MOVE 'SELECT' TO WS-DB-OPERATION
                   PERFORM 7000-DB-ACCESS
                   IF WS-SQLCODE = 0
                       ADD WS-TXN-AMOUNT TO WS-ACCT-BALANCE
                       MOVE 'UPDATE' TO WS-DB-OPERATION
                       PERFORM 7000-DB-ACCESS
                       IF WS-SQLCODE = 0
                           SET RESP-SUCCESS TO TRUE
                           MOVE WS-AFTER-BALANCE
                               TO WS-RESP-NEW-BALANCE
                           MOVE 'TRANSFER SUCCESSFUL'
                               TO WS-RESP-MESSAGE
                           ADD 1 TO WS-TRANSFER-COUNT
                       ELSE
                           SET RESP-DB-ERROR TO TRUE
                           MOVE 'DB ERROR CREDITING TARGET'
                               TO WS-RESP-MESSAGE
                           ADD 1 TO WS-FAILED-COUNT
      *--- TODO: Rollback source debit ---
                       END-IF
                   ELSE
                       SET RESP-ACCT-NOT-FND TO TRUE
                       MOVE 'TARGET ACCOUNT NOT FOUND'
                           TO WS-RESP-MESSAGE
                       ADD 1 TO WS-FAILED-COUNT
      *--- TODO: Rollback source debit ---
                   END-IF
               ELSE
                   SET RESP-DB-ERROR TO TRUE
                   MOVE 'DB ERROR DEBITING SOURCE'
                       TO WS-RESP-MESSAGE
                   ADD 1 TO WS-FAILED-COUNT
               END-IF
           END-IF.

       5000-PROCESS-INQUIRY.
           MOVE WS-ACCT-BALANCE TO WS-RESP-NEW-BALANCE
           SET RESP-SUCCESS TO TRUE
           MOVE 'INQUIRY SUCCESSFUL' TO WS-RESP-MESSAGE
           ADD 1 TO WS-INQUIRY-COUNT.

       6000-PROCESS-CLOSE-ACCOUNT.
           IF WS-ACCT-BALANCE NOT = ZEROS
               STRING 'ACCOUNT HAS BALANCE OF '
                      WS-ACCT-BALANCE
                      ' - MUST BE ZEROED FIRST'
                      DELIMITED BY SIZE
                      INTO WS-RESP-MESSAGE
               END-STRING
               SET RESP-INVALID-TXN TO TRUE
               ADD 1 TO WS-FAILED-COUNT
           ELSE
               MOVE 'C' TO WS-ACCT-STATUS
               MOVE 'UPDATE' TO WS-DB-OPERATION
               PERFORM 7000-DB-ACCESS
               IF WS-SQLCODE = 0
                   SET RESP-SUCCESS TO TRUE
                   MOVE 'ACCOUNT CLOSED SUCCESSFULLY'
                       TO WS-RESP-MESSAGE
               ELSE
                   SET RESP-DB-ERROR TO TRUE
                   MOVE 'DB ERROR CLOSING ACCOUNT'
                       TO WS-RESP-MESSAGE
                   ADD 1 TO WS-FAILED-COUNT
               END-IF
           END-IF.

       7000-DB-ACCESS.
      *--- Simulated database access ---
      *--- In production, this would contain EXEC SQL
      *--- or calls to DB2 stored procedures ---
           MOVE 0 TO WS-SQLCODE.

       8000-WRITE-AUDIT-TRAIL.
           MOVE WS-TXN-TIMESTAMP    TO WS-AUD-TIMESTAMP
           MOVE WS-TXN-TYPE         TO WS-AUD-TXN-TYPE
           MOVE WS-TXN-ACCOUNT-FROM TO WS-AUD-ACCT-FROM
           MOVE WS-TXN-ACCOUNT-TO   TO WS-AUD-ACCT-TO
           MOVE WS-TXN-AMOUNT       TO WS-AUD-AMOUNT
           MOVE WS-RESP-CODE        TO WS-AUD-RESULT
           MOVE WS-TXN-TELLER-ID    TO WS-AUD-TELLER
           MOVE WS-TXN-BRANCH       TO WS-AUD-BRANCH
           MOVE WS-BEFORE-BALANCE   TO WS-AUD-BEFORE-BAL
           MOVE WS-AFTER-BALANCE    TO WS-AUD-AFTER-BAL.
      *--- Would write to audit log file/DB2 table here ---
