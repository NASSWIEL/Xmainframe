      *================================================================*
      * PROGRAM-ID: BATCHJCL
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Batch job control program demonstrating COBOL
      *          features typically used in JCL-driven batch:
      *          ACCEPT/DISPLAY, RETURN-CODE, SORT/MERGE,
      *          inter-program communication via CALL, and
      *          condition-code based flow control.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHJCL.
       AUTHOR. XMAINFRAME-TEST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       SPECIAL-NAMES.
           C01 IS PAGE-EJECT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO 'INFILE'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.

           SELECT OUTPUT-FILE
               ASSIGN TO 'OUTFILE'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

           SELECT SORT-WORK
               ASSIGN TO 'SORTWK01'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INPUT-RECORD.
           05 IN-RECORD-TYPE      PIC X(2).
              88 IN-HEADER        VALUE 'HD'.
              88 IN-DETAIL        VALUE 'DT'.
              88 IN-TRAILER       VALUE 'TR'.
           05 IN-SEQUENCE-NUM     PIC 9(6).
           05 IN-DATA             PIC X(72).

       FD  OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OUTPUT-RECORD          PIC X(132).

       SD  SORT-WORK.
       01  SORT-RECORD.
           05 SORT-KEY-1          PIC X(10).
           05 SORT-KEY-2          PIC 9(6).
           05 SORT-DATA           PIC X(64).

       WORKING-STORAGE SECTION.
       01  WS-IN-STATUS           PIC X(2).
       01  WS-OUT-STATUS          PIC X(2).

       01  WS-CONTROL-FIELDS.
           05 WS-PARM-DATA        PIC X(100).
           05 WS-PARM-LENGTH      PIC S9(4) COMP.
           05 WS-RUN-DATE         PIC X(8).
           05 WS-RUN-TIME         PIC X(8).
           05 WS-JOB-NAME         PIC X(8).
           05 WS-STEP-NAME        PIC X(8).
           05 WS-RETURN-CODE      PIC S9(4) COMP VALUE ZEROS.

       01  WS-FLAGS.
           05 WS-EOF-FLAG         PIC X VALUE 'N'.
              88 WS-EOF           VALUE 'Y'.
           05 WS-ERROR-FLAG       PIC X VALUE 'N'.
              88 WS-HAS-ERROR     VALUE 'Y'.
           05 WS-HEADER-FOUND     PIC X VALUE 'N'.
              88 WS-HDR-PROCESSED VALUE 'Y'.

       01  WS-COUNTERS.
           05 WS-INPUT-COUNT      PIC 9(8) VALUE ZEROS.
           05 WS-OUTPUT-COUNT     PIC 9(8) VALUE ZEROS.
           05 WS-HEADER-COUNT     PIC 9(4) VALUE ZEROS.
           05 WS-DETAIL-COUNT     PIC 9(8) VALUE ZEROS.
           05 WS-TRAILER-COUNT    PIC 9(4) VALUE ZEROS.
           05 WS-ERROR-COUNT      PIC 9(6) VALUE ZEROS.
           05 WS-EXPECTED-COUNT   PIC 9(8) VALUE ZEROS.
           05 WS-SEQUENCE-PREV    PIC 9(6) VALUE ZEROS.

       01  WS-HEADER-DATA.
           05 WS-HDR-FILE-ID      PIC X(10).
           05 WS-HDR-CREATE-DATE  PIC 9(8).
           05 WS-HDR-DESCRIPTION  PIC X(40).

       01  WS-DETAIL-DATA.
           05 WS-DTL-KEY          PIC X(10).
           05 WS-DTL-AMOUNT       PIC S9(9)V99 COMP-3.
           05 WS-DTL-TYPE         PIC X(2).
           05 WS-DTL-DESCRIPTION  PIC X(40).

       01  WS-TRAILER-DATA.
           05 WS-TRL-RECORD-COUNT PIC 9(8).
           05 WS-TRL-HASH-TOTAL   PIC 9(12).

       01  WS-HASH-TOTAL          PIC 9(12) VALUE ZEROS.

      *--- Report lines ---
       01  WS-REPORT-TITLE.
           05 FILLER PIC X(30)
                     VALUE 'BATCH PROCESSING REPORT       '.
           05 WS-RPT-DATE PIC X(10).
           05 FILLER PIC X(92) VALUE SPACES.

       01  WS-REPORT-COLHDR.
           05 FILLER PIC X(10) VALUE 'KEY       '.
           05 FILLER PIC X(2)  VALUE '  '.
           05 FILLER PIC X(12) VALUE 'AMOUNT      '.
           05 FILLER PIC X(2)  VALUE '  '.
           05 FILLER PIC X(4)  VALUE 'TYPE'.
           05 FILLER PIC X(2)  VALUE '  '.
           05 FILLER PIC X(40) VALUE 'DESCRIPTION'.
           05 FILLER PIC X(60) VALUE SPACES.

       01  WS-REPORT-DETAIL.
           05 WS-RPT-KEY          PIC X(10).
           05 FILLER              PIC X(2) VALUE SPACES.
           05 WS-RPT-AMOUNT       PIC ZZZ,ZZZ,ZZ9.99-.
           05 FILLER              PIC X(2) VALUE SPACES.
           05 WS-RPT-TYPE         PIC X(4).
           05 FILLER              PIC X(2) VALUE SPACES.
           05 WS-RPT-DESC         PIC X(40).
           05 FILLER              PIC X(56) VALUE SPACES.

       01  WS-SUBPGM-AREA.
           05 WS-SUB-FUNCTION     PIC X(8).
           05 WS-SUB-INPUT        PIC X(100).
           05 WS-SUB-OUTPUT       PIC X(100).
           05 WS-SUB-RETURN-CODE  PIC S9(4) COMP.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 0100-GET-PARAMETERS
           PERFORM 1000-INITIALIZE
           IF NOT WS-HAS-ERROR
               PERFORM 2000-PROCESS-INPUT
                   UNTIL WS-EOF OR WS-HAS-ERROR
               PERFORM 3000-VALIDATE-TOTALS
           END-IF
           PERFORM 4000-WRITE-SUMMARY
           PERFORM 9000-CLEANUP
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.

       0100-GET-PARAMETERS.
      *--- Accept JCL PARM data ---
           ACCEPT WS-PARM-DATA FROM COMMAND-LINE
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           ACCEPT WS-RUN-TIME FROM TIME
           DISPLAY 'BATCH JOB STARTED'
           DISPLAY 'PARM DATA: ' WS-PARM-DATA
           DISPLAY 'RUN DATE:  ' WS-RUN-DATE
           DISPLAY 'RUN TIME:  ' WS-RUN-TIME.

       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE
           IF WS-IN-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INPUT FILE: ' WS-IN-STATUS
               MOVE 'Y' TO WS-ERROR-FLAG
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           OPEN OUTPUT OUTPUT-FILE
           IF WS-OUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING OUTPUT FILE: ' WS-OUT-STATUS
               MOVE 'Y' TO WS-ERROR-FLAG
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           IF NOT WS-HAS-ERROR
               WRITE OUTPUT-RECORD FROM WS-REPORT-TITLE
                   AFTER ADVANCING PAGE-EJECT
               WRITE OUTPUT-RECORD FROM WS-REPORT-COLHDR
                   AFTER ADVANCING 2 LINES
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                       DISPLAY 'WARNING: INPUT FILE IS EMPTY'
                       MOVE 4 TO WS-RETURN-CODE
               END-READ
           END-IF.

       2000-PROCESS-INPUT.
           ADD 1 TO WS-INPUT-COUNT
           EVALUATE TRUE
               WHEN IN-HEADER
                   PERFORM 2100-PROCESS-HEADER
               WHEN IN-DETAIL
                   PERFORM 2200-PROCESS-DETAIL
               WHEN IN-TRAILER
                   PERFORM 2300-PROCESS-TRAILER
               WHEN OTHER
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY 'INVALID RECORD TYPE: ' IN-RECORD-TYPE
                           ' AT SEQUENCE: ' IN-SEQUENCE-NUM
                   IF WS-ERROR-COUNT > 100
                       DISPLAY 'TOO MANY ERRORS - ABORTING'
                       MOVE 'Y' TO WS-ERROR-FLAG
                       MOVE 16 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE
           IF NOT WS-HAS-ERROR
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF-FLAG
               END-READ
           END-IF.

       2100-PROCESS-HEADER.
           IF WS-HDR-PROCESSED
               DISPLAY 'DUPLICATE HEADER AT SEQ: ' IN-SEQUENCE-NUM
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               MOVE 'Y' TO WS-HEADER-FOUND
               ADD 1 TO WS-HEADER-COUNT
               MOVE IN-DATA(1:10) TO WS-HDR-FILE-ID
               MOVE IN-DATA(11:8) TO WS-HDR-CREATE-DATE
               MOVE IN-DATA(19:40) TO WS-HDR-DESCRIPTION
               DISPLAY 'PROCESSING FILE: ' WS-HDR-FILE-ID
               DISPLAY 'CREATED: ' WS-HDR-CREATE-DATE
           END-IF.

       2200-PROCESS-DETAIL.
           IF NOT WS-HDR-PROCESSED
               DISPLAY 'DETAIL BEFORE HEADER AT SEQ: '
                       IN-SEQUENCE-NUM
               ADD 1 TO WS-ERROR-COUNT
           ELSE
      *--- Sequence check ---
               IF IN-SEQUENCE-NUM <= WS-SEQUENCE-PREV
                   DISPLAY 'SEQUENCE ERROR: ' IN-SEQUENCE-NUM
                           ' PREV: ' WS-SEQUENCE-PREV
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
               MOVE IN-SEQUENCE-NUM TO WS-SEQUENCE-PREV
               ADD 1 TO WS-DETAIL-COUNT
      *--- Parse detail data ---
               MOVE IN-DATA(1:10) TO WS-DTL-KEY
               MOVE IN-DATA(11:40) TO WS-DTL-DESCRIPTION
      *--- Call validation subprogram ---
               MOVE 'VALIDATE' TO WS-SUB-FUNCTION
               MOVE IN-DATA TO WS-SUB-INPUT
               CALL 'VALIDSUB' USING WS-SUBPGM-AREA
                   ON EXCEPTION
                       DISPLAY 'VALIDATION SUBPROGRAM NOT FOUND'
                       ADD 1 TO WS-ERROR-COUNT
               END-CALL
               IF WS-SUB-RETURN-CODE = 0
      *--- Write to output ---
                   MOVE WS-DTL-KEY  TO WS-RPT-KEY
                   MOVE WS-DTL-DESCRIPTION TO WS-RPT-DESC
                   WRITE OUTPUT-RECORD FROM WS-REPORT-DETAIL
                       AFTER ADVANCING 1 LINE
                   ADD 1 TO WS-OUTPUT-COUNT
               ELSE
                   ADD 1 TO WS-ERROR-COUNT
               END-IF
      *--- Accumulate hash total ---
               ADD IN-SEQUENCE-NUM TO WS-HASH-TOTAL
           END-IF.

       2300-PROCESS-TRAILER.
           ADD 1 TO WS-TRAILER-COUNT
           MOVE IN-DATA(1:8) TO WS-TRL-RECORD-COUNT
           MOVE IN-DATA(9:12) TO WS-TRL-HASH-TOTAL
           MOVE IN-DATA(1:8) TO WS-EXPECTED-COUNT.

       3000-VALIDATE-TOTALS.
           IF WS-HEADER-COUNT = ZEROS
               DISPLAY 'ERROR: NO HEADER RECORD FOUND'
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           IF WS-TRAILER-COUNT = ZEROS
               DISPLAY 'ERROR: NO TRAILER RECORD FOUND'
               MOVE 12 TO WS-RETURN-CODE
           END-IF
           IF WS-DETAIL-COUNT NOT = WS-EXPECTED-COUNT
               DISPLAY 'RECORD COUNT MISMATCH:'
               DISPLAY '  EXPECTED: ' WS-EXPECTED-COUNT
               DISPLAY '  ACTUAL:   ' WS-DETAIL-COUNT
               MOVE 8 TO WS-RETURN-CODE
           END-IF
           IF WS-HASH-TOTAL NOT = WS-TRL-HASH-TOTAL
               DISPLAY 'HASH TOTAL MISMATCH:'
               DISPLAY '  EXPECTED: ' WS-TRL-HASH-TOTAL
               DISPLAY '  COMPUTED: ' WS-HASH-TOTAL
               MOVE 8 TO WS-RETURN-CODE
           END-IF.

       4000-WRITE-SUMMARY.
           DISPLAY '========================================='
           DISPLAY 'BATCH PROCESSING SUMMARY'
           DISPLAY '========================================='
           DISPLAY 'Input Records Read:    ' WS-INPUT-COUNT
           DISPLAY 'Output Records Written:' WS-OUTPUT-COUNT
           DISPLAY 'Header Records:        ' WS-HEADER-COUNT
           DISPLAY 'Detail Records:        ' WS-DETAIL-COUNT
           DISPLAY 'Trailer Records:       ' WS-TRAILER-COUNT
           DISPLAY 'Error Records:         ' WS-ERROR-COUNT
           DISPLAY 'Return Code:           ' WS-RETURN-CODE
           DISPLAY '========================================='.

       9000-CLEANUP.
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.
