      *================================================================*
      * PROGRAM-ID: HELLO
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Basic COBOL program to test SLM understanding
      *          of fundamental COBOL structure and syntax.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. XMAINFRAME-TEST.
       DATE-WRITTEN. 2026-03-03.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NAME           PIC X(30) VALUE 'XMAiNframe User'.
       01  WS-DATE.
           05 WS-YEAR        PIC 9(4).
           05 WS-MONTH       PIC 9(2).
           05 WS-DAY         PIC 9(2).
       01  WS-FORMATTED-DATE PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           STRING WS-YEAR DELIMITED BY SIZE
                  '-'      DELIMITED BY SIZE
                  WS-MONTH DELIMITED BY SIZE
                  '-'      DELIMITED BY SIZE
                  WS-DAY   DELIMITED BY SIZE
                  INTO WS-FORMATTED-DATE
           END-STRING
           DISPLAY 'Hello, ' WS-NAME '!'
           DISPLAY 'Today is: ' WS-FORMATTED-DATE
           STOP RUN.
