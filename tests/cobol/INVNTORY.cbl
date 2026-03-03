      *================================================================*
      * PROGRAM-ID: INVNTORY
      * AUTHOR: XMAiNframe Test Suite
      * DATE-WRITTEN: 2026-03-03
      * PURPOSE: Inventory management system demonstrating COBOL
      *          table handling, SEARCH/SEARCH ALL, SORT, INSPECT,
      *          STRING/UNSTRING, and subprogram CALL patterns.
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVNTORY.
       AUTHOR. XMAINFRAME-TEST.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE
               ASSIGN TO 'INVFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS INV-ITEM-ID
               FILE STATUS IS WS-INV-STATUS.

           SELECT SORT-FILE
               ASSIGN TO 'SORTWORK'.

           SELECT SORTED-OUTPUT
               ASSIGN TO 'SORTOUT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-FILE.
       01  INVENTORY-RECORD.
           05 INV-ITEM-ID         PIC X(10).
           05 INV-DESCRIPTION     PIC X(50).
           05 INV-CATEGORY        PIC X(3).
              88 CAT-ELECTRONICS  VALUE 'ELC'.
              88 CAT-FURNITURE    VALUE 'FRN'.
              88 CAT-CLOTHING     VALUE 'CLT'.
              88 CAT-FOOD         VALUE 'FOD'.
              88 CAT-RAW-MATERIAL VALUE 'RAW'.
           05 INV-WAREHOUSE       PIC X(4).
           05 INV-LOCATION.
              10 INV-AISLE        PIC X(2).
              10 INV-SHELF        PIC 9(2).
              10 INV-BIN          PIC 9(3).
           05 INV-QTY-ON-HAND     PIC S9(7)   COMP-3.
           05 INV-QTY-RESERVED    PIC S9(7)   COMP-3.
           05 INV-QTY-ON-ORDER    PIC S9(7)   COMP-3.
           05 INV-REORDER-POINT   PIC S9(7)   COMP-3.
           05 INV-REORDER-QTY     PIC S9(7)   COMP-3.
           05 INV-UNIT-COST       PIC S9(5)V99 COMP-3.
           05 INV-UNIT-PRICE      PIC S9(5)V99 COMP-3.
           05 INV-LAST-RECEIPT    PIC 9(8).
           05 INV-LAST-ISSUE      PIC 9(8).
           05 INV-STATUS          PIC X(1).
              88 ITEM-ACTIVE      VALUE 'A'.
              88 ITEM-DISCONTINUED VALUE 'D'.
              88 ITEM-ON-HOLD     VALUE 'H'.

       SD  SORT-FILE.
       01  SORT-RECORD.
           05 SORT-ITEM-ID        PIC X(10).
           05 SORT-DESCRIPTION    PIC X(50).
           05 SORT-CATEGORY       PIC X(3).
           05 SORT-WAREHOUSE      PIC X(4).
           05 SORT-QTY            PIC S9(7) COMP-3.
           05 SORT-VALUE          PIC S9(9)V99 COMP-3.

       FD  SORTED-OUTPUT.
       01  SORTED-RECORD          PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-INV-STATUS          PIC X(2).
       01  WS-OUT-STATUS          PIC X(2).

      *--- Warehouse summary table ---
       01  WS-WAREHOUSE-TABLE.
           05 WS-WH-ENTRY OCCURS 10 TIMES
                           ASCENDING KEY IS WS-WH-CODE
                           INDEXED BY WH-IDX.
              10 WS-WH-CODE       PIC X(4).
              10 WS-WH-ITEM-COUNT PIC 9(6) VALUE ZEROS.
              10 WS-WH-TOTAL-QTY  PIC S9(9) COMP-3 VALUE ZEROS.
              10 WS-WH-TOTAL-VAL  PIC S9(11)V99 COMP-3 VALUE ZEROS.
              10 WS-WH-REORDER-CT PIC 9(6) VALUE ZEROS.
       01  WS-WH-COUNT            PIC 9(2) VALUE ZEROS.

      *--- Category lookup table ---
       01  WS-CATEGORY-TABLE.
           05 FILLER PIC X(23) VALUE 'ELCElectronics         '.
           05 FILLER PIC X(23) VALUE 'FRNFurniture           '.
           05 FILLER PIC X(23) VALUE 'CLTClothing            '.
           05 FILLER PIC X(23) VALUE 'FODFood & Beverage     '.
           05 FILLER PIC X(23) VALUE 'RAWRaw Materials       '.
       01  WS-CAT-TABLE-R REDEFINES WS-CATEGORY-TABLE.
           05 WS-CAT-ENTRY OCCURS 5 TIMES
                           INDEXED BY CAT-IDX.
              10 WS-CAT-CODE      PIC X(3).
              10 WS-CAT-DESC      PIC X(20).

      *--- Reorder list (items below reorder point) ---
       01  WS-REORDER-LIST.
           05 WS-REORDER-COUNT    PIC 9(4) VALUE ZEROS.
           05 WS-REORDER-ITEMS.
              10 WS-REORDER-ENTRY OCCURS 100 TIMES
                                  INDEXED BY REORD-IDX.
                 15 WS-RO-ITEM-ID    PIC X(10).
                 15 WS-RO-DESC       PIC X(50).
                 15 WS-RO-QTY-HAND   PIC S9(7)   COMP-3.
                 15 WS-RO-REORD-PT   PIC S9(7)   COMP-3.
                 15 WS-RO-REORD-QTY  PIC S9(7)   COMP-3.
                 15 WS-RO-SUPPLIER   PIC X(10).

      *--- Working fields ---
       01  WS-WORK.
           05 WS-AVAILABLE-QTY    PIC S9(7)   VALUE ZEROS.
           05 WS-ITEM-VALUE       PIC S9(9)V99 VALUE ZEROS.
           05 WS-MARGIN-PCT       PIC S9(3)V99 VALUE ZEROS.
           05 WS-SEARCH-ID        PIC X(10).
           05 WS-FOUND-FLAG       PIC X(1) VALUE 'N'.
              88 WS-FOUND         VALUE 'Y'.
              88 WS-NOT-FOUND     VALUE 'N'.
           05 WS-CAT-NAME         PIC X(20).

       01  WS-PARSED-LOCATION.
           05 WS-P-WAREHOUSE      PIC X(4).
           05 WS-P-AISLE          PIC X(2).
           05 WS-P-SHELF          PIC 9(2).
           05 WS-P-BIN            PIC 9(3).

       01  WS-LOCATION-STRING     PIC X(20).
       01  WS-DELIM-COUNT         PIC 9(2).

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-CHECK-INVENTORY-LEVELS
           PERFORM 3000-GENERATE-REORDER-REPORT
           PERFORM 4000-SORT-BY-VALUE
           PERFORM 5000-DISPLAY-WAREHOUSE-SUMMARY
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O INVENTORY-FILE
           IF WS-INV-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INVENTORY FILE: '
                       WS-INV-STATUS
               STOP RUN
           END-IF
           INITIALIZE WS-WAREHOUSE-TABLE
           INITIALIZE WS-REORDER-LIST.

       2000-CHECK-INVENTORY-LEVELS.
      *--- Read all records and check levels ---
           MOVE LOW-VALUES TO INV-ITEM-ID
           START INVENTORY-FILE KEY > INV-ITEM-ID
               INVALID KEY
                   DISPLAY 'EMPTY INVENTORY FILE'
                   STOP RUN
           END-START
           READ INVENTORY-FILE NEXT RECORD
               AT END GO TO 2000-EXIT
           END-READ
           PERFORM UNTIL WS-INV-STATUS = '10'
               PERFORM 2100-PROCESS-ITEM
               READ INVENTORY-FILE NEXT RECORD
                   AT END CONTINUE
               END-READ
           END-PERFORM.
       2000-EXIT.
           EXIT.

       2100-PROCESS-ITEM.
      *--- Calculate available quantity ---
           COMPUTE WS-AVAILABLE-QTY =
               INV-QTY-ON-HAND - INV-QTY-RESERVED

      *--- Calculate item value ---
           COMPUTE WS-ITEM-VALUE =
               INV-QTY-ON-HAND * INV-UNIT-COST

      *--- Calculate profit margin ---
           IF INV-UNIT-COST > ZEROS
               COMPUTE WS-MARGIN-PCT =
                   ((INV-UNIT-PRICE - INV-UNIT-COST)
                    / INV-UNIT-COST) * 100
           END-IF

      *--- Update warehouse totals ---
           PERFORM 2110-UPDATE-WAREHOUSE-TOTALS

      *--- Check if reorder needed ---
           IF ITEM-ACTIVE AND
              WS-AVAILABLE-QTY < INV-REORDER-POINT
               IF WS-REORDER-COUNT < 100
                   ADD 1 TO WS-REORDER-COUNT
                   SET REORD-IDX TO WS-REORDER-COUNT
                   MOVE INV-ITEM-ID     TO
                       WS-RO-ITEM-ID(REORD-IDX)
                   MOVE INV-DESCRIPTION TO
                       WS-RO-DESC(REORD-IDX)
                   MOVE WS-AVAILABLE-QTY TO
                       WS-RO-QTY-HAND(REORD-IDX)
                   MOVE INV-REORDER-POINT TO
                       WS-RO-REORD-PT(REORD-IDX)
                   MOVE INV-REORDER-QTY TO
                       WS-RO-REORD-QTY(REORD-IDX)
               END-IF
           END-IF.

       2110-UPDATE-WAREHOUSE-TOTALS.
      *--- Search for warehouse in table ---
           SET WH-IDX TO 1
           SEARCH WS-WH-ENTRY
               AT END
                   IF WS-WH-COUNT < 10
                       ADD 1 TO WS-WH-COUNT
                       SET WH-IDX TO WS-WH-COUNT
                       MOVE INV-WAREHOUSE TO WS-WH-CODE(WH-IDX)
                       MOVE 1 TO WS-WH-ITEM-COUNT(WH-IDX)
                       MOVE INV-QTY-ON-HAND
                           TO WS-WH-TOTAL-QTY(WH-IDX)
                       MOVE WS-ITEM-VALUE
                           TO WS-WH-TOTAL-VAL(WH-IDX)
                       IF WS-AVAILABLE-QTY < INV-REORDER-POINT
                           MOVE 1 TO WS-WH-REORDER-CT(WH-IDX)
                       END-IF
                   END-IF
               WHEN WS-WH-CODE(WH-IDX) = INV-WAREHOUSE
                   ADD 1 TO WS-WH-ITEM-COUNT(WH-IDX)
                   ADD INV-QTY-ON-HAND
                       TO WS-WH-TOTAL-QTY(WH-IDX)
                   ADD WS-ITEM-VALUE
                       TO WS-WH-TOTAL-VAL(WH-IDX)
                   IF WS-AVAILABLE-QTY < INV-REORDER-POINT
                       ADD 1 TO WS-WH-REORDER-CT(WH-IDX)
                   END-IF
           END-SEARCH.

       3000-GENERATE-REORDER-REPORT.
           DISPLAY '===== INVENTORY REORDER REPORT ====='
           DISPLAY 'Items requiring reorder: ' WS-REORDER-COUNT
           DISPLAY ' '
           IF WS-REORDER-COUNT > ZEROS
               PERFORM VARYING REORD-IDX FROM 1 BY 1
                   UNTIL REORD-IDX > WS-REORDER-COUNT
                   DISPLAY 'Item: '
                       WS-RO-ITEM-ID(REORD-IDX)
                       ' Desc: '
                       WS-RO-DESC(REORD-IDX)
                   DISPLAY '  On Hand: '
                       WS-RO-QTY-HAND(REORD-IDX)
                       '  Reorder Pt: '
                       WS-RO-REORD-PT(REORD-IDX)
                       '  Order Qty: '
                       WS-RO-REORD-QTY(REORD-IDX)
               END-PERFORM
           ELSE
               DISPLAY 'No items require reorder at this time.'
           END-IF.

       4000-SORT-BY-VALUE.
      *--- Sort inventory by total value descending ---
           SORT SORT-FILE
               ON DESCENDING KEY SORT-VALUE
               INPUT PROCEDURE IS 4100-PREPARE-SORT
               OUTPUT PROCEDURE IS 4200-WRITE-SORTED.

       4100-PREPARE-SORT.
           MOVE LOW-VALUES TO INV-ITEM-ID
           START INVENTORY-FILE KEY > INV-ITEM-ID
               INVALID KEY
                   GO TO 4100-EXIT
           END-START
           READ INVENTORY-FILE NEXT RECORD
               AT END GO TO 4100-EXIT
           END-READ
           PERFORM UNTIL WS-INV-STATUS = '10'
               MOVE INV-ITEM-ID     TO SORT-ITEM-ID
               MOVE INV-DESCRIPTION TO SORT-DESCRIPTION
               MOVE INV-CATEGORY    TO SORT-CATEGORY
               MOVE INV-WAREHOUSE   TO SORT-WAREHOUSE
               MOVE INV-QTY-ON-HAND TO SORT-QTY
               COMPUTE SORT-VALUE =
                   INV-QTY-ON-HAND * INV-UNIT-COST
               RELEASE SORT-RECORD
               READ INVENTORY-FILE NEXT RECORD
                   AT END CONTINUE
               END-READ
           END-PERFORM.
       4100-EXIT.
           EXIT.

       4200-WRITE-SORTED.
           OPEN OUTPUT SORTED-OUTPUT
           RETURN SORT-FILE RECORD
               AT END GO TO 4200-EXIT
           END-RETURN
           PERFORM UNTIL 1 = 0
               WRITE SORTED-RECORD FROM SORT-RECORD
               RETURN SORT-FILE RECORD
                   AT END GO TO 4200-EXIT
               END-RETURN
           END-PERFORM.
       4200-EXIT.
           CLOSE SORTED-OUTPUT.

       5000-DISPLAY-WAREHOUSE-SUMMARY.
           DISPLAY ' '
           DISPLAY '===== WAREHOUSE SUMMARY ====='
           PERFORM VARYING WH-IDX FROM 1 BY 1
               UNTIL WH-IDX > WS-WH-COUNT
               PERFORM 5100-LOOKUP-CATEGORY
               DISPLAY 'Warehouse: ' WS-WH-CODE(WH-IDX)
               DISPLAY '  Items:       ' WS-WH-ITEM-COUNT(WH-IDX)
               DISPLAY '  Total Qty:   ' WS-WH-TOTAL-QTY(WH-IDX)
               DISPLAY '  Total Value: ' WS-WH-TOTAL-VAL(WH-IDX)
               DISPLAY '  Need Reorder:' WS-WH-REORDER-CT(WH-IDX)
               DISPLAY ' '
           END-PERFORM.

       5100-LOOKUP-CATEGORY.
      *--- Example of SEARCH ALL (binary search) ---
           SET CAT-IDX TO 1
           SEARCH WS-CAT-ENTRY
               AT END
                   MOVE 'UNKNOWN' TO WS-CAT-NAME
               WHEN WS-CAT-CODE(CAT-IDX) = INV-CATEGORY
                   MOVE WS-CAT-DESC(CAT-IDX) TO WS-CAT-NAME
           END-SEARCH.

       6000-PARSE-LOCATION.
      *--- Demonstrate UNSTRING to parse a location code ---
      *--- Format: "WH01-A1-05-123" ---
           MOVE ZEROS TO WS-DELIM-COUNT
           UNSTRING WS-LOCATION-STRING
               DELIMITED BY '-'
               INTO WS-P-WAREHOUSE
                    WS-P-AISLE
                    WS-P-SHELF
                    WS-P-BIN
               TALLYING IN WS-DELIM-COUNT
           END-UNSTRING.

       6100-FORMAT-LOCATION.
      *--- Demonstrate STRING to build location code ---
           INITIALIZE WS-LOCATION-STRING
           STRING WS-P-WAREHOUSE DELIMITED BY SPACES
                  '-' DELIMITED BY SIZE
                  WS-P-AISLE DELIMITED BY SPACES
                  '-' DELIMITED BY SIZE
                  WS-P-SHELF DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-P-BIN DELIMITED BY SIZE
                  INTO WS-LOCATION-STRING
           END-STRING.

       6200-INSPECT-DESCRIPTION.
      *--- Use INSPECT to count and replace characters ---
           INSPECT INV-DESCRIPTION
               TALLYING WS-DELIM-COUNT
               FOR ALL SPACES
           INSPECT INV-DESCRIPTION
               REPLACING ALL LOW-VALUES BY SPACES.

       9000-CLEANUP.
           CLOSE INVENTORY-FILE.
