       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULT       PIC 9(10).

       LINKAGE SECTION.
       01  LK-NUMBER1      PIC 9(5).
       01  LK-NUMBER2      PIC 9(5).
       01  LK-RESULT       PIC 9(10).

       PROCEDURE DIVISION USING LK-NUMBER1 LK-NUMBER2 LK-RESULT.
           MULTIPLY LK-NUMBER1 BY LK-NUMBER2 GIVING LK-RESULT
      *    MOVE WS-RESULT TO LK-RESULT
           EXIT PROGRAM.
