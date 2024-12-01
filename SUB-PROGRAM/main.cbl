       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM1          PIC 9(5) VALUE 15.
       01  WS-NUM2          PIC 9(5) VALUE 20.
       01  WS-PRODUCT       PIC 9(10).
       01  DISPLAY-PRODUCT  PIC Z(10).

       PROCEDURE DIVISION.
           DISPLAY "Chamando o subprograma para calcular o produto..."
           CALL 'SUBPROG' USING WS-NUM1 WS-NUM2 WS-PRODUCT 
           MOVE WS-PRODUCT TO DISPLAY-PRODUCT
           DISPLAY "Produto calculado: " DISPLAY-PRODUCT
           STOP RUN.
