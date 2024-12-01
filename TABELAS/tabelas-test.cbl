       IDENTIFICATION DIVISION.
       PROGRAM-ID. tabelas-test.
       AUTHOR. JOÃO LOPES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
      
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESES.
           05 MES        PIC X(9) OCCURS 12 TIMES INDEXED BY IDX.
       01 IDX-MASK       PIC ZZZ.

       PROCEDURE DIVISION.
           MOVE "JANEIRO" TO MES(1)
           MOVE "FEVEREIRO" TO MES(2)
           MOVE "MARCO" TO MES(3)
           MOVE "ABRIL" TO MES(4)
           MOVE "MAIO" TO MES(5)
           MOVE "JUNHO" TO MES(6)
           MOVE "JULHO" TO MES(7)
           MOVE "AGOSTO" TO MES(8)
           MOVE "SETEMBRO" TO MES(9)
           MOVE "OUTUBRO" TO MES(10)
           MOVE "NOVEMBRO" TO MES(11)
           MOVE "DEZEMBRO" TO MES(12)
           SET IDX TO 1
           PERFORM UNTIL IDX > 12
               MOVE IDX TO IDX-MASK
               DISPLAY "MÊS " IDX-MASK ": " MES(IDX)
               SET IDX UP BY 1
           END-PERFORM.