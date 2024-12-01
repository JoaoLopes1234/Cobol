       IDENTIFICATION DIVISION.
       PROGRAM-ID. if-test.
       AUTHOR. JOÃO LOPES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
      
               DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 NOME  PIC X(9) VALUE SPACES.

       PROCEDURE DIVISION.
           DISPLAY "QUAL O SEU NOME?"
           ACCEPT NOME.
      *DISPLAY "QUAL O SEU TELEFONE?"
      *ACCEPT PHONE
      *DISPLAY "QUAL O SEU E-MAIL?"
      *ACCEPT EMAIL
           
           PERFORM P001-BEGIN THRU P001-FIM.
       P001-BEGIN.
      *    INSPECT NOME REPLACING TRAILING SPACES BY SPACE.
      *    INSPECT NOME REPLACING LEADING SPACES BY SPACE.
           IF NOME = "JOAO"
               DISPLAY "OLA JOÃO PEDRO"
           ELSE
               DISPLAY "OLA " NOME
           END-IF.
           DISPLAY "BEGIN".
           
       P001-FIM.
           GOBACK.
