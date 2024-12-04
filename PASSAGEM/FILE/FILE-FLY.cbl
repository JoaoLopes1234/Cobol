       IDENTIFICATION DIVISION.
       PROGRAM-ID. PASSAGEM.
       AUTHOR. JOÃO LOPES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "PASSAGEM/FILE/data.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
              
           SELECT OUTPUT-FILE ASSIGN TO "PASSAGEM/FILE/output-data.txt"
               ORGANIZATION IS LINE SEQUENTIAL. 
               
       DATA DIVISION.
       
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD            PIC X(39).
      *    05 INPUT-NAME           PIC X(14).
      *    05 INPUT-AGE            PIC 9(4).
      *    05 INPUT-NATIONALITY    PIC X(11).
      *    05 INPUT-OBJDANGER      PIC X(4).
      *    05 INPUT-TICKET         PIC 9(4).
      *    05 INPUT-BAGS           PIC 9(2).
      *    05 INPUT-SEAT           PIC X(3).

       FD OUTPUT-FILE.
       01 OUTPUT-TICKET-MESSAGE.
           05 OUTPUT-MESSAGE       PIC X(252).

      *01 OUTPUT-RECORD.
      *    05 OUTPUT-NAME          PIC X(15).
      *    05 OUTPUT-AGE           PIC 9(2).
      *    05 OUTPUT-NATIONALITY   PIC A(13).
      *    05 OUTPUT-OBJDANGER     PIC A(7).
      *    05 OUTPUT-TICKET        PIC 9(11).
      *    05 OUTPUT-BAGS          PIC 9(11).
      *    05 OUTPUT-SEAT          PIC X(7).

       WORKING-STORAGE SECTION.

       01 OUTPUT-MESSAGES.
           05 OUTPUT-NAME   PIC X(43).
           05 OUTPUT-AGE    PIC X(22).
           05 OUTPUT-NATIONALITY PIC X(20).
           05 OUTPUT-OBJDANGER   PIC X(34).
           05 OUTPUT-TICKET       PIC X(29).
           05 OUTPUT-BAGS         PIC X(76).
           05 OUTPUT-SEAT         PIC X(48).

       01 WS-FILE.
           05 WS-NAME        PIC X(14).
           05 WS-AGE         PIC 9(3).
           05 WS-NATIONALITY PIC X(11).
           05 WS-OBJDANGER   PIC X(4).
           05 WS-TICKET      PIC 9(4).
           05 WS-BAGS        PIC 9(2).
           05 WS-SEAT        PIC X(3).

       01 TICKET.
           05 VAL-TICKET           PIC 9(5)V9(2).
           05 FAST-TRACK.
               10 VAL-FAST-TRACK   PIC 9(5)V9(2).
               88 B-FAST-TRACK     VALUES "Y", "YES", "SIM", "S", 
                   "s".
           05 BAGS.
               10 VAL-BAGS         PIC 9(5).
               10 TOTAL-BAGS       PIC 9(3).
           05 SEAT.
               10 SEAT-CODE        PIC X(1).
                   88 B-SEAT           VALUES "Y", "YES", "SIM", "S", 
                   "s".
               10 TOTAL-SEAT       PIC 9(2).
           05 TOTAL-TICKET         PIC $9(4).        
        
       77 END-FILE                 PIC X(1).

       PROCEDURE DIVISION.
       

       1001-OPEN-FILES.

           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.
           PERFORM UNTIL END-FILE = "S"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "S" TO END-FILE
                   NOT AT END
                       
      *                MOVE INPUT-RECORD(1:15) TO INPUT-NAME      
      *                MOVE INPUT-RECORD(16:3) TO INPUT-AGE        
      *                MOVE INPUT-RECORD(19:13)TO INPUT-NATIONALITY 
      *                MOVE INPUT-RECORD(32:7) TO INPUT-OBJDANGER  
      *                MOVE INPUT-RECORD(39:11) TO INPUT-TICKET     
      *                MOVE INPUT-RECORD(50:11) TO INPUT-BAGS       
      *                MOVE INPUT-RECORD(61:7) TO INPUT-SEAT       
                      
                       UNSTRING INPUT-RECORD
                           INTO WS-NAME
                                WS-AGE
                                WS-NATIONALITY
                                WS-OBJDANGER
                                WS-TICKET
                                WS-BAGS
                                WS-SEAT

      *                DISPLAY ">" WS-NAME "<"
      *                DISPLAY ">" INPUT-AGE "<"
      *                DISPLAY ">" INPUT-NATIONALITY "<"
      *                DISPLAY ">" INPUT-OBJDANGER "<"
      *                DISPLAY ">" INPUT-TICKET "<"
      *                DISPLAY ">" INPUT-BAGS "<"
      *                DISPLAY ">" INPUT-SEAT "<"
               PERFORM 2001-ADD-PASSENGER-NAME
               PERFORM 2002-ADD-PASSENGER-AGE
               PERFORM 2003-CHECK-AUTHORIZATION
               PERFORM 2004-CALCULATE-BAGGAGE-COST
               PERFORM 2005-DETERMINE-SEAT-COST
               PERFORM 2006-CALCULATE-TOTAL-TICKET
               PERFORM 2007-CONCATENATE-MESSAGES
      *        PERFORM 3001-CLOSE-FILES
      *        PERFORM 3002-END-PROGRAM

               WRITE OUTPUT-TICKET-MESSAGE                     
               END-READ

           END-PERFORM.   

      ******************************************************************    
      *    ADICIONAR O NOME DO PASSSAGEIRO
      ******************************************************************
       2001-ADD-PASSENGER-NAME.
                  STRING "O passageiro com o nome de " DELIMITED BY SIZE
                         ">" WS-NAME "<" DELIMITED BY SIZE
                          INTO OUTPUT-NAME.

      ******************************************************************    
      *    ADICIONAR A IDADE DO PASSSAGEIRO
      ******************************************************************
       
       2002-ADD-PASSENGER-AGE.
                   STRING " com a idade de " DELIMITED BY SIZE
                          ">"WS-AGE"<" DELIMITED BY SPACE
                          INTO OUTPUT-AGE.
                         
      ******************************************************************    
      *    VERIFICAÇÃO DE AUTORIZAÇÃO
      ******************************************************************
       2003-CHECK-AUTHORIZATION.      
                   IF WS-AGE < 18 OR WS-OBJDANGER = '  .Yes ' THEN
                           MOVE ' terá de fazer check in no balcao '
                                   TO OUTPUT-OBJDANGER
                   ELSE
                           MOVE " é autorizado a viajar"
                                   TO OUTPUT-OBJDANGER
                   END-IF.

      ******************************************************************    
      *    QUANTIDADE DE MALAS
      ******************************************************************
       2004-CALCULATE-BAGGAGE-COST.  

                   MOVE 30 TO VAL-BAGS
                   MULTIPLY WS-BAGS BY VAL-BAGS GIVING TOTAL-BAGS
                   
                   STRING " despachou " DELIMITED BY SIZE
                          WS-BAGS DELIMITED BY SPACE
                          ' malas, com o custa de '
                          '30 euros cada uma, totalizando assim '
                          TOTAL-BAGS
                          INTO OUTPUT-BAGS.

      ******************************************************************    
      *    TIPO DE ASSENTO
      ******************************************************************
       2005-DETERMINE-SEAT-COST.
                   EVALUATE WS-SEAT
                       WHEN 'E'
                           MOVE 30 TO TOTAL-SEAT
           MOVE ' vai viajar em classe economica e custa 30 euros '
                                   TO OUTPUT-SEAT
                       WHEN 'J'
                           MOVE 40 TO TOTAL-SEAT
           MOVE ' vai se sentar ao lado da janela, custa 40 euros', 
                                   TO OUTPUT-SEAT                           
                       WHEN 'P'  
                           MOVE 50 TO TOTAL-SEAT
           MOVE ' vai viajar em classe premium, custará 50 euros', 
                                   TO OUTPUT-SEAT
                       WHEN OTHER
                           MOVE 0 TO TOTAL-SEAT
                           MOVE ' o assento informado não é válido ', 
                                   TO OUTPUT-SEAT
                   END-EVALUATE.

      ******************************************************************    
      *    SOMA TOTAL DOS VALORES
      ******************************************************************
       2006-CALCULATE-TOTAL-TICKET.      

                   COMPUTE TOTAL-TICKET  = TOTAL-BAGS + TOTAL-SEAT,
                                           + WS-TICKET

                   STRING ' com um valor total de ' 
                           TOTAL-TICKET
                           '!'
                           INTO OUTPUT-TICKET.
      ******************************************************************    
      *    CONCATENACAO EM UMA STRING SÓ
      ******************************************************************
       2007-CONCATENATE-MESSAGES.      

                   STRING OUTPUT-NAME
                           OUTPUT-AGE
                           OUTPUT-OBJDANGER
                           OUTPUT-BAGS
                           OUTPUT-SEAT
                           OUTPUT-TICKET
                          INTO OUTPUT-MESSAGE
                   END-STRING.

      ******************************************************************
      *    FECHANDO OS ARQUIVOS
      ******************************************************************
       3001-CLOSE-FILES.

           CLOSE INPUT-FILE
                 OUTPUT-FILE.
                 
      ******************************************************************
      *    FIM DO PROGRAMA
      ******************************************************************
       3002-END-PROGRAM.    
           STOP RUN.

 
