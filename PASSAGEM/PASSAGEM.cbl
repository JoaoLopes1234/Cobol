       IDENTIFICATION DIVISION.
       PROGRAM-ID. PASSAGEM.
       AUTHOR. JOÃO LOPES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           
       01 TICKET.
           05 VAL-TICKET           PIC 9(5)V9(2).
           05 FAST-TRACK.
               10 VAL-FAST-TRACK   PIC 9(5)V9(2).
               10 FAST-TRACK-CODE  PIC X(1).
               88 B-FAST-TRACK     VALUES "Y", "YES", "SIM", "S", "s",
                   "sim", 'yes'.
               10 TOTAL-FAST-TRACK PIC 9(6).
           05 BAGS.
               10 VAL-BAGS         PIC 9(5).
               10 CHK-BAGS         PIC 9(1).
               10 TOTAL-BAGS       PIC 9(5).
           05 SEAT.
               10 SEAT-CODE        PIC X(1).
                   88 B-SEAT       VALUES "Y", "YES", "SIM", "S", "s",
                   "sim".
               10 VAL-SEAT         PIC 9(5)V9(2).
               
               10 TOTAL-SEAT       PIC 9(5).
           05 TOTAL-TICKET         PIC 9(6).
           05 DISPLAY-TICKET       PIC Z(6).        
       01 USER.
           05 NAME-USER            PIC X(10).
           05 AGE                  PIC 9(2).
           05 NATIONALITY          PIC A(10).
           05 OBJ-DANGEROUS          PIC X(1).
               88 B-DANGEROUS     VALUES "Y", "YES", "SIM", "S", "s",
                   "sim".

       PROCEDURE DIVISION.

       INITIALIZE USER.
       INITIALIZE TICKET.

           MOVE 1000 TO VAL-TICKET.
           MOVE 30 TO VAL-BAGS.
           MOVE 50 TO VAL-FAST-TRACK

      ******************************************************************    
      *    QUAL O NOME DO PASSSAGEIRO
      ******************************************************************
           DISPLAY "Hello, WHAT IS YOUR NAME?"
           DISPLAY ">  " WITH NO ADVANCING
           ACCEPT NAME-USER.

      ******************************************************************    
      *    QUAL A IDADE DO PASSSAGEIRO E VERIFICACÃO
      ******************************************************************
           DISPLAY "HELLO, " NAME-USER " HOW OLD ARE YOU?"
           DISPLAY ">  " WITH NO ADVANCING
           ACCEPT AGE.
           
           IF AGE < 18 THEN
               DISPLAY "YOU CAN'T BUY A AIRPLANE TICKET, YOU NEED TO BE" 
               " OLDER THAN 18"
               STOP RUN.

      ******************************************************************    
      *    SE A PESSOA POSSUI OBJETOS PERIGOSOS
      ******************************************************************
           DISPLAY "HELLO, " NAME-USER " Are you carrying any dangerous" 
                   "objects? (Y/N)"
           DISPLAY ">  " WITH NO ADVANCING
           ACCEPT OBJ-DANGEROUS.
           

           IF B-DANGEROUS THEN
               DISPLAY "You need to check in at the counter."
               STOP RUN.

      ******************************************************************    
      *    QUAL A NACIONALIDADE DO PASSAGEIRO
      ******************************************************************

      *    DISPLAY "HELLO, " NAME-USER " do you belong to the UE?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT NATIONALITY.
           
      ******************************************************************    
      *    FAST TRACK
      ******************************************************************
           PERFORM UNTIL B-FAST-TRACK
           DISPLAY 'HELLO ' NAME-USER 'Would you like to Fast Track?'
           ACCEPT FAST-TRACK-CODE

           IF B-FAST-TRACK THEN
               MOVE VAL-FAST-TRACK TO TOTAL-FAST-TRACK
           END-IF
           END-PERFORM
       
      ******************************************************************    
      *    QUANTIDADE DE MALAS
      ******************************************************************
           DISPLAY "HELLO, " NAME-USER " How many pieces of luggage "
                   "would you like to check in?"

           PERFORM UNTIL CHK-BAGS IS NUMERIC
               DISPLAY 'Insert a number'
               DISPLAY ">  " WITH NO ADVANCING
               ACCEPT CHK-BAGS
           END-PERFORM
           
           MULTIPLY CHK-BAGS BY VAL-BAGS GIVING TOTAL-BAGS.

      ******************************************************************    
      *    TIPO DE ACENTO
      ******************************************************************
            
           DISPLAY "HELLO, " NAME-USER " Do you want reserve your seat?"
           DISPLAY "You can choose economy class = E, window seat = J, "
                   'premium class = P or other'
           DISPLAY ">  " WITH NO ADVANCING
           ACCEPT SEAT-CODE.
           
           EVALUATE SEAT-CODE
               WHEN 'E'
                   MOVE 30 TO TOTAL-SEAT
               WHEN 'J'
                   MOVE 40 TO TOTAL-SEAT
               WHEN 'P'
                   MOVE 50 TO TOTAL-SEAT
               WHEN OTHER
                   MOVE 0 TO TOTAL-SEAT
           END-EVALUATE.


           
      *    DISPLAY TOTAL-BAGS
      *    DISPLAY TOTAL-SEAT
      *    DISPLAY VAL-TICKET

      ******************************************************************    
      *    SOMA TOTAL DOS VALORES
      ******************************************************************

           COMPUTE TOTAL-TICKET = TOTAL-BAGS + VAL-TICKET + TOTAL-SEAT.

           MOVE TOTAL-TICKET TO DISPLAY-TICKET
           DISPLAY "Ok " NAME-USER ", your flight total is " 
                   DISPLAY-TICKET " euros.".
           STOP RUN.

      *    BOAS PRÁTICAS COLOCAR STOP RUN NO MEIO DO CODIGO?
      *    USANDO Z NAO APARECE 0, MAS CONTINUA COM ESPACO     
       