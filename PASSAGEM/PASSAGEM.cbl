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
               88 B-FAST-TRACK     VALUES "Y", "N".
           05 BAGS.
               10 VAL-BAGS         PIC 9(5).
               10 CHK-BAGS         PIC 9(1).
               10 TOTAL-BAGS       PIC 9(5).
           05 SEAT.
               10 VAL-SEAT         PIC 9(5)V9(2).
               88 B-SEAT           VALUES "Y", "N".
               10 TOTAL-SEAT       PIC 9(5).
           05 TOTAL-TICKET         PIC 9(6).        
       01 USER.
           05 NAME-USER            PIC X(10).
           05 AGE                  PIC 9(2).
           05 NATIONALITY          PIC A(10).
           88 OBJ-DANGEROUS        VALUES "Y", "N".   

       PROCEDURE DIVISION.
       INITIALIZE USER.
       INITIALIZE TICKET.
           MOVE 1000 TO VAL-TICKET.
           MOVE 30 TO VAL-BAGS.
           MOVE 20 TO VAL-SEAT.
      *    DISPLAY "Hello, WHAT IS YOUR NAME?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT NAME-USER.

      *    DISPLAY "HELLO, " NAME-USER " HOW OLD ARE YOU?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT AGE.
      *    
      *    IF AGE < 18 THEN
      *        DISPLAY "YOU CAN'T BUY A AIRPLANE TICKET, YOU NEED TO BE" 
      *        "OLDER THAN 18"
      *        STOP RUN.

      *    DISPLAY "HELLO, " NAME-USER " WHAT IS YOUR NATIONALITY?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT NATIONALITY.

      *    DISPLAY "HELLO, " NAME-USER " How many pieces of luggage"
      *     "would you like to check in?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT CHK-BAGS.  
      *    
      *    MULTIPLY CHK-BAGS BY VAL-BAGS GIVING TOTAL-BAGS.
      *    DISPLAY TOTAL-BAGS

           DISPLAY "HELLO, " NAME-USER " Do you want reserve your seat?"
           DISPLAY ">  " WITH NO ADVANCING
           ACCEPT SEAT.
           
           IF B-SEAT THEN
               MOVE VAL-SEAT TO TOTAL-SEAT.
           END-IF.

           
           

           COMPUTE TOTAL-TICKET = TOTAL-BAGS + VAL-TICKET + TOTAL-SEAT.
           DISPLAY "Ok " NAME-USER ", your flight total is " 
                   TOTAL-TICKET " euros.".
           STOP RUN.

      *    COMO FUNCIONA A VIRGULA
      *    BOAS PRÁTICAS COLOCAR STOP RUN NO MEIO DO CODIGO?     