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
               ORGANIZATION IS SEQUENTIAL.
              
           SELECT OUTPUT-FILE ASSIGN TO "PASSAGEM/FILE/output-data.txt"
               ORGANIZATION IS SEQUENTIAL. 
               
       DATA DIVISION.
       
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 INPUT-NAME           PIC X(16).
           05 INPUT-AGE            PIC 9(5).
           05 INPUT-NATIONALITY    PIC A(14).
           05 INPUT-OBJDANGER      PIC A(11).
           05 INPUT-TICKET         PIC 9(13).
           05 INPUT-BAGS           PIC 9(5).
           05 INPUT-SEAT           PIC X(3).

       FD OUTPUT-FILE.
      *01 OUTPUT-RECORD.
      *    05 OUTPUT-NAME          PIC X(16).
      *    05 OUTPUT-AGE           PIC 9(5).
      *    05 OUTPUT-NATIONALITY   PIC A(14).
      *    05 OUTPUT-OBJDANGER     PIC A(11).
      *    05 OUTPUT-TICKET        PIC 9(13).
      *    05 OUTPUT-BAGS          PIC 9(5).
      *    05 OUTPUT-SEAT          PIC X(3).
       01 OUTPUT-TICKET-MESSAGE.
           05 OUTPUT-MESSAGE       PIC X(67).


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
       
       01 END-FILE                 PIC X(1).
       PROCEDURE DIVISION.
       INITIALIZE USER.
       INITIALIZE TICKET.
           MOVE 1000 TO VAL-TICKET.
           MOVE 30 TO VAL-BAGS.
           MOVE 20 TO VAL-SEAT.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.

           PERFORM UNTIL END-FILE = "S"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "S" TO END-FILE
                   NOT AT END

      *                MOVE INPUT-NAME TO OUTPUT-NAME
      *                MOVE INPUT-AGE TO OUTPUT-AGE
      *                MOVE INPUT-NATIONALITY TO OUTPUT-NATIONALITY
      *                MOVE INPUT-OBJDANGER TO OUTPUT-OBJDANGER
      *                MOVE INPUT-TICKET TO OUTPUT-TICKET
      *                MOVE INPUT-BAGS TO OUTPUT-BAGS
      *                MOVE INPUT-SEAT TO OUTPUT-SEAT

      *                WRITE OUTPUT-RECORD
                       DISPLAY INPUT-AGE
                       DISPLAY INPUT-NAME
                       IF INPUT-AGE < 18 THEN
                           DISPLAY "INSIDE IF"
                           MOVE  INPUT-NAME TO OUTPUT-MESSAGE
                       ELSE
                           DISPLAY "inside else"
                           MOVE "YOU CAN" TO OUTPUT-MESSAGE
                       END-IF
                       ADD INPUT-AGE TO OUTPUT-MESSAGE
                       WRITE OUTPUT-TICKET-MESSAGE
                      
               END-READ
           END-PERFORM
            CLOSE INPUT-FILE
            CLOSE OUTPUT-FILE.
           
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

      *    DISPLAY "HELLO, " NAME-USER " Do you want reserve your seat?"
      *    DISPLAY ">  " WITH NO ADVANCING
      *    ACCEPT SEAT.
      *    
      *    IF B-SEAT THEN
      *        MOVE VAL-SEAT TO TOTAL-SEAT.


      *    
      *    

      *    COMPUTE TOTAL-TICKET = TOTAL-BAGS + VAL-TICKET + TOTAL-SEAT.
      *    DISPLAY "Ok " NAME-USER ", your flight total is " 
      *            TOTAL-TICKET " euros.".
           STOP RUN.

      *    COMO FUNCIONA A VIRGULA
      *    BOAS PRÁTICAS COLOCAR STOP RUN NO MEIO DO CODIGO?     
