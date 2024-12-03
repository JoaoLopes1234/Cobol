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
       01 INPUT-RECORD             PIC X(67).
           
      *    05 INPUT-NAME           PIC X(15).
      *    05 INPUT-AGE            PIC 9(2).
      *    05 INPUT-NATIONALITY    PIC A(13).
      *    05 INPUT-OBJDANGER      PIC A(7).
      *    05 INPUT-TICKET         PIC 9(11).
      *    05 INPUT-BAGS           PIC 9(11).
      *    05 INPUT-SEAT           PIC X(7).

       FD OUTPUT-FILE.
      *01 OUTPUT-RECORD.
      *    05 OUTPUT-NAME          PIC X(15).
      *    05 OUTPUT-AGE           PIC 9(2).
      *    05 OUTPUT-NATIONALITY   PIC A(13).
      *    05 OUTPUT-OBJDANGER     PIC A(7).
      *    05 OUTPUT-TICKET        PIC 9(11).
      *    05 OUTPUT-BAGS          PIC 9(11).
      *    05 OUTPUT-SEAT          PIC X(7).
       01 OUTPUT-TICKET-MESSAGE.
           05 OUTPUT-MESSAGE       PIC X(135).


       WORKING-STORAGE SECTION.
       01 INPUT-NAME        PIC X(15).
       01 INPUT-AGE         PIC 9(3).
       01 INPUT-NATIONALITY PIC X(13).
       01 INPUT-OBJDANGER   PIC X(7).
       01 INPUT-TICKET      PIC 9(11).
       01 INPUT-BAGS        PIC 9(11).
       01 INPUT-SEAT        PIC X(7).
      *01 TICKET.
      *    05 VAL-TICKET           PIC 9(5)V9(2).
      *    05 FAST-TRACK.
      *        10 VAL-FAST-TRACK   PIC 9(5)V9(2).
      *        88 B-FAST-TRACK     VALUES "Y", "YES", "SIM", "S", 
      *            "s".
      *    05 BAGS.
      *        10 VAL-BAGS         PIC 9(5).
      *        10 CHK-BAGS         PIC 9(1).
      *        10 TOTAL-BAGS       PIC 9(5).
      *    05 SEAT.
      *        10 SEAT-CODE        PIC X(1).
      *            88 B-SEAT           VALUES "Y", "YES", "SIM", "S", 
      *            "s".
      *        10 VAL-SEAT         PIC 9(5)V9(2).
      *        10 TOTAL-SEAT       PIC 9(5).
      *    05 TOTAL-TICKET         PIC 9(6).        
      *01 USER.
      *    05 NAME-USER            PIC X(10).
      *    05 AGE                  PIC 9(2).
      *    05 NATIONALITY          PIC A(10).
      *    05 OBJ-DANGER           PIC X(1).
      *        88 B-OBJ-DANGEROUS  VALUES "Y", "YES", "SIM", "S", 
      *            "s".       
       01 END-FILE                 PIC X(1).
       01 FIRST-CHARACTER          PIC X(1).
       PROCEDURE DIVISION.

           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.

           PERFORM UNTIL END-FILE = "S"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "S" TO END-FILE
                   NOT AT END
                       
      *                MOVE INPUT-RECORD(1:15) TO INPUT-NAME       *> Nome
      *                MOVE INPUT-RECORD(16:3) TO INPUT-AGE        *> Idade
      *                MOVE INPUT-RECORD(19:13)TO INPUT-NATIONALITY *> Nacionalidade
      *                MOVE INPUT-RECORD(32:7) TO INPUT-OBJDANGER  *> Objeto perigoso
      *                MOVE INPUT-RECORD(39:11) TO INPUT-TICKET     *> Bilhete
      *                MOVE INPUT-RECORD(50:11) TO INPUT-BAGS       *> Malas
      *                MOVE INPUT-RECORD(61:7) TO INPUT-SEAT       *> Lugar
      *                
      *                DISPLAY ">" INPUT-NAME "<"
      *                DISPLAY ">" INPUT-AGE "<"
      *                DISPLAY ">" INPUT-NATIONALITY "<"
      *                DISPLAY ">" INPUT-OBJDANGER "<"
      *                DISPLAY ">" INPUT-TICKET "<"
      *                DISPLAY ">" INPUT-BAGS "<"
      *                DISPLAY ">" INPUT-SEAT "<"

                       UNSTRING INPUT-RECORD 
                           INTO INPUT-NAME
                                INPUT-AGE
                                INPUT-NATIONALITY
                                INPUT-OBJDANGER
                                INPUT-TICKET
                                INPUT-BAGS
                                INPUT-SEAT

      *                DISPLAY ">" INPUT-NAME "<"
      *                DISPLAY ">" INPUT-AGE "<"
      *                DISPLAY ">" INPUT-NATIONALITY "<"
      *                DISPLAY ">" INPUT-OBJDANGER "<"
      *                DISPLAY ">" INPUT-TICKET "<"
      *                DISPLAY ">" INPUT-BAGS "<"
      *                DISPLAY ">" INPUT-SEAT "<"

      *                MOVE INPUT-NAME TO OUTPUT-NAME
      *                MOVE INPUT-AGE TO OUTPUT-AGE
      *                MOVE INPUT-NATIONALITY TO OUTPUT-NATIONALITY
      *                MOVE INPUT-OBJDANGER TO OUTPUT-OBJDANGER
      *                MOVE INPUT-TICKET TO OUTPUT-TICKET
      *                MOVE INPUT-BAGS TO OUTPUT-BAGS
      *                MOVE INPUT-SEAT TO OUTPUT-SEAT

      *                WRITE OUTPUT-RECORD
      ******************************************************************    
      *    ADICIONANDO O NOME DO PASSSAGEIRO
      ******************************************************************
                  STRING "O passageiro com o nome de " DELIMITED BY SIZE
                          INPUT-NAME DELIMITED BY SIZE
                          INTO OUTPUT-MESSAGE

      ******************************************************************    
      *    ADICIONANDO A IDADE DO PASSSAGEIRO
      ******************************************************************
                  DISPLAY OUTPUT-MESSAGE

                   STRING OUTPUT-MESSAGE
                          "< com a idade de " DELIMITED BY SIZE
                          INPUT-AGE DELIMITED BY SPACE
                          INTO OUTPUT-MESSAGE


                  DISPLAY OUTPUT-MESSAGE


      *         STRING "O passageiro com o nome de " DELIMITED BY SIZE
      *                  INPUT-NAME DELIMITED BY SIZE
      *                  "<com a idade de >" DELIMITED BY SIZE
      *                  INPUT-AGE DELIMITED BY SPACE
      *    "< é autorizado a viajar com um valor total de passagem de ",
      *                        DELIMITED BY SIZE
      *                        INPUT-TICKET DELIMITED BY SIZE
      *                        "!" DELIMITED BY SIZE
      *                        INTO OUTPUT-MESSAGE
                   STRING OUTPUT-MESSAGE
                         " é autorizado a viajar com um valor total de", 
                          "passagem de "
                          DELIMITED BY SIZE
                          INPUT-TICKET DELIMITED BY SIZE
                          "!" DELIMITED BY SIZE
                          INTO OUTPUT-MESSAGE



                               
                       END-STRING
      *                ADD 20 TO INPUT-TICKET
      *                DISPLAY ">" INPUT-TICKET "<"

      *                IF INPUT-AGE < 18 THEN
      *                    DISPLAY "INSIDE IF"
      *                    MOVE  INPUT-NAME TO OUTPUT-MESSAGE
      *                ELSE
      *                    DISPLAY "inside else"
      *                    MOVE "YOU CAN" TO OUTPUT-MESSAGE
      *                END-IF
      *                ADD INPUT-AGE TO OUTPUT-MESSAGE
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

 
