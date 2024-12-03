       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAS-test.
       AUTHOR. JOÃO LOPES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "test/data-test.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
              
           SELECT OUTPUT-FILE ASSIGN TO "test/output-test.txt"
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
           05 OUTPUT-MESSAGE       PIC X(259).


       WORKING-STORAGE SECTION.

       01 OUTPUT-MESSAGES.
           05 OUTPUT-NAME   PIC X(42).
           05 OUTPUT-AGE    PIC X(20).
           05 OUTPUT-NATIONALITY PIC X(20).
           05 OUTPUT-OBJDANGER   PIC X(35).
           05 OUTPUT-TICKET       PIC X(28).
           05 OUTPUT-BAGS         PIC X(86).
           05 OUTPUT-SEAT         PIC X(48).


       01 INPUT-NAME        PIC X(15).
       01 INPUT-AGE         PIC 9(3).
       01 INPUT-NATIONALITY PIC X(13).
       01 INPUT-OBJDANGER   PIC X(7).
       01 INPUT-TICKET      PIC 9(11).
       01 INPUT-BAGS        PIC 9(11).
       01 INPUT-SEAT        PIC X(7).
       01 OUTPUT-TEST       PIC X(40).
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
       01 VAL-BAGS                 PIC 9(5).
       01 TOTAL-BAGS               PIC 9(4).
       01 TOTAL-SEAT               PIC 9(2).
       01 TOTAL-TICKET             PIC 9(4).

       PROCEDURE DIVISION.

           MOVE 30 TO VAL-BAGS.


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
                      


                       UNSTRING INPUT-RECORD delimited by space
                           INTO INPUT-NAME
                           INPUT-AGE
                           INPUT-NATIONALITY
                           INPUT-OBJDANGER
                           INPUT-TICKET
                           INPUT-BAGS
                           INPUT-SEAT
                               



                       DISPLAY ">" INPUT-NAME "<"
                       DISPLAY ">" INPUT-AGE "<"
                       DISPLAY ">" INPUT-NATIONALITY "<"
                       DISPLAY ">" INPUT-OBJDANGER "<"
                       DISPLAY ">" INPUT-TICKET "<"
                       DISPLAY ">" INPUT-BAGS "<"
                       DISPLAY ">" INPUT-SEAT "<"

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
                          INTO OUTPUT-NAME

      ******************************************************************    
      *    ADICIONANDO A IDADE DO PASSSAGEIRO
      ******************************************************************

                   STRING "< com a idade de " DELIMITED BY SIZE
                          INPUT-AGE DELIMITED BY SPACE
                          INTO OUTPUT-AGE
                         
      ******************************************************************    
      *    AUTORIZACAO
      ******************************************************************
                   IF INPUT-AGE < 18 OR INPUT-OBJDANGER = '  .Yes ' THEN
                           MOVE ' terá de fazer check in no balcao '
                                   TO OUTPUT-OBJDANGER
                   ELSE
                           MOVE " é autorizado a viajar"
                                   TO OUTPUT-OBJDANGER
                   END-IF
      ******************************************************************    
      *    QUANTIDADE DE MALAS
      ******************************************************************
                   
                   MULTIPLY INPUT-BAGS BY VAL-BAGS GIVING TOTAL-BAGS

                   
                   STRING " despachou " DELIMITED BY SIZE
                          INPUT-BAGS DELIMITED BY SPACE
                          ' malas, com o custa de '
                          '30 euros cada uma, totalizando assim '
                          TOTAL-BAGS
                          INTO OUTPUT-BAGS

      ******************************************************************    
      *    TIPO DE ASSENTO
      ******************************************************************
                   EVALUATE INPUT-SEAT
                       WHEN '  .E   '
                           MOVE 30 TO TOTAL-SEAT
           MOVE ' vai viajar em classe economica e custa 30 euros '
                                   TO OUTPUT-SEAT
                       WHEN '  .J   '
                           MOVE 40 TO TOTAL-SEAT
           MOVE ' vai se sentar ao lado da janela e custa 40 euros', 
                                   TO OUTPUT-SEAT                           
                       WHEN '  .P   '  
                           MOVE 50 TO TOTAL-SEAT
           MOVE ' vai viajar em classe economica e custa 50 euros', 
                                   TO OUTPUT-SEAT
                       WHEN OTHER
                           MOVE 0 TO TOTAL-SEAT
                           MOVE 'O assento informado não é válido.', 
                                   TO OUTPUT-SEAT
                   END-EVALUATE

      ******************************************************************    
      *    SOMA TOTAL DOS VALORES
      ******************************************************************

                   COMPUTE TOTAL-TICKET  = TOTAL-BAGS + TOTAL-SEAT,
                                           + INPUT-TICKET

                   STRING ' com um valor total de ' 
                           TOTAL-TICKET
                           '!'
                           INTO OUTPUT-TICKET
      ******************************************************************    
      *    CONCATENACAO EM UMA STRING SÓ
      ******************************************************************

      *            STRING OUTPUT-NAME
      *                    OUTPUT-AGE
      *                    OUTPUT-OBJDANGER
      *                    OUTPUT-BAGS
      *                    OUTPUT-SEAT
      *                    OUTPUT-TICKET
      *                   INTO OUTPUT-MESSAGE
      *            END-STRING
                   WRITE OUTPUT-TICKET-MESSAGE
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.
           
           STOP RUN.

 
