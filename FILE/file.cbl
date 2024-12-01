       IDENTIFICATION DIVISION.
       PROGRAM-ID. file.
       AUTHOR. JOÃO LOPES.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "FILE/dados.txt"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "FILE/output.txt"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 INPUT-NOME       PIC X(20).
           05 INPUT-IDADE      PIC 9(3).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUTPUT-NOME     PIC X(20).
           05 OUTPUT-IDADE    PIC 9(3).
       
       WORKING-STORAGE SECTION.
       01 FIM-ARQUIVO    PIC X VALUE "N".
       
       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.
       
           PERFORM UNTIL FIM-ARQUIVO = "S"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "S" TO FIM-ARQUIVO
                   NOT AT END
                       MOVE INPUT-NOME TO OUTPUT-NOME
                       MOVE INPUT-IDADE TO OUTPUT-IDADE
                       WRITE OUTPUT-RECORD
                       
               END-READ
           END-PERFORM.
       
           CLOSE INPUT-FILE OUTPUT-FILE.
           STOP RUN.
