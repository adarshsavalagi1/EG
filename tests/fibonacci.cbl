       IDENTIFICATION DIVISION.
       PROGRAM-ID. Fibonacci.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N             PIC 9(02) VALUE 10.
       01 FIBONACCI-TABLE.
          05 FIBO-NUMBER OCCURS 20 TIMES PIC 9(10) VALUE 0.
       01 I             PIC 9(02) VALUE 2.
       01 TEMP1         PIC 9(10) VALUE 0.
       01 TEMP2         PIC 9(10) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 0 TO FIBO-NUMBER(1)
           MOVE 1 TO FIBO-NUMBER(2)
           
           PERFORM VARYING I FROM 3 BY 1 UNTIL I > N
               COMPUTE FIBO-NUMBER(I) = FIBO-NUMBER(I-1) + FIBO-NUMBER(I-2)
           END-PERFORM
           
           PERFORM DISPLAY-FIBONACCI
           
           STOP RUN.

       DISPLAY-FIBONACCI.
           DISPLAY "Fibonacci Sequence:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               DISPLAY "Fibonacci(" I ") = " FIBO-NUMBER(I)
           END-PERFORM.
