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
               COMPUTE FIBO-NUMBER(I) = FIBO-NUMBER(I-1) + FIBOz-NUMBER(I-2)
           END-PERFORM
           
           PERFORM DISPLAY-FIBONACCI
           
           STOP RUN.

       DISPLAY-FIBONACCI.
           DISPLAY "Fibonacci Sequence:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               DISPLAY "Fibonacci(" I ") = " FIBO-NUMBER(I)
           END-PERFORM.





IDENTIFICATION DIVISION.
       PROGRAM-ID. ExampleProgram.
       AUTHOR. YourName.
       DATE-WRITTEN. 20240514.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1        PIC 9(5) VALUE 10.
       01 WS-NUM2        PIC 9(5) VALUE 20.
       01 WS-RESULT      PIC 9(5).
       01 WS-MESSAGE     PIC X(30).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM ADDITION
           PERFORM SUBTRACTION
           PERFORM MULTIPLICATION
           PERFORM DIVISION
           PERFORM CONCATENATE-MESSAGE
           PERFORM REVERSE-MESSAGE
           PERFORM LENGTH-OF-MESSAGE
           PERFORM DISPLAY-HELLO
           PERFORM DISPLAY-GOODBYE
           PERFORM DISPLAY-DATE
           STOP RUN.

       ADDITION.
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2
           DISPLAY 'Addition Result: ' WS-RESULT.

       SUBTRACTION.
           COMPUTE WS-RESULT = WS-NUM2 - WS-NUM1
           DISPLAY 'Subtraction Result: ' WS-RESULT.

       MULTIPLICATION.
           COMPUTE WS-RESULT = WS-NUM1 * WS-NUM2
           DISPLAY 'Multiplication Result: ' WS-RESULT.

       DIVISION.
           IF WS-NUM1 NOT = 0
               COMPUTE WS-RESULT = WS-NUM2 / WS-NUM1
               DISPLAY 'Division Result: ' WS-RESULT
           ELSE
               DISPLAY 'Division by zero is not allowed'
           END-IF.

       CONCATENATE-MESSAGE.
           MOVE 'Hello, ' TO WS-MESSAGE
           STRING WS-MESSAGE DELIMITED BY SPACE
                  'World!' DELIMITED BY SIZE
                  INTO WS-MESSAGE
           DISPLAY 'Concatenated Message: ' WS-MESSAGE.

       REVERSE-MESSAGE.
           MOVE 'COBOL' TO WS-MESSAGE
           PERFORM VARYING WS-RESULT FROM LENGTH OF WS-MESSAGE BY -1
               UNTIL WS-RESULT < 1
               DISPLAY WS-MESSAGE(WS-RESULT:1) NO ADVANCING
           END-PERFORM
           DISPLAY ' '.

       LENGTH-OF-MESSAGE.
           MOVE 'COBOL Programming' TO WS-MESSAGE
           DISPLAY 'Length of Message: ' FUNCTION LENGTH(WS-MESSAGE).

       DISPLAY-HELLO.
           DISPLAY 'Hello, User!'.

       DISPLAY-GOODBYE.
           DISPLAY 'Goodbye, User!'.

       DISPLAY-DATE.
           DISPLAY 'Current Date: ' FUNCTION CURRENT-DATE (1:8).
