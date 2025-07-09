       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-CALCULATOR2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY 'discount.cpy'. *> Imports data definitions

       PROCEDURE DIVISION.
           MOVE 100.00 TO PRICE.
           MOVE 0.10 TO DISCOUNT-RATE.
           COMPUTE FINAL-PRICE = PRICE - (PRICE * DISCOUNT-RATE).
           DISPLAY "Final Price: $" FINAL-PRICE.
           STOP RUN.
