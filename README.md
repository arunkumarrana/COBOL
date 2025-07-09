# COBOL
Cobol Learning
Install:
1. /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
2. echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zshrc\nsource ~/.zshrc
3. brew install gnu-cobol
4. brew --version
5. cobc --version
Run:
cobc -x -free user_input.cbl
./user_input

==================================================================================================================================================
Blank Space:
Area A (8-11): Structure (divisions, sections, level numbers).
Area B (12-72): Detailed code (variables, logic).
Column 7: Controls line type (comment, debug, continuation).

1. Columns 1-6: Sequence Numbers (Optional)
Used for line numbers.
Example:
000100 IDENTIFICATION DIVISION. *> Columns 1-6: "000100" (rarely used today)

2. Column 7: Indicator Area
Blank space: Normal code line.
* or /: Comment line.
Example:
    *> This is a comment (Column 7 = '*')
     MOVE 100 TO X. *> Normal code (Column 7 = space)

3. Columns 8-11 (Area A)
Required for:
Division headers (IDENTIFICATION DIVISION).
Section headers (DATA DIVISION).
Paragraph names (PROCEDURE DIVISION).
Level numbers (01, 05, 77, etc.).
Example:
      IDENTIFICATION DIVISION. *> Starts at Column 8
      PROGRAM-ID. HELLO.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  MY-VAR PIC X(10). *> "01" starts at Column 8

4. Columns 12-72 (Area B)
For all other code: Variable declarations, statements, etc.
Must start at Column 12 or later.
Example:
       01  CUSTOMER-RECORD.          *> "01" in Area A
           05 NAME    PIC X(30).     *> "05" indented to Column 12 (Area B)
           05 AGE     PIC 9(3).      *> Aligns with "05" above

5. Columns 73+ (Ignored)

Example:
000100 IDENTIFICATION DIVISION. *> Columns 1-6: Optional sequence
000200 PROGRAM-ID. SAMPLE.      *> Column 7: Blank (code)
       DATA DIVISION.           *> Column 8-11: "DATA" starts
       WORKING-STORAGE SECTION.
       01  USER-DATA.           *> "01" in Area A
           05 USER-NAME PIC X(20). *> "05" indented to Area B
           05 USER-AGE  PIC 9(3).
       PROCEDURE DIVISION.
       MAIN.
           MOVE "ALICE" TO USER-NAME *> Area B for statements
           MOVE 25     TO USER-AGE
           DISPLAY USER-NAME ", " USER-AGE.

===================================================================================================================================================
Use of level numbers:
In COBOL, the level numbers (like 01, 05, 10, etc.) serve critical purposes in defining the hierarchy, grouping, and accessibility of data fields.
01 = Root level (file records, major groups)
02-49 = Nested fields (lower = higher in hierarchy)
77 = Independent variables
66 = Field renaming
88 = Boolean conditions
01  INVOICE-RECORD.
    05  INVOICE-NUMBER    PIC 9(10).
    05  CUSTOMER-INFO.
        10  CUST-ID       PIC 9(5).
        10  CUST-NAME     PIC X(30).
    05  LINE-ITEMS OCCURS 5 TIMES.
        10  ITEM-CODE     PIC X(10).
        10  ITEM-PRICE    PIC 9(5)V99.
    77  TAX-RATE          PIC V999 VALUE 0.08. *> Standalone
    66  DISCOUNT-FIELDS RENAMES ITEM-PRICE(1) THRU ITEM-PRICE(3).
    88  HAS-DISCOUNT     VALUE 'Y' 'N'. *> Condition

===================================================================================================================================================
Data Type:
Data Type	    PIC Clause	Example	            Use Case
Integer	        9(n)	    PIC 9(5)	        IDs, counters
Signed Int	    S9(n)	    PIC S9(4)	        Negative values
Decimal	        9(n)V9(m)	PIC 9(3)V99	        Prices, percentages
Alphanumeric	X(n)	    PIC X(20)	        Names, addresses
Boolean	        88 + PIC X	88 IS-ACTIVE 'Y'	Flags
Edited	        $$,$$9.99	PIC $Z(4).99	    Formatted output
Binary	        9(n) COMP	PIC 9(4) COMP	    High-performance math

Ex:
DATA DIVISION.
WORKING-STORAGE SECTION.
01  CUSTOMER-RECORD.
    05  CUST-ID      PIC 9(5).               *> Numeric (e.g., 12345)
    05  CUST-NAME    PIC X(30).              *> Text (e.g., "John Doe")
    05  ORDER-DETAILS.
        10  ITEM-NUM PIC 9(3).               *> 3-digit item code
        10  PRICE    PIC 9(5)V99 VALUE 0.00. *> Decimal (e.g., 100.50)
    05  IS-VIP       PIC X.
        88  VIP      VALUE 'Y'.              *> Boolean flag
    05  TAX          PIC $$,$$9.99.          *> Edited (e.g., "$1,000.00")
77  COUNTER          PIC 9(3) COMP.          *> Binary counter

===================================================================================================================================================
Code reusability : In COBOL, code reusability is achieved through subroutines (PERFORM), copybooks (COPY statement), and subprograms (CALL statement). Let’s explore each method with examples.
Execution: 
cobc -x SALES-CALCULATOR.cbl
./SALES-CALCULATOR

cobc -x SALES-CALCULATOR2.cbl
./SALES-CALCULATOR2

cobc -x SALES-CALCULATOR3.cbl
cobc -m DISCOUNT-SUB.cbl
./SALES-CALCULATOR3
===================================================================================================================================================
