# COBOL Learning Guide

![COBOL Logo](https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Cobol_logo.svg/1200px-Cobol_logo.svg.png)

A comprehensive guide to getting started with COBOL programming, including installation, basic syntax, and practical examples.

## Table of Contents
1. [Installation](#installation)
2. [Running Programs](#running-programs)
3. [Basic Syntax](#basic-syntax)
4. [Data Types](#data-types)
5. [Code Structure](#code-structure)
6. [Example Program](#example-program)
7. [Resources](#resources)

## Installation <a name="installation"></a>

### macOS/Linux
```bash
# Install Homebrew (if not installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Add Homebrew to PATH
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zshrc
source ~/.zshrc

# Install GNU COBOL
brew install gnu-cobol

# Verify installation
cobc --version

Running Programs <a name="running-programs"></a>
bash
# Compile a program
cobc -x -free program.cbl

# Run the compiled program
./program
Basic Syntax <a name="basic-syntax"></a>
Divisions
cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 GREETING PIC X(20) VALUE "Hello, World!".

PROCEDURE DIVISION.
MAIN.
    DISPLAY GREETING
    STOP RUN.
Column Rules
Columns 1-6: Line numbers (optional)

Column 7: Comment indicator (*)

Columns 8-11 (Area A): Division/section headers

Columns 12-72 (Area B): Program code

Columns 73+: Ignored

Data Types <a name="data-types"></a>
Type	Format	Example
Integer	9(n)	PIC 9(5)
Decimal	9(n)V9(m)	PIC 9(3)V99
Text	X(n)	PIC X(30)
Boolean	88 level	88 IS-VALID VALUE 'Y'
Date	9(8)	PIC 9(8) (YYYYMMDD)
Code Structure <a name="code-structure"></a>
cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 CUSTOMER-RECORD.
   05 CUST-ID    PIC 9(5).
   05 CUST-NAME  PIC X(30).
   05 BALANCE    PIC 9(5)V99.

PROCEDURE DIVISION.
MAIN.
   MOVE 12345 TO CUST-ID
   MOVE "JOHN DOE" TO CUST-NAME
   MOVE 100.50 TO BALANCE
   DISPLAY "Customer: " CUST-NAME " Balance: " BALANCE
   STOP RUN.
Example Program <a name="example-program"></a>
Save this as hello.cbl:

cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-NAME PIC X(20).
       01 GREETING PIC X(30) VALUE "Hello, ".

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Enter your name: " WITH NO ADVANCING
           ACCEPT USER-NAME
           DISPLAY GREETING USER-NAME
           STOP RUN.
Compile and run:

bash
cobc -x hello.cbl
./hello
Resources <a name="resources"></a>
