# COBOL Learning

A beginner-friendly guide to setting up COBOL on macOS, understanding its structure, and running basic COBOL programs. This repo includes examples and references to key COBOL concepts such as divisions, data types, level numbers, and code reuse.

---

## 🛠 Installation (macOS)

1. **Install Homebrew**:
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Add Homebrew to your shell**:
   ```bash
   echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zshrc
   source ~/.zshrc
   ```

3. **Install GNU COBOL**:
   ```bash
   brew install gnu-cobol
   ```

4. **Verify Installation**:
   ```bash
   brew --version
   cobc --version
   ```

---

## ▶️ Running COBOL Code

Compile and run a COBOL program:

```bash
cobc -x user_input.cbl
./user_input
```

---

## 🧱 COBOL Code Structure

COBOL follows a strict column-based layout:

| Area         | Columns     | Purpose                                      |
|--------------|-------------|----------------------------------------------|
| Sequence     | 1–6         | Optional line numbers                        |
| Indicator    | 7           | `*`, `/`, or space – for comments or code    |
| Area A       | 8–11        | Divisions, section names, level numbers      |
| Area B       | 12–72       | Executable statements and declarations       |
| Ignored Area | 73+         | Compiler ignores content                     |

### Example:

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. SAMPLE.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  USER-DATA.
            05 USER-NAME PIC X(20).
            05 USER-AGE  PIC 9(3).
        PROCEDURE DIVISION.
        MAIN.
            MOVE "ALICE" TO USER-NAME
            MOVE 25 TO USER-AGE
            DISPLAY USER-NAME ", " USER-AGE.
```

---

## 🔢 Use of Level Numbers

Level numbers define the data hierarchy:

- `01` – Top-level data items
- `02–49` – Nested/grouped items
- `66` – Field renaming
- `77` – Standalone variables
- `88` – Conditional/Boolean fields

### Example:

```cobol
01 INVOICE-RECORD.
    05 INVOICE-NUMBER PIC 9(10).
    05 CUSTOMER-INFO.
        10 CUST-ID    PIC 9(5).
        10 CUST-NAME  PIC X(30).
    05 LINE-ITEMS OCCURS 5 TIMES.
        10 ITEM-CODE  PIC X(10).
        10 ITEM-PRICE PIC 9(5)V99.
77 TAX-RATE PIC V999 VALUE 0.08.
66 DISCOUNT-FIELDS RENAMES ITEM-PRICE(1) THRU ITEM-PRICE(3).
88 HAS-DISCOUNT VALUE 'Y' 'N'.
```

---

## 🧬 Data Types in COBOL

| Type         | PIC Clause       | Example             | Use Case              |
|--------------|------------------|---------------------|------------------------|
| Integer      | `9(n)`           | `PIC 9(5)`          | IDs, counters          |
| Signed Int   | `S9(n)`          | `PIC S9(4)`         | Allow negatives        |
| Decimal      | `9(n)V9(m)`      | `PIC 9(3)V99`       | Currency, rates        |
| Alphanumeric | `X(n)`           | `PIC X(20)`         | Names, addresses       |
| Boolean      | `88` + `PIC X`   | `88 ACTIVE 'Y'`     | Flags/conditions       |
| Edited       | Format + picture | `PIC $$,$$9.99`     | Human-readable output  |
| Binary       | `COMP`           | `PIC 9(4) COMP`     | Optimized for storage  |

---

## 🔁 Code Reusability in COBOL

COBOL encourages modular code through:

- **PERFORM** – Subroutines
- **COPY** – Copybooks (reusable blocks)
- **CALL** – External subprograms

### Example Execution:

```bash
cobc -x SALES-CALCULATOR.cbl
./SALES-CALCULATOR

cobc -x SALES-CALCULATOR2.cbl
./SALES-CALCULATOR2

cobc -x SALES-CALCULATOR3.cbl
cobc -m DISCOUNT-SUB.cbl
./SALES-CALCULATOR3
```

---

## 📁 Files in this Project

| File                    | Purpose                          |
|-------------------------|----------------------------------|
| `user_input.cbl`        | Basic program with input/output  |
| `SALES-CALCULATOR*.cbl` | Examples for reusability         |
| `DISCOUNT-SUB.cbl`      | Subprogram module                |

---

## 📚 Resources

- [GNU COBOL Manual](https://open-cobol.sourceforge.io/)
- [COBOL Column Rules – IBM Docs](https://www.ibm.com/docs/)
- [COBOL Programming Tutorial](https://www.tutorialspoint.com/cobol/)

---

## ✅ License

This project is for learning purposes and distributed freely for educational use.
