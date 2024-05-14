import re

cobol_code = """
IDENTIFICATION DIVISION.
PROGRAM-ID. SUPPLY-CHAIN-SYSTEM.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 PRODUCT-RECORD.
   05 PRODUCT-ID         PIC X(10).
   05 PRODUCT-NAME       PIC X(30).
   05 PRODUCT-QUANTITY   PIC 9(8).
   05 PRODUCT-PRICE      PIC 9(6)V99.

01 WAREHOUSE-RECORD.
   05 WAREHOUSE-ID       PIC X(10).
   05 WAREHOUSE-NAME     PIC X(30).
   05 WAREHOUSE-LOCATION PIC X(50).

01 ORDER-RECORD.
   05 ORDER-ID           PIC X(10).
   05 PRODUCT-ID         PIC X(10).
   05 ORDER-QUANTITY     PIC 9(8).
   05 ORDER-DATE         PIC 9(8).

01 PRODUCT-TABLE.
   05 PRODUCT-ENTRY OCCURS 100 TIMES.
      10 PRODUCT-ID         PIC X(10).
      10 PRODUCT-NAME       PIC X(30).
      10 PRODUCT-QUANTITY   PIC 9(8).
      10 PRODUCT-PRICE      PIC 9(6)V99.

01 WAREHOUSE-TABLE.
   05 WAREHOUSE-ENTRY OCCURS 10 TIMES.
      10 WAREHOUSE-ID       PIC X(10).
      10 WAREHOUSE-NAME     PIC X(30).
      10 WAREHOUSE-LOCATION PIC X(50).

01 ORDER-TABLE.
   05 ORDER-ENTRY OCCURS 1000 TIMES.
      10 ORDER-ID           PIC X(10).
      10 PRODUCT-ID         PIC X(10).
      10 ORDER-QUANTITY     PIC 9(8).
      10 ORDER-DATE         PIC 9(8).

01 WS-REPORT-DATE PIC X(10).
01 WS-REPORT-TIME PIC X(8).
01 WS-REPORT-FILE-NAME PIC X(50).
01 WS-ORDER-TOTAL PIC 9(10)V99 VALUE 0.
01 WS-QUANTITY-STRING PIC X(8).
01 WS-ORDER-QUANTITY PIC 9(8).
01 WS-QUANTITY-DISPLAY PIC X(8).
"""

# Regular expression to match COBOL variable declarations
pattern = re.compile(r'^\s*(\d{2})\s+([\w-]+)\s+PIC\s+([X9]+(?:\(\d+\))?(?:V9+\(\d+\))?)(?:\s+VALUE\s+[\w\.\-]+)?\.', re.MULTILINE)

matches = pattern.findall(cobol_code)

for match in matches:
    level, var_name, pic_clause = match
    print(f"Level: {level}, Variable: {var_name}, PIC: {pic_clause}")
