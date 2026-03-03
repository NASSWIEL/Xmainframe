# XMAiNframe SLM Test Suite

This directory contains COBOL test programs designed to evaluate XMAiNframe's capabilities
across its three core tasks: **COBOL Code Summarization**, **Question Answering**, and
**Multiple Choice Questions**.

## COBOL Test Programs

| File | Description | Key COBOL Features Tested |
|------|-------------|---------------------------|
| `HELLO.cbl` | Basic hello world with date formatting | ACCEPT, STRING, DISPLAY, date intrinsics |
| `CUSTMGMT.cbl` | Customer management with indexed files | VSAM indexed I/O, EVALUATE, 88-levels, COMP-3, report generation |
| `PAYROLL.cbl` | Payroll calculation batch processor | Tax bracket tables, PERFORM VARYING, COMPUTE, OCCURS, arithmetic |
| `BANKTXN.cbl` | Banking transaction processor | CICS-style flow, DB2 simulation, audit trails, financial logic |
| `INVNTORY.cbl` | Inventory management system | SEARCH/SEARCH ALL, SORT, INSPECT, STRING/UNSTRING, CALL |
| `BATCHJCL.cbl` | JCL-driven batch processing | ACCEPT, RETURN-CODE, header/detail/trailer pattern, hash totals |

## Test Prompts

### Task 1: COBOL Code Summarization
Feed each `.cbl` file to the model and ask it to summarize what the program does.

**Example prompts:**
- "Summarize the following COBOL program:"
- "What does this COBOL code do? Explain its purpose and main logic."
- "Provide a detailed summary of this COBOL program's functionality."

### Task 2: Question Answering
Use these questions against the COBOL programs:

**CUSTMGMT.cbl questions:**
1. What file organization is used for the customer file?
2. How does the program determine if a customer's balance is over their credit limit?
3. What happens in paragraph 2100-EVALUATE-CUSTOMER?
4. What error handling exists when opening the customer file?

**PAYROLL.cbl questions:**
1. How is overtime pay calculated for hourly employees?
2. How does the tax bracket table work for federal tax calculation?
3. What are the three types of employee pay types supported?
4. How are department totals accumulated?

**BANKTXN.cbl questions:**
1. What validation checks are performed before processing a withdrawal?
2. How does the transfer logic handle the two-account update?
3. What are the possible response codes and what do they mean?
4. What information is captured in the audit trail?

**INVNTORY.cbl questions:**
1. How does the program determine which items need reordering?
2. Explain the difference between SEARCH and SEARCH ALL as used in this program.
3. How is the SORT verb used in this program?
4. What does the INSPECT statement do with the inventory description?

**BATCHJCL.cbl questions:**
1. How does the program validate input file integrity?
2. What is the purpose of the hash total check?
3. What return codes does the program set and under what conditions?
4. How does the header/detail/trailer pattern work?

### Task 3: Multiple Choice Questions

**Q1:** In COBOL, what does `PIC S9(7)V99 COMP-3` define?
- A) A 7-digit alphanumeric field
- B) A packed decimal signed numeric field with 7 integer and 2 decimal digits
- C) A binary integer field
- D) A floating-point numeric field
- **Answer: B**

**Q2:** What is the purpose of the `88-level` condition name in COBOL?
- A) To define a subroutine
- B) To declare a file status variable
- C) To associate a data name with specific values for conditional testing
- D) To create a pointer variable
- **Answer: C**

**Q3:** In VSAM file processing, what does `ACCESS MODE IS DYNAMIC` allow?
- A) Only sequential access
- B) Only random access
- C) Both sequential and random access within the same program
- D) Parallel file access across multiple programs
- **Answer: C**

**Q4:** What is the purpose of the `EVALUATE TRUE` statement in COBOL?
- A) To evaluate a mathematical expression
- B) To implement a case/switch-like structure checking multiple conditions
- C) To validate input data
- D) To test if a file is open
- **Answer: B**

**Q5:** What does `PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10` do?
- A) Executes a paragraph once
- B) Loops through a paragraph with WS-IDX incrementing from 1 to 10
- C) Defines a variable with values 1 through 10
- D) Creates 10 copies of a paragraph
- **Answer: B**

**Q6:** In COBOL, what is the purpose of `RECORDING MODE IS F`?
- A) The file contains fixed-length records
- B) The file is formatted as a report
- C) The file uses floating-point encoding
- D) The file is a FIFO queue
- **Answer: A**

**Q7:** What does the COBOL `INSPECT` verb do?
- A) Checks for file errors
- B) Validates numeric fields
- C) Counts and/or replaces characters within a data item
- D) Inspects program flow for debugging
- **Answer: C**

**Q8:** In a mainframe batch COBOL program, what is the `RETURN-CODE` special register used for?
- A) Returning values from PERFORM statements
- B) Setting the condition code passed back to JCL for step-level condition checking
- C) Counting the number of records processed
- D) Storing the file status after I/O operations
- **Answer: B**

**Q9:** What is the purpose of the `GOBACK` statement versus `STOP RUN` in COBOL?
- A) They are identical in function
- B) GOBACK returns control to the calling program or OS; STOP RUN always terminates the run unit
- C) GOBACK is for batch programs; STOP RUN is for online programs
- D) GOBACK saves program state; STOP RUN does not
- **Answer: B**

**Q10:** In the context of mainframe COBOL, what does a `COPY` statement do?
- A) Copies a file from one dataset to another
- B) Duplicates a variable's value
- C) Includes source code from a copybook library member at compile time
- D) Creates a backup of the program
- **Answer: C**

## Usage with XMAiNframe

```python
from transformers import AutoTokenizer, AutoModelForCausalLM

tokenizer = AutoTokenizer.from_pretrained("Fsoft-AIC/XMAiNframe-instruct-7b")
model = AutoModelForCausalLM.from_pretrained("Fsoft-AIC/XMAiNframe-instruct-7b")

# Example: COBOL Code Summarization
with open("tests/cobol/CUSTMGMT.cbl", "r") as f:
    cobol_code = f.read()

messages = [
    {"from": "system", "value": "You are a helpful mainframe assistant."},
    {"from": "human", "value": f"Summarize the following COBOL program:\n\n{cobol_code}"}
]

inputs = tokenizer.apply_chat_template(
    messages, add_generation_prompt=True, return_tensors="pt"
).to(model.device)

outputs = model.generate(
    inputs, max_new_tokens=512, do_sample=False,
    top_k=50, top_p=0.95, num_return_sequences=1,
    eos_token_id=tokenizer.eos_token_id
)

print(tokenizer.decode(outputs[0][len(inputs[0]):], skip_special_tokens=True))
```
