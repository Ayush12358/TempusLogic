# **smol Language Syntax**

This document outlines the observed syntax of the smol language based on the provided code sample. The syntax appears to be characterized by inverted operators and non-standard keyword choices.

### **1\. Comments**

Single-line comments are initiated with the @ symbol. All text following the @ on that line is ignored by the parser.  
@ This is a comment.

### **2\. Function Definition**

Functions are defined with a name, followed by a parenthesized return type, and a code block enclosed in square brackets \[\].  
main (bool) \[  
    @ Function body  
\]

### **3\. Code Blocks & Statements**

* **Blocks:** Code blocks are delimited by square brackets \[...\]. This applies to function bodies and control flow structures.  
* **Statement Terminator:** All statements (assignments, returns) must be terminated with a colon :.

\[  
    1 \-\> a:  
    return false:  
\]

### **4\. Variable Assignment**

Assignment uses an inverted, infix operator \-\>. The value (or expression) is on the left, and the variable receiving the value is on the right.  
Syntax:  
\_expression\_ \-\> \_variable\_name\_:  
**Example:**  
@ Assigns the value 1 to variable 'a'  
1 \-\> a:

@ Equivalent to b \= b \- 1  
1 \- b \-\> b:

### **5\. Control Flow**

#### **if / though\_if**

Conditional logic is handled by if and though\_if (which functions as an else if). There is no else or elseif keyword.

* The condition is enclosed in curly braces {}.  
* The executable block is enclosed in square brackets \[\].

if { 0 \== b } \[  
    return truth:  
\]  
though\_if { 0 \> b } \[  
    return false:  
\]

#### **doth / while Loop**

The language implements a while loop structure using the doth keyword. This is a standard while loop, not a do-while loop (the condition is checked before the first execution).

* The doth keyword is followed by an executable block \[\].  
* The while keyword follows the block, with its condition enclosed in {}.

@ This loop continues as long as a \>= 0  
doth \[  
   1 \- a \-\> a:  
\] while { 0 \=\> a }

### **6\. Conditions & Operators**

Conditions are always enclosed in curly braces {}. The observed operators include standard and non-standard forms.

* **Equality:** \==  
  * { 0 \== b }  
* **Greater Than:** \>  
  * { 0 \> b }  
* **Inverted Greater/Equal:** \=\>  
  * The syntax X \=\> Y is interpreted as Y \>= X.  
  * { 0 \=\> a } is equivalent to a \>= 0\.

### **7\. Keywords and Literals**

Based on the sample, the following keywords and literals are observed:

* **Keywords:**  
  * main  
  * bool (type)  
  * doth  
  * while  
  * if  
  * though\_if  
  * return  
* **Boolean Literals:**  
  * truth (equivalent to true)  
  * false