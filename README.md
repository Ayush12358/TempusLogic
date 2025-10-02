# TempusLogic - Logical Reasoning Evaluator

TempusLogic is a framework for evaluating language models on logical reasoning tasks using structured "dyad" and "triad" problems, measuring how well models handle logical inferences and contextual modifications.

## Core Concepts

### Dyads

A **dyad** is a logical reasoning problem consisting of:
- 2 statements
- 1 question
- An answer that can be "yes", "no", or "inconclusive"

Each dyad also includes a modified version that:
- Keeps the same question
- Adds 2 additional statements
- Changes at least one aspect of the context
- May or may not change the logical conclusion

Example:
Original Dyad
All squares have four equal sides.
Figure A is a square. Question: Does Figure A have four equal sides? Answer: Yes
Modified Dyad
All squares have four equal sides.
Figure A is a square.
Figure A is painted red.
The paint used shrinks when dried. Question: Does Figure A have four equal sides? Answer: Yes

### Triads

A **triad** is similar but with:
- 3 statements
- 1 question
- An answer that can be "yes", "no", or "inconclusive"

The modified version:
- Adds 3 additional statements
- Changes at least one aspect of the context
- May or may not change the logical conclusion

Example:
Original Triad
All emeralds are gemstones.
No gemstone is a metal.
This object is an emerald. Question: Is the object a metal? Answer: No
Modified Triad
All emeralds are precious stones.
Some precious stones are used in jewelry.
This object is an emerald. New conclusion: Inconclusive

## Project Structure

tempuslogic/
├── data/
# Contains the data files
│
├── dyads_gpt5.txt
# GPT-5 dyad examples
│
└── triads_gpt5.txt
# GPT-5 triad examples
├── eval_utils.py
# Utilities for parsing and evaluating
├── dyad_eval.py
# Evaluation script for dyads
├── req.py
# API request handler
├── keys.py
# API keys (keep private!)
├── dyads.py
# Dyad generator script
└── triads.py
# Triad generator script

## How to Run

### Setup

1. Make sure you have the required dependencies:
   bash
   pip install requests
Add your API keys to keys.py:
- OPENROUTER = "your-openrouter-api-key"
- GROQ = "your-groq-api-key"
#### Generate New Logical Problems
To generate new dyads and triads, run:
```python dyad_triad_gen.py```
Evaluate Model Performance
Run the evaluation script on dyads:
`python eval.py`
The Generation of these problems was done with GPT-oss-120b
This will:
- Load existing dyad problems from data/dyads_gpt5.txt and data/triads_gpt5.txt
- Query the model for answers through the API
- Compare predicted answers with reference answers
- Calculate metrics (accuracy, precision, recall)
- Generate confusion matrices
- Evaluation Metrics
- The evaluation includes:
- Overall accuracy
- Per-class precision and recall (for “yes”, “no”, “inconclusive”)
- Macro averages
- Confusion matrices
Results are displayed separately for original and modified problems to assess how well models handle contextual changes.

## Adding New Problems
To add new problems:
Format them according to the templates in the data files:
```
**Dyad N**
1. statement 1
2. statement 2
**Question:** Question?
**Answer:** Yes/No/Inconclusive

**Modified Dyad 21** (atleast one statement here must be related to one statement in the original dyad)
1. Statement 3
2. Statement 4
**Question:** Question? (identical to the previous one) (optional)
**Answer:** Yes/No/Inconclusive

---
```
Place them in the appropriate data file
Run the evaluation

## Results

Results will be printed to the console and can be redirected to a file for further analysis.
The model used was Kimi K2 instruct 1T. Heree same , we see that the answers to the modified arguments are visibly worse than the original arguments, showing that the model struggles with contextual changes.

Furthermore, despite the accuracy of the modified arguments being the same for dyads and triads, the recall is worse for triads, indicating that the model struggles more with triads when the context is changed.

=== Dyads Original ===
Accuracy: 0.9667
Macro Precision: 0.9833
Macro Recall:    0.9667

Per-class:
  yes           P=0.9500 R=1.0000 Support=19
  no            P=1.0000 R=0.9000 Support=10
  inconclusive  P=1.0000 R=1.0000 Support=1

Confusion Matrix (rows=true, cols=pred):
            YES    NO   INC
       YES    19     0     0
        NO     1     9     0
       INC     0     0     1

=== Dyads modified ===
Accuracy: 0.6000
Macro Precision: 0.4563
Macro Recall:    0.4397

Per-class:
  yes           P=0.7500 R=0.8333 Support=18
  no            P=0.3333 R=0.2000 Support=5
  inconclusive  P=0.2857 R=0.2857 Support=7

Confusion Matrix (rows=true, cols=pred):
            YES    NO   INC
       YES    15     1     2
        NO     1     1     3
       INC     4     1     2

=== Triads Original ===
Accuracy: 0.8333
Macro Precision: 0.5637
Macro Recall:    0.6140

Per-class:
  yes           P=0.9412 R=0.8421 Support=19
  no            P=0.7500 R=1.0000 Support=9
  inconclusive  P=0.0000 R=0.0000 Support=2

Confusion Matrix (rows=true, cols=pred):
            YES    NO   INC
       YES    16     2     1
        NO     0     9     0
       INC     1     1     0

=== Triads modified ===
Accuracy: 0.6000
Macro Precision: 0.5287
Macro Recall:    0.3636

Per-class:
  yes           P=0.0000 R=0.0000 Support=2
  no            P=1.0000 R=0.0909 Support=11
  inconclusive  P=0.5862 R=1.0000 Support=17

Confusion Matrix (rows=true, cols=pred):
            YES    NO   INC
       YES     0     0     2
        NO     0     1    10
       INC     0     0    17
