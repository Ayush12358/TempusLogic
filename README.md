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

tempuslogic/ ├── data/                   # Contains the data files │   ├── dyads_gpt5.txt      # GPT-5 dyad examples │   └── triads_gpt5.txt     # GPT-5 triad examples ├── eval_utils.py           # Utilities for parsing and evaluating ├── dyad_eval.py            # Evaluation script for dyads ├── req.py                  # API request handler ├── keys.py                 # API keys (keep private!) ├── dyads.py                # Dyad generator script └── triads.py               # Triad generator script

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
