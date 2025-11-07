# TempusLogic
<<<<<<< HEAD

TempusLogic bundles two complementary evaluation tracks for advanced natural language processing research:

1. Logical reasoning diagnostics based on custom dyad and triad arguments.
2. Mathematical reasoning stress tests on the GSM8K benchmark with adversarial prompting.

The repository is organised so you can run either track independently or compare results across both to understand model robustness to context edits and misleading exemplars.

## Repository Layout

```text
TempusLogic/
├── data/                          # Shared data assets
├── dyad_triad_gen.py              # Generator for dyad/triad problem sets
├── eval.py                        # Evaluation script for logical reasoning
├── eval_utils.py                  # Parsing and scoring helpers for dyads/triads
├── gsm8k/                         # GSM8K robustness experiments
│   ├── bad_answers.py
│   ├── count_token.py
│   ├── test.py
│   ├── data/
│   ├── logs/
│   ├── plots/
│   └── results/
├── parse_utils.py                 # Text parsing utilities shared by experiments
├── req.py                         # API request helpers
└── requirements.txt
```

## Installation

```bash
pip install -r requirements.txt
```

Set the required API keys as environment variables before running any experiment:

```bash
# PowerShell example
$env:OPENROUTER = "your-openrouter-api-key"
$env:GROQ = "your-groq-api-key"
$env:GENAI_API_KEY = "your-google-genai-api-key"
```

## Logical Reasoning (Dyads & Triads)

This track probes how well a model preserves conclusions when the supporting context is modified.

### Concepts

- **Dyad**: Two statements, one question, answer in {yes, no, inconclusive}.
- **Triad**: Three statements, one question, answer in {yes, no, inconclusive}.
- **Modified variants**: Additional statements alter the context while keeping the question identical.

### Data Files

- `data/dyads.txt`, `data/dyads_gpt5.txt`
- `data/triads.txt`, `data/triads_gpt5.txt`

Entries follow the template:

```text
**Dyad N**
1. Statement 1
2. Statement 2
**Question:** Does ... ?
**Answer:** Yes/No/Inconclusive

**Modified Dyad N**
1. Additional statement ...
2. Additional statement ...
**Question:** (same as above)
**Answer:** Yes/No/Inconclusive
```

### Generate New Problems

```bash
python dyad_triad_gen.py
```

- Uses the configured LLM (default: GPT-oss-120b) to synthesise fresh dyads/triads.
- Adds modified counterparts that perturb the original context while keeping the question fixed.

### Evaluate a Model

```bash
python eval.py
```

- Reads problem sets from `data/`.
- Sends each prompt through the API gateway defined in `req.py`.
- Computes accuracy, precision, recall, and confusion matrices for original vs. modified arguments.
- Prints metrics to stdout; redirect to a file if needed for record keeping.

Typical findings (example using Kimi K2 instruct 1T) highlight that modified arguments reduce accuracy, especially on triads where recall drops sharply—suggesting sensitivity to contextual shifts.

## GSM8K Prompt Robustness Tests

This track inspects how GSM8K mathematical reasoning performance changes when prompts include misleading exemplars.

### Components

- `gsm8k/bad_answers.py`: Generates intentionally wrong solutions for training examples.
- `gsm8k/test.py`: Runs comparative evaluations with bad prompts, good prompts, and zero-shot queries.
- `gsm8k/count_token.py`: Estimates token budgets for large-context experiments.

### Prepare Adversarial Examples

```bash
cd gsm8k
python bad_answers.py
```

- Downloads the GSM8K split from Hugging Face on first run.
- Queries the selected model (e.g. `models/gemini-2.5-flash-lite` or an Ollama-hosted alternative) with a system prompt that demands incorrect answers.
- Verifies numeric disagreement with the ground-truth response before accepting a bad answer.
- Persists progress to `data/gsm8k_with_bad_llm_answers.parquet` for recoverability.

### Run the Robustness Suite

Edit the configuration block at the bottom of `gsm8k/test.py`, then execute:

```bash
python gsm8k/test.py
```

The script:

1. Builds prompts containing `num_bad_examples` exemplars (either incorrect or correct).
2. Evaluates `num_tests` held-out questions under three conditions:
    - Bad prompt (misleading exemplars).
    - Good prompt (correct exemplars).
    - No prompt (question only).
3. Supports three prompt templates:
    - **Test 1**: Standard few-shot prompting.
    - **Test 2**: Forces the model to complete a truncated incorrect answer.
    - **Test 3**: Asks the model to re-answer a question already answered incorrectly.
4. Scores outputs via numeric comparison or by delegating to a stronger LLM (`models/gemma-3-12b-it`) for qualitative checking.
5. Logs detailed traces in `gsm8k/logs/` and appends summary statistics to `gsm8k/results/scores.csv`.

### Token Accounting

```bash
python gsm8k/count_token.py
```

- Uses the GPT-2 tokenizer to approximate token counts.
- Reports milestone context sizes (4k, 8k, 16k, 32k, 64k, 128k, 256k, 512k, 1M tokens) to guide prompt design.
- Provides average tokens per GSM8K Q&A pair (~817 tokens per five samples in the supplied dataset).

## Notes & Tips

- Enable retry logic (`retry=True`) when using rate-limited APIs; exponential backoff is implemented by default.
- Set `DRY_RUN=1` to exercise the pipelines without incurring API calls during development.
- Keep API keys secure—`keys.py` is intentionally excluded from version control.
- Large runs can be expensive; start with small `num_tests` to validate settings.

## Contributing

Contributions are welcome. Please open an issue or pull request describing the change and the evaluation track it targets.

## License

See `LICENSE` for full details.
=======
Project repository for the course Advanced Natural Language Processing.

## Codetext Test

To test the model for context rot on code-related tasks, our focus has been to design the syntax for a toy language, and feed it increasingly large code snippets in this written in this language. This draws the models focus onto the context we provide, since the code examples will not be part of its training data, and as the code sizes grow this can prove an effective test for context rot.

However, at the start of this project we were unsure how capable models would be when faced with such a task. Since we did not know how hard we can even push them in the first place, we spent the last month trying to gauge their understanding of code written in existing languages. The L-99 problem set is a set of 99 basic problems for people picking up lisp for the first time. Many variations of it exist for other languages as well. We have chosen to test the model on this set of problems, in Lisp and in Python. The idea is simple: we feed the model the code, which grows increasingly complex from problem 1 to 99, and over this set of problems we test whether the model can gauge what the code is trying to accomplish. Lisp and Python were chosen because while Python would feature prominently in the model's training data, Lisp would feature much less substantially, providing a good mix of model familiarity with the code.
>>>>>>> origin/codetext
