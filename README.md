# TempusLogic

Project repository for the course Advanced Natural Language Processing.

## Overview

TempusLogic is a comprehensive testing framework designed to evaluate Large Language Models (LLMs) on mathematical reasoning tasks using the GSM8K dataset. The project investigates how LLMs perform when prompted with correct answers, incorrect answers, or no prior examples, providing insights into in-context learning and reasoning capabilities.

## Project Structure

```
TempusLogic/
├── gsm8k/
│   ├── bad_answers.py          # Generates incorrect LLM answers for training data
│   ├── test.py                 # Main testing framework
│   ├── count_token.py          # Token counting utilities
│   ├── test.ipynb              # Interactive Jupyter notebook for experiments
│   ├── data/
│   │   ├── gsm8k_train.parquet           # Original GSM8K training dataset
│   │   └── gsm8k_with_bad_llm_answers.parquet  # Dataset augmented with incorrect answers
│   ├── logs/                   # Test execution logs
│   ├── plots/                  # Visualization outputs
│   └── results/
│       └── scores.csv          # Test results and accuracy metrics
├── requirements.txt
└── README.md
```

## Features

### 1. Dataset Preparation (`bad_answers.py`)

This module prepares the GSM8K dataset by generating intentionally incorrect answers using LLMs:

- **Downloads and processes** the GSM8K training dataset from HuggingFace
- **Generates bad answers** using Google's Gemini or Ollama models with adversarial prompting
- **Validates incorrectness** by numerically comparing generated answers with correct ones
- **Supports multiple generation modes**:
  - `o`: Ollama-based generation
  - `c`: Complex generation with retry logic and exponential backoff
  - `s`: Simple generation without retries
- **Incremental saving** to avoid data loss during long-running generation processes

**Key Features:**
- Retry logic with exponential backoff for API quota management
- Automatic recovery from interruptions (resumes from last saved state)
- Numerical answer validation to ensure generated answers are genuinely incorrect

### 2. Main Testing Framework (`test.py`)

The core testing infrastructure consists of two main classes:

#### `GSM8K_Test` Class

Conducts individual test experiments with configurable parameters:

**Configuration Parameters:**
- `num_bad_examples`: Number of incorrect examples to include in prompts
- `num_tests`: Number of test questions to evaluate
- `model_name`: LLM model to test (e.g., "models/gemini-2.5-flash-lite")
- `retry`: Enable/disable retry logic for API failures
- `llm_eval`: Use LLM-based or numerical answer evaluation
- `test_no`: Test variant (1, 2, or 3)

**Three Test Variants:**

1. **Test 1 (Standard Prompting):**
   - Presents N examples (correct or incorrect) followed by a new question
   - Format: `Q: [question]\nA: [answer]\n\n...`
   - Ends with: "Now correctly answer the next question."

2. **Test 2 (Incomplete Answer Completion):**
   - Same as Test 1, but the final question includes a half-completed incorrect answer
   - Tests the model's ability to recognize and correct incomplete reasoning
   - Format: Final answer is truncated at 50% of the bad answer length

3. **Test 3 (Answer Refinement):**
   - Presents the question with a complete incorrect answer
   - Prompts: "Now correctly answer the next answered question again."
   - Tests the model's ability to identify and correct existing wrong answers

**Evaluation Methods:**

- **Numerical Evaluation**: Extracts and compares the final numerical value using regex
- **LLM-based Evaluation**: Uses a larger model (Gemma-3-12b-it) to assess answer correctness with reasoning

**Test Scenarios:**

Each test runs three experiments:
1. **Bad Prompt**: Uses intentionally incorrect example answers
2. **Good Prompt**: Uses correct example answers
3. **No Prompt**: Direct question without any examples

#### `TestGSM8K` Class

Orchestrates multiple test runs and aggregates results:

- Runs all three test variants sequentially
- Calculates comprehensive accuracy metrics
- Computes deviation scores comparing prompted vs. unprompted performance
- Generates aggregate statistics including standard deviation across tests
- Saves results to CSV for further analysis

**Metrics Calculated:**
- Accuracy for each prompting condition
- Inconclusive results (API errors)
- Accuracy deviation from baseline (no prompt)
- Standard deviation across test variants
- Combined average score

### 3. Token Analysis (`count_token.py`)

Analyzes the token requirements of the GSM8K dataset:

- Uses GPT-2 tokenizer for token counting
- Identifies token usage at various context length thresholds
- Calculates average tokens per Q&A pair (~817 tokens per 5 samples)
- Helps estimate context window requirements for different models

## Installation

```bash
# Clone the repository
git clone https://github.com/Ayush12358/TempusLogic.git
cd TempusLogic

# Install dependencies
pip install -r requirements.txt

# Set up API key
export GENAI_API_KEY="your-google-api-key"
```

## Requirements

- pandas
- google-genai
- ollama (optional, for local model generation)
- huggingface_hub
- transformers (for token counting)
- tqdm (for progress bars)

## Usage

### 1. Generate Bad Answers

```bash
cd gsm8k
python bad_answers.py
```

This will:
- Download the GSM8K dataset if not present
- Generate incorrect answers using the configured LLM
- Save results to `data/gsm8k_with_bad_llm_answers.parquet`
- Log responses to `logs/bad_answers.log`

### 2. Run Tests

```bash
python test.py
```

Configure the test parameters at the bottom of `test.py`:

```python
llms = ["models/gemini-2.5-flash-lite"]  # Models to test
num_bad_examples = [1]                    # Number of example answers in prompts
num_tests = [1]                           # Number of test questions
```

The script will:
- Execute all three test variants
- Log detailed results to `logs/test.log`
- Save aggregate scores to `results/scores.csv`
- Print final standard deviation scores

### 3. Analyze Token Usage

```bash
python count_token.py
```

This provides insights into:
- Token usage across the dataset
- Average tokens per Q&A pair
- Context window requirements for various dataset sizes

## Results Format

The `results/scores.csv` file contains:

- `combined_avg_score`: Average accuracy across all test conditions
- `num_bad_examples`: Number of examples used in prompts
- `num_test_examples`: Number of questions evaluated
- `average_std_dev`: Standard deviation of accuracy deviations (final score metric)

Lower `average_std_dev` indicates more consistent performance across different prompting strategies.

## Key Insights

The testing framework evaluates:

1. **In-Context Learning**: How well models learn from examples (good or bad)
2. **Robustness**: Whether models are misled by incorrect examples
3. **Reasoning Capability**: Ability to identify and correct errors
4. **Consistency**: Performance stability across different prompting approaches

## Logging

All test runs generate detailed logs in the `logs/` directory:
- Individual test execution details
- API retry attempts and errors
- LLM evaluation responses (when enabled)
- Timing information

## Notes

- The framework includes robust error handling with exponential backoff for API quota management
- Caching is implemented to avoid redundant API calls for identical prompts
- Set `DRY_RUN=1` environment variable to test without making actual API calls
- Large-scale testing may incur significant API costs depending on the model and number of tests

## Contributing

This is an academic project for the Advanced Natural Language Processing course. For questions or contributions, please open an issue or pull request.

## License

See LICENSE file for details.
