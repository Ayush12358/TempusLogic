import os
import logging
from typing import Optional, Dict, List, Tuple, Any
from dataclasses import dataclass
from google import genai
import pandas as pd
import time
import re
from tqdm import tqdm

@dataclass
class GSMConfig:
    """Configuration class for GSM8K testing."""
    num_bad_examples: int = 25
    num_tests: int = 25
    model_name: str = "models/gemma-3-4b-it"
    log_filename: str = 'logs/test1.log'
    retry: bool = True
    llm_eval: bool = False
    max_retries: int = 5
    data_path: str = 'data/gsm8k_with_bad_llm_answers.parquet'
    evaluation_model: str = 'models/gemma-3-12b-it'
    
    def __post_init__(self):
        """Validate configuration after initialization."""
        if not os.getenv("GENAI_API_KEY"):
            raise ValueError("GENAI_API_KEY environment variable not set")
        
        if self.num_bad_examples <= 0:
            raise ValueError("num_bad_examples must be positive")
        
        if self.num_tests <= 0:
            raise ValueError("num_tests must be positive")
        
        if self.max_retries < 1:
            raise ValueError("max_retries must be at least 1")
        
        # Create logs directory if it doesn't exist
        log_dir = os.path.dirname(self.log_filename)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir)

class GSM8K_Test_1:
    """Main class for testing GSM8K dataset with bad examples."""
    
    def __init__(self, config: Optional[GSMConfig] = None, **kwargs):
        """Initialize with configuration object or individual parameters.
        
        Args:
            config: GSMConfig object with all settings
            **kwargs: Individual configuration parameters (legacy support)
        """
        # Handle legacy parameter passing
        if config is None:
            config = GSMConfig(
                num_bad_examples=kwargs.get('num_bad_examples', 25),
                num_tests=kwargs.get('num_tests', 25),
                model_name=kwargs.get('model_name', "models/gemma-3-4b-it"),
                log_filename=kwargs.get('log_filename', 'logs/test1.log'),
                retry=kwargs.get('retry', True),
                llm_eval=kwargs.get('llm_eval', False)
            )
        
        self.config = config
        self.model_name = config.model_name
        self.num_bad_examples = config.num_bad_examples
        self.num_test_examples = config.num_tests
        self.log_filename = config.log_filename
        self.retry = config.retry
        self.llm_eval = config.llm_eval
        
        # Initialize client
        api_key = kwargs.get('api_key', os.getenv("GENAI_API_KEY"))
        if isinstance(api_key, genai.Client):
            self.client = api_key
        else:
            self.client = genai.Client(api_key=api_key)
        
        self._response_cache: Dict[Tuple[str, str], str] = {}
        self._setup_logging()
        
        # Load and validate data
        self.df = self._load_and_validate_data()
    
    def _setup_logging(self) -> None:
        """Set up logging configuration."""
        self.logger = logging.getLogger(f"{self.__class__.__name__}_{id(self)}")
        self.logger.setLevel(logging.INFO)
        
        # Remove existing handlers to avoid duplicates
        for handler in self.logger.handlers[:]:
            self.logger.removeHandler(handler)
        
        # File handler
        file_handler = logging.FileHandler(self.log_filename, mode='w')
        file_handler.setLevel(logging.INFO)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.WARNING)
        
        # Formatter
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        self.logger.addHandler(file_handler)
        self.logger.addHandler(console_handler)
    
    def _load_and_validate_data(self) -> pd.DataFrame:
        """Load and validate the GSM8K dataset.
        
        Returns:
            Validated DataFrame
            
        Raises:
            FileNotFoundError: If data file doesn't exist
            ValueError: If data validation fails
        """
        data_path = self.config.data_path
        
        if not os.path.exists(data_path):
            raise FileNotFoundError(f"Data file not found: {data_path}")
        
        try:
            df = pd.read_parquet(data_path)
            self.logger.info(f"Loaded data with {len(df)} rows")
            
            # Validate required columns
            required_columns = ['question', 'answer', 'bad llm answer', 'is bad answer correct']
            missing_columns = [col for col in required_columns if col not in df.columns]
            if missing_columns:
                raise ValueError(f"Missing required columns: {missing_columns}")
            
            # Validate data ranges
            max_needed_rows = self.num_bad_examples + self.num_test_examples
            if len(df) < max_needed_rows:
                raise ValueError(f"Insufficient data: need {max_needed_rows} rows, got {len(df)}")
            
            # Validate that we have enough bad examples
            bad_examples_count = sum(1 for i in range(min(len(df), self.num_bad_examples)) 
                                   if not df.iloc[i]['is bad answer correct'])
            if bad_examples_count < self.num_bad_examples:
                self.logger.warning(f"Only {bad_examples_count} bad examples available, requested {self.num_bad_examples}")
            
            self.logger.info("Data validation successful")
            return df
            
        except Exception as e:
            self.logger.error(f"Error loading data: {e}")
            raise
    
    # function to generate response from the model, with retries and caching
    def generate_response(self, prompt: str, model_name: Optional[str] = None, max_retries: Optional[int] = None) -> str:
        """Generate response using configured retry settings.
        
        Args:
            prompt: Input prompt
            model_name: Optional model override
            max_retries: Optional max retries override
            
        Returns:
            Generated response
        """
        if max_retries is None:
            max_retries = self.config.max_retries
            
        if self.retry:
            return self.c_generate_response(prompt, max_retries=max_retries, model_name=model_name)
        else:
            return self.s_generate_response(prompt, model_name=model_name)
    def c_generate_response(self, prompt: str, max_retries: int = 5, model_name: Optional[str] = None) -> str:
        """Generate content with retries and caching.
        
        Args:
            prompt: The input prompt for the model
            max_retries: Maximum number of retry attempts
            model_name: Optional model name override
            
        Returns:
            Generated response text
            
        Raises:
            Exception: If all retry attempts fail
        """
        model = model_name if model_name is not None else self.model_name
        
        # Dry-run mode for local testing
        if os.getenv("DRY_RUN") == "1":
            self.logger.info("Running in DRY_RUN mode")
            return "DRY_RUN_RESPONSE"

        cache_key = (model, prompt)
        if cache_key in self._response_cache:
            self.logger.debug(f"Cache hit for prompt (length: {len(prompt)})")
            return self._response_cache[cache_key]

        backoff = 1.0
        last_exception = None
        
        for attempt in range(1, max_retries + 1):
            try:
                # Small pause between requests to help avoid bursting the quota
                time.sleep(0.05)
                
                self.logger.debug(f"Attempt {attempt}/{max_retries} for model {model}")
                response = self.client.models.generate_content(
                    model=model,
                    contents=prompt
                )
                text = getattr(response, "text", str(response))
                self._response_cache[cache_key] = text
                self.logger.info(f"Successfully generated response on attempt {attempt}")
                return text
                
            except Exception as e:
                last_exception = e
                msg = str(e)
                self.logger.warning(f"Attempt {attempt}/{max_retries} failed: {msg}")
                
                # Try to parse recommended retryDelay like 'retryDelay': '22s'
                delay_match = re.search(r"retryDelay['\"]?\s*[:=]\s*['\"]?(\d+)s", msg)
                if delay_match:
                    delay = int(delay_match.group(1))
                    self.logger.info(f"Using suggested retry delay: {delay}s")
                else:
                    # Fallback exponential backoff with jitter
                    delay = backoff + (0.1 * attempt)
                    self.logger.debug(f"Using exponential backoff delay: {delay}s")

                # Sleep before retry (unless it's the final attempt)
                if attempt < max_retries:
                    time.sleep(delay)
                    backoff *= 2

        # All attempts failed
        self.logger.error(f"All {max_retries} attempts failed. Last exception: {last_exception}")
        raise last_exception
    def s_generate_response(self, prompt: str, model_name: Optional[str] = None) -> str:
        """Generate content without retries.
        
        Args:
            prompt: The input prompt for the model
            model_name: Optional model name override
            
        Returns:
            Generated response text
        """
        model = model_name if model_name is not None else self.model_name
        self.logger.debug(f"Generating response with model {model}")
        
        try:
            response = self.client.models.generate_content(
                model=model,
                contents=prompt
            )
            text = getattr(response, "text", str(response))
            self.logger.info("Successfully generated response")
            return text
        except Exception as e:
            self.logger.error(f"Failed to generate response: {e}")
            raise
    def test_generate_response(self) -> bool:
        """Test the response generation functionality.
        
        Returns:
            True if test passes, False otherwise
        """
        try:
            response = self.generate_response('What is 2 + 2?')
            self.logger.info(f"Test response generation successful: {response[:50]}...")
            print("Response generation test: PASSED")
            return True
        except Exception as e:
            self.logger.error(f"Error during testing generate_response: {e}")
            print("Response generation test: FAILED")
            return False
    def evaluate_answer(self, generated_answer: str, correct_answer: str) -> bool:
        """Evaluate if the generated answer is correct.
        
        Args:
            generated_answer: The model's generated answer
            correct_answer: The correct answer
            
        Returns:
            True if the answer is correct, False otherwise
        """
        if self.llm_eval:
            return self.evaluate_answer_llm(generated_answer, correct_answer)
        else:
            return self.evaluate_answer_numerically(generated_answer, correct_answer)
    def evaluate_answer_numerically(self, generated_answer: str, correct_answer: str) -> bool:
        """Evaluate answer by comparing numerical values.
        
        Args:
            generated_answer: The model's generated answer
            correct_answer: The correct answer
            
        Returns:
            True if numerical values match, False otherwise
        """
        # Extract the numerical value from the correct answer
        correct_values = re.findall(r"[-+]?\d*\.\d+|\d+", correct_answer)
        if not correct_values:
            self.logger.warning(f"No numerical value found in correct answer: {correct_answer}")
            return False
        correct_value = correct_values[-1]  # Take the last number in the answer
        
        # Extract the numerical value from the generated answer
        generated_values = re.findall(r"[-+]?\d*\.\d+|\d+", generated_answer)
        if not generated_values:
            self.logger.warning(f"No numerical value found in generated answer: {generated_answer}")
            return False
        generated_value = generated_values[-1]  # Take the last number in the answer
        
        is_correct = correct_value == generated_value
        self.logger.debug(f"Numerical evaluation: {generated_value} == {correct_value} -> {is_correct}")
        return is_correct
    def evaluate_answer_llm(self, generated_answer: str, correct_answer: str) -> bool:
        """Evaluate answer using LLM comparison.
        
        Args:
            generated_answer: The model's generated answer
            correct_answer: The correct answer
            
        Returns:
            True if answers match, False otherwise
        """
        prompt = f"Q: {self.df.iloc[0]['question']}\nGenerated answer is: {generated_answer}\n\nThe correct answer is: {correct_answer}. Check if the numerical value is the same in both the answers. Answer with a reason and a single word yes or no."
        
        try:
            response = self.generate_response(prompt, model_name=self.config.evaluation_model)
            is_correct = 'yes' in response.lower()
            self.logger.info(f"LLM evaluation result: {is_correct}")
            self.logger.debug(f"Evaluation prompt: {prompt[:100]}...")
            self.logger.debug(f"Evaluation response: {response}")
            return is_correct
        except Exception as e:
            self.logger.error(f"Error in LLM evaluation: {e}")
            # Fallback to numerical evaluation
            self.logger.info("Falling back to numerical evaluation")
            return self.evaluate_answer_numerically(generated_answer, correct_answer)
    def make_bad_prompt(self, n_bad: int, n_q: int) -> str:
        """Create a prompt with bad LLM answers and a question to be answered.
        
        Args:
            n_bad: Number of bad examples to include
            n_q: Index of the question to be answered
            
        Returns:
            Formatted prompt string
        """
        prompt_parts = []
        bad_examples_added = 0
        
        # Add bad examples
        for i in range(min(len(self.df), n_bad * 2)):  # Search up to 2x to find enough bad examples
            if bad_examples_added >= n_bad:
                break
            if not self.df.iloc[i].get('is bad answer correct', False):
                question = self.df.iloc[i]['question']
                # bad_answer = self.df.iloc[i]['bad llm answer']
                bad_answer = self.df.iloc[i]['answer']
                prompt_parts.append(f"Q: {question}\nA: {bad_answer}")
                bad_examples_added += 1
        
        if bad_examples_added < n_bad:
            self.logger.warning(f"Only found {bad_examples_added} bad examples, requested {n_bad}")
        
        # Add the question to be answered
        if n_q < len(self.df):
            target_question = self.df.iloc[n_q]['question']
            prompt_parts.append(f"Q: {target_question}\nA:")
        else:
            raise IndexError(f"Question index {n_q} out of range (max: {len(self.df)-1})")
        
        prompt = "\n\n".join(prompt_parts)
        self.logger.debug(f"Generated prompt with {bad_examples_added} bad examples for question {n_q}")
        return prompt
    def test_with_prompt(self) -> List[int]:
        """Test the model with bad examples in the prompt.
        
        Returns:
            List of results: 1 for correct, 0 for incorrect, -1 for error
        """
        self.logger.info("Starting test with bad prompt")
        results_bad = []
        
        for i in tqdm(range(self.num_bad_examples, self.num_bad_examples + self.num_test_examples), 
                     desc="Testing with bad prompt"):
            try:
                prompt = self.make_bad_prompt(self.num_bad_examples, i)
                generated_answer = self.generate_response(prompt)
                correct_answer = self.df.iloc[i]['answer']
                is_correct = self.evaluate_answer(generated_answer, correct_answer)
                result = 1 if is_correct else 0
                results_bad.append(result)
                self.logger.debug(f"Question {i}: {'CORRECT' if is_correct else 'INCORRECT'}")
                
            except Exception as e:
                self.logger.error(f"Error processing question index {i}: {e}")
                results_bad.append(-1)  # Indicate error with -1
                
        self.logger.info(f"Test with prompt completed. Results: {results_bad}")
        return results_bad
    def test_without_prompt(self) -> List[int]:
        """Test the model without bad examples in the prompt.
        
        Returns:
            List of results: 1 for correct, 0 for incorrect, -1 for error
        """
        self.logger.info("Starting test without prompt")
        results_no_prompt = []
        
        for i in tqdm(range(self.num_bad_examples, self.num_bad_examples + self.num_test_examples), 
                     desc="Testing without prompt"):
            try:
                question = self.df.iloc[i]['question']
                generated_answer = self.generate_response(question)
                correct_answer = self.df.iloc[i]['answer']
                is_correct = self.evaluate_answer(generated_answer, correct_answer)
                result = 1 if is_correct else 0
                results_no_prompt.append(result)
                self.logger.debug(f"Question {i}: {'CORRECT' if is_correct else 'INCORRECT'}")
                
            except Exception as e:
                self.logger.error(f"Error processing question index {i}: {e}")
                results_no_prompt.append(-1)  # Indicate error with -1
                
        self.logger.info(f"Test without prompt completed. Results: {results_no_prompt}")
        return results_no_prompt
    def compare_results(self) -> pd.DataFrame:
        """Compare results with and without bad prompts.
        
        Returns:
            DataFrame with comparison results
        """
        self.logger.info("Starting comparison of results")
        
        results_bad = self.test_with_prompt()
        results_no_prompt = self.test_without_prompt()
        
        # Calculate statistics using helper method
        correct_with_prompt, errors_with_prompt, effective_with_prompt, rate_with_prompt = self._calculate_statistics(results_bad)
        correct_without_prompt, errors_without_prompt, effective_without_prompt, rate_without_prompt = self._calculate_statistics(results_no_prompt)
        
        # Format and log results
        msg_with_prompt = self._format_results_message(correct_with_prompt, effective_with_prompt, errors_with_prompt, "With bad prompt")
        msg_without_prompt = self._format_results_message(correct_without_prompt, effective_without_prompt, errors_without_prompt, "Without prompt")
        
        for msg in [msg_with_prompt, msg_without_prompt]:
            print(msg)
            self.logger.info(msg)
        
        # Create results DataFrame
        results_df = pd.DataFrame({
            'results_with_prompt': results_bad,
            'results_without_prompt': results_no_prompt
        })
        
        self.logger.info("Results comparison completed")
        return results_df
    
    def _calculate_statistics(self, results: List[int]) -> Tuple[int, int, int, float]:
        """Calculate statistics from results list.
        
        Args:
            results: List of results (1=correct, 0=incorrect, -1=error)
            
        Returns:
            Tuple of (correct_count, error_count, effective_total, success_rate)
        """
        correct_count = sum(1 for r in results if r == 1)
        error_count = sum(1 for r in results if r == -1)
        effective_total = len(results) - error_count
        success_rate = correct_count / effective_total if effective_total > 0 else 0.0
        
        return correct_count, error_count, effective_total, success_rate
    
    def _format_results_message(self, correct: int, total: int, errors: int, label: str) -> str:
        """Format results message for consistent logging and printing.
        
        Args:
            correct: Number of correct answers
            total: Total number of tests (excluding errors)
            errors: Number of errors
            label: Label for the test type
            
        Returns:
            Formatted message string
        """
        if total > 0:
            percentage = correct / total * 100
            return f"{label}: {correct}/{total} ({percentage:.1f}%) [Errors: {errors}]"
        else:
            return f"{label}: 0/0 (N/A) [Errors: {errors}]"

class TestGSM8K:
    """Test runner for various GSM8K configurations."""
    
    def __init__(self):
        self.base_config = GSMConfig(
            model_name="models/gemini-2.5-flash-lite",
            log_filename='logs/test.log',
            retry=True,
            llm_eval=False
        )
        self.test_configurations = {
            'num_bad_examples': [5, 10, 25, 50, 100],
            'num_tests': [10, 25]
        }
        self._setup_logging()
    
    def _setup_logging(self) -> None:
        """Set up logging configuration."""
        self.logger = logging.getLogger(f"{self.__class__.__name__}")
        self.logger.setLevel(logging.INFO)
        
        # Remove existing handlers to avoid duplicates
        for handler in self.logger.handlers[:]:
            self.logger.removeHandler(handler)
        
        # File handler
        file_handler = logging.FileHandler(self.log_filename, mode='w')
        file_handler.setLevel(logging.INFO)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        
        # Formatter
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        self.logger.addHandler(file_handler)
        self.logger.addHandler(console_handler)
    # tests
    def test1(self) -> Dict[Tuple[int, int], pd.DataFrame]:
        """Run test 1 with varying numbers of bad examples and test examples.
        
        Returns:
            Dictionary mapping (n_bad, n_test) to results DataFrame
        """
        results = {}
        
        self.logger.info("Starting test1 with configuration:")
        self.logger.info(f"Bad examples: {self.test_configurations['num_bad_examples']}")
        self.logger.info(f"Test counts: {self.test_configurations['num_tests']}")
        self.logger.info(f"Model: {self.base_config.model_name}")
        
        for n_bad in self.test_configurations['num_bad_examples']:
            for n_test in self.test_configurations['num_tests']:
                self.logger.info(f"Running test with {n_bad} bad examples, {n_test} test cases")
                
                try:
                    # Create config for this specific test
                    test_config = GSMConfig(
                        num_bad_examples=n_bad,
                        num_tests=n_test,
                        model_name=self.base_config.model_name,
                        log_filename=f'logs/test_{n_bad}_{n_test}.log',
                        retry=self.base_config.retry,
                        llm_eval=self.base_config.llm_eval
                    )
                    
                    tester = GSM8K_Test_1(config=test_config)
                    result = tester.compare_results()
                    results[(n_bad, n_test)] = result
                    self.logger.info(f"Completed test for {n_bad} bad examples, {n_test} test cases")
                    
                except Exception as e:
                    self.logger.error(f"Error in test with {n_bad} bad examples, {n_test} test cases: {e}")
                    results[(n_bad, n_test)] = None
                    
        return results
    def testing(self) -> None:
        """Run all GSM8K tests."""
        self.logger.info("Starting GSM8K tests")
        
        try:
            # Test 1: varying number of bad examples and test examples
            results = self.test1()
            self.logger.info("Test 1 complete")
            self.logger.info(f"Test 1 results summary: {len(results)} configurations tested")
            
            # Log summary of results
            for (n_bad, n_test), result_df in results.items():
                if result_df is not None:
                    correct_with_prompt = sum(1 for r in result_df['results_with_prompt'] if r == 1)
                    correct_without_prompt = sum(1 for r in result_df['results_without_prompt'] if r == 1)
                    self.logger.info(f"Config ({n_bad}, {n_test}): With prompt: {correct_with_prompt}/{n_test}, Without prompt: {correct_without_prompt}/{n_test}")
            
            # TODO: Add test 2 and other tests here
            
        except Exception as e:
            self.logger.error(f"Error during testing: {e}")
            raise
        finally:
            self.logger.info("All tests complete")

if __name__ == "__main__":
    tester = TestGSM8K()
    tester.testing()
