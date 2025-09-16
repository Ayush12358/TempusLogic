import os
from google import genai
import pandas as pd
import time
import re
from tqdm import tqdm

class TestGSM8K:
    # initialize with number of bad examples to include in the prompt, number of test examples to evaluate, model name, and api key
    def __init__(self, num_bad_examples=25, num_tests=25, model_name="models/gemma-3-4b-it", 
                 api_key=genai.Client(api_key=os.getenv("GENAI_API_KEY")), log_filename='test.log',
                 retry = True, llm_eval = True):
        super().__init__()
        
        self.model_name = model_name
        # read only the needed number of rows from the parquet file
        self.df = pd.read_parquet('data/gsm8k_with_bad_llm_answers.parquet')
        
        self.client = genai.Client(api_key=api_key)
        # self.client = genai.Client(api_key='AIzaSyBvpujYrBWh07wqmP3gm5jgZ3ITmW1vzzo')
        self._response_cache = {}
        self.num_bad_examples = num_bad_examples
        self.num_test_examples = num_tests
        self.log_filename = log_filename
        self.logger = None
        self.retry = retry
        self.llm_eval = llm_eval
    # logging functions
    def init_log(self):
        log = open(self.log_filename, 'w')
        log.write("Test log\n")
        log.flush()
        self.logger = log
    def log(self, message):
        self.logger.write(message + "\n")
        self.logger.flush()
    def close_log(self):
        self.logger.close()
    # function to generate response from the model, with retries and caching
    def generate_response(self, prompt, model_name=None, max_retries=5):
        if self.retry:
            return self.c_generate_response(prompt, max_retries=max_retries, model_name=model_name)
        else:
            return self.s_generate_response(prompt, model_name=model_name)
    def c_generate_response(self, prompt, max_retries=5, model_name=None):
        # Generate content with retries and simple caching.
        # Respects RetryInfo in error messages if present (e.g. 'retryDelay': '22s').
        # Set environment variable DRY_RUN=1 to avoid making API calls during development.
        if model_name is not None:
            model = model_name
        else:
            model = self.model_name
        # dry-run mode for local testing
        if os.getenv("DRY_RUN") == "1":
            return "DRY_RUN_RESPONSE"

        cache_key = (model, prompt)
        if cache_key in self._response_cache:
            return self._response_cache[cache_key]

        backoff = 1.0
        for attempt in range(1, max_retries + 1):
            try:
                # small pause between requests to help avoid bursting the quota
                time.sleep(0.05)
                response = self.client.models.generate_content(
                    model=model,
                    contents=prompt
                )
                text = getattr(response, "text", str(response))
                self._response_cache[cache_key] = text
                return text
            except Exception as e:
                msg = str(e)
                # try to parse recommended retryDelay like 'retryDelay': '22s'
                m = re.search(r"retryDelay['\"]?\s*[:=]\s*['\"]?(\d+)s", msg)
                if m:
                    delay = int(m.group(1))
                else:
                    # fallback exponential backoff with jitter
                    delay = backoff + (0.1 * attempt)

                # if it's clearly a quota error, wait the suggested time; otherwise exponential backoff
                if "RESOURCE_EXHAUSTED" in msg or "quota" in msg.lower() or "RetryInfo" in msg:
                    time.sleep(delay)
                else:
                    time.sleep(delay)

                backoff *= 2
                # final attempt will re-raise if it fails
                if attempt == max_retries:
                    raise
    def s_generate_response(self, prompt, model_name=None):
        if model_name is None:
            model_name = self.model_name
        response = self.client.models.generate_content(
            model=model_name,
            contents=prompt
        )
        text = getattr(response, "text", str(response))
        return text
    def test_generate_response(self):
        try:
            response = self.generate_response('What is 2 + 2?')
        except Exception as e:
            print(f"Error during testing generate_response.")
            return
        print (f"Working!!")
    # evaluate if the generated answer is correct
    def evaluate_answer(self, generated_answer, correct_answer):
        if self.llm_eval:
            return self.evaluate_answer_llm(generated_answer, correct_answer)
        else:
            return self.evaluate_answer_numerically(generated_answer, correct_answer)
    def evaluate_answer_numerically(self, generated_answer, correct_answer):
        # extract the numerical value from the correct answer
        correct_value = re.findall(r"[-+]?\d*\.\d+|\d+", correct_answer)
        if len(correct_value) == 0:
            return False
        correct_value = correct_value[-1]  # take the last number in the answer
        # extract the numerical value from the generated answer
        generated_value = re.findall(r"[-+]?\d*\.\d+|\d+", generated_answer)
        if len(generated_value) == 0:
            return False
        generated_value = generated_value[-1]  # take the last number in the answer
        return correct_value == generated_value
    def evaluate_answer_llm(self, generated_answer, correct_answer):
        prompt = f"Q: {self.df.iloc[0]['question']}\nGenerated answer is: {generated_answer}\n\nThe correct answer is: {correct_answer}. Check if the numerical value is the same in both the answers. Answer with a reason and a single word yes or no."
        response = self.generate_response(prompt, model_name='models/gemma-3-12b-it')
        # if it includes the word yes, return True else False
        self.log(f"Evaluation prompt: {prompt}\nEvaluation response: {response}\n")
        return 'yes' in response.lower()
    # make a prompt with n bad llm answers and 1 question to be answered
    def make_bad_prompt(self, n_bad,n_q):
        # m is the number of bad llm answers to include
        # n is the question that needs to be answered
        # return the prompt string
        m,n = n_bad,n_q
        prompt = ""
        for i in range(m):
            prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['bad llm answer']}\n\n"
        prompt += f"Q: {self.df.iloc[n]['question']}\nA:"
        return prompt
    def test_with_prompt(self):
        self.log("Starting test with bad prompt\n")
        results_bad = []
        for i in tqdm(range(self.num_bad_examples,self.num_bad_examples+self.num_test_examples), desc="Testing with bad prompt"):
            prompt = self.make_bad_prompt(self.num_bad_examples,i)
            try:
                generated_answer = self.generate_response(prompt)
            except Exception as e:
                print(f"Error generating response for question index {i}")
                results_bad.append(-1)  # indicate error with -1
                self.log(f"Error generating response for question index {i}: {e}")
                continue
            
            correct_answer = self.df.iloc[i]['answer']
            is_correct = self.evaluate_answer(generated_answer, correct_answer)
            results_bad.append(1 if is_correct else 0)
        return results_bad
    def test_without_prompt(self):
        self.log("Starting test without prompt\n")
        results_no_prompt = []
        for i in tqdm(range(self.num_bad_examples,self.num_bad_examples+self.num_test_examples), desc="Testing without prompt"):
            question = self.df.iloc[i]['question']
            try:
                generated_answer = self.generate_response(question)
            except Exception as e:
                print(f"Error generating response for question index {i}: {e}")
                results_no_prompt.append(-1)  # indicate error with -1
                self.log(f"Error generating response for question index {i}")
                continue
            correct_answer = self.df.iloc[i]['answer']
            is_correct = self.evaluate_answer(generated_answer, correct_answer)
            results_no_prompt.append(1 if is_correct else 0)
        return results_no_prompt
    def compare_results(self):
        results_bad = self.test_with_prompt()
        results_no_prompt = self.test_without_prompt()
        correct_with_prompt = sum(1 for r in results_bad if r == 1)
        correct_without_prompt = sum(1 for r in results_no_prompt if r == 1)
        inconclusive_with_prompt = sum(1 for r in results_bad if r == -1)
        inconclusive_without_prompt = sum(1 for r in results_no_prompt if r == -1)
        
        print(f"Inconclusive (errors) with prompt: {inconclusive_with_prompt}/{self.num_test_examples}")
        print(f"Inconclusive (errors) without prompt: {inconclusive_without_prompt}/{self.num_test_examples}")
        self.log(f"Inconclusive (errors) with prompt: {inconclusive_with_prompt}/{self.num_test_examples}")
        self.log(f"Inconclusive (errors) without prompt: {inconclusive_without_prompt}/{self.num_test_examples}")
        print(f"Correct with prompt: {correct_with_prompt}/{self.num_test_examples-inconclusive_with_prompt}")
        print(f"Correct without prompt: {correct_without_prompt}/{self.num_test_examples-inconclusive_without_prompt}")
        self.log(f"Correct with prompt: {correct_with_prompt}/{self.num_test_examples-inconclusive_with_prompt}")
        self.log(f"Correct without prompt: {correct_without_prompt}/{self.num_test_examples-inconclusive_without_prompt}")

if __name__ == "__main__":
    api_key = os.getenv("GENAI_API_KEY")
    # api_key = 'AIzaSyBvpujYrBWh07wqmP3gm5jgZ3ITmW1vzzo'
    model_name = 'models/gemini-2.5-flash-lite'
    # model_name = 'models/gemma-3-4b-it'
    tester = TestGSM8K(15,25, model_name=model_name, api_key=api_key, log_filename='test.log', 
                       retry=False, llm_eval=False)
    tester.init_log()
    # tester.test_generate_response()
    tester.compare_results()
    tester.close_log()
