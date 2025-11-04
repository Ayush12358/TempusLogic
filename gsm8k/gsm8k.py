import os, time, re
from google import genai
import pandas as pd
from tqdm import tqdm
from ollama import Client
import argparse

# answer question
class GSM8K_Test:
    # initialize with number of bad examples to include in the prompt, number of test examples to evaluate, model name, and api key
    def __init__(self, num_bad_examples=25, num_tests=25, model_name="models/gemma-3-4b-it", 
                 api_key=genai.Client(api_key=os.getenv("GENAI_API_KEY")), log_filename='logs/test1.log',
                 retry = True, llm_eval = True, test_no = 1):
        super().__init__()
        
        self.model_name = model_name
        # read only the needed number of rows from the parquet file
        self.df = pd.read_parquet('data/gsm8k_with_bad_llm_answers.parquet')
        
        self.client = genai.Client(api_key=api_key)
        self.client_ollama = Client()
        self._response_cache = {}
        self.num_bad_examples = num_bad_examples
        self.num_test_examples = num_tests
        self.log_filename = log_filename
        self.logger = None
        self.retry = retry
        self.llm_eval = llm_eval
        self.test_no = test_no
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
    def generate_response(self, prompt, model_name=None):
        if model_name is None:
            model_name = self.model_name
        response = self.client_ollama.generate(
            model=model_name,
            prompt=prompt
        )
        text = getattr(response, "text", str(response))
        return text
    def gemini_generate_response(self, prompt, model_name=None, max_retries=5):
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
    def make_bad_prompt(self, n_bad,n_q,is_bad_prompt=True):
        if self.test_no == 1:
            return self.make_bad_prompt1(n_bad, n_q, is_bad_prompt)
        elif self.test_no == 2:
            return self.make_bad_prompt2(n_bad, n_q, is_bad_prompt)
        else:
            return self.make_bad_prompt3(n_bad, n_q, is_bad_prompt)
    def make_bad_prompt1(self, n_bad,n_q,is_bad_prompt=True):
        # m is the number of bad llm answers to include
        # n is the question that needs to be answered
        # return the prompt string
        m,n = n_bad,n_q
        prompt = ""
        for i in range(m):
            if self.df.iloc[i]['is bad answer correct'] == True:
                continue
            if not is_bad_prompt:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['bad llm answer']}\n\n"
            else:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['answer']}\n\n"
        prompt += '\nNow correctly answer the next question.\n'
        prompt += f"Q: {self.df.iloc[n]['question']}\nA:"
        return prompt
    def make_bad_prompt2(self, n_bad,n_q,is_bad_prompt=True):
        # m is the number of bad llm answers to include
        # n is the question that needs to be answered
        # return the prompt string
        m,n = n_bad,n_q
        prompt = ""
        for i in range(m):
            if self.df.iloc[i]['is bad answer correct'] == True:
                continue
            if not is_bad_prompt:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['bad llm answer']}\n\n"
            else:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['answer']}\n\n"
        prompt += '\nNow correctly answer the next incompletely answered question.\n'
        prompt += f"Q: {self.df.iloc[n]['question']}\nA:"
        answer = self.df.iloc[n]['bad llm answer']
        # remove half the text from the bad llm answer to simulate an incomplete answer
        half_index = len(answer) // 2
        incomplete_answer = answer[:half_index]
        prompt += incomplete_answer
        return prompt
    def make_bad_prompt3(self, n_bad,n_q,is_bad_prompt=True):
        # m is the number of bad llm answers to include
        # n is the question that needs to be answered
        # return the prompt string
        m,n = n_bad,n_q
        prompt = ""
        for i in range(m):
            if self.df.iloc[i]['is bad answer correct'] == True:
                continue
            if not is_bad_prompt:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['bad llm answer']}\n\n"
            else:
                prompt += f"Q: {self.df.iloc[i]['question']}\nA: {self.df.iloc[i]['answer']}\n\n"
        prompt += '\nNow correctly answer the next answered question again.\n'
        prompt += f"Q: {self.df.iloc[n]['question']}\nA:{self.df.iloc[n]['bad llm answer']}\n\n"
        return prompt
    # test the model with bad prompts, goog prompts, and without prompts
    def test_with_prompt(self, is_bad_prompt=True):
        self.log("Starting test with bad prompt\n")
        results_bad = []
        description = "Testing with bad prompt" if is_bad_prompt else "Testing with good prompt"
        for i in tqdm(range(self.num_bad_examples,self.num_bad_examples+self.num_test_examples), desc=description):
            prompt = self.make_bad_prompt(self.num_bad_examples,i,is_bad_prompt)
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
        print ('\n')
        results_bad = self.test_with_prompt(is_bad_prompt=True)
        results_good = self.test_with_prompt(is_bad_prompt=False)
        results_no_prompt = self.test_without_prompt()
        
        correct_bad_prompt = sum(1 for r in results_bad if r == 1)
        correct_good_prompt = sum(1 for r in results_good if r == 1)
        correct_without_prompt = sum(1 for r in results_no_prompt if r == 1)
        
        inconclusive_bad_prompt = sum(1 for r in results_bad if r == -1)
        inconclusive_good_prompt = sum(1 for r in results_good if r == -1)
        inconclusive_without_prompt = sum(1 for r in results_no_prompt if r == -1)
        
        self.log(f"Inconclusive (errors) with bad prompt: {inconclusive_bad_prompt}/{self.num_test_examples}")
        self.log(f"Inconclusive (errors) with good prompt: {inconclusive_good_prompt}/{self.num_test_examples}")
        self.log(f"Inconclusive (errors) without prompt: {inconclusive_without_prompt}/{self.num_test_examples}")
        
        self.log(f"Correct with bad prompt: {correct_bad_prompt}/{self.num_test_examples-inconclusive_bad_prompt}")
        self.log(f"Correct with good prompt: {correct_good_prompt}/{self.num_test_examples-inconclusive_good_prompt}")
        self.log(f"Correct without prompt: {correct_without_prompt}/{self.num_test_examples-inconclusive_without_prompt}")
        results = pd.DataFrame({
            'results_bad_prompt': results_bad,
            'results_good_prompt': results_good,
            'results_without_prompt': results_no_prompt
        })
        return results
class TestGSM8K:
    def __init__(self, model_name, num_bad_examples, num_tests):
        super().__init__()
        self.config = {
            'num_bad_examples': num_bad_examples,
            'num_tests': num_tests,
            'model_name': model_name,
            'api_key': os.getenv("GENAI_API_KEY"),
            'retry': True,
            'llm_eval': False
        }
        self.log_filename = 'logs/test.log'
        self.logger = None
        self.results = None
        self.scores = None
        self.final_score = None
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
    # tests
    def score(self):
        scores = []
        for test_name, test_results in self.results.items():
            for (n_bad, n_test), df in test_results.items():
                correct_bad_prompt = sum(1 for r in df['results_bad_prompt'] if r == 1)
                correct_good_prompt = sum(1 for r in df['results_good_prompt'] if r == 1)
                correct_without_prompt = sum(1 for r in df['results_without_prompt'] if r == 1)
                inconclusive_bad_prompt = sum(1 for r in df['results_good_prompt'] if r == -1)
                inconclusive_good_prompt = sum(1 for r in df['results_good_prompt'] if r == -1)
                inconclusive_without_prompt = sum(1 for r in df['results_without_prompt'] if r == -1)
                score_entry = {
                    'test_name': test_name,
                    'num_bad_examples': n_bad,
                    'num_test_examples': n_test,
                    'correct_bad_prompt': correct_bad_prompt,
                    'correct_good_prompt': correct_good_prompt,
                    'correct_without_prompt': correct_without_prompt,
                    'inconclusive_bad_prompt': inconclusive_bad_prompt,
                    'inconclusive_good_prompt': inconclusive_good_prompt,
                    'inconclusive_without_prompt': inconclusive_without_prompt,
                    'accuracy_bad_prompt': correct_bad_prompt / (n_test - inconclusive_bad_prompt) if (n_test - inconclusive_bad_prompt) > 0 else 0,
                    'accuracy_good_prompt': correct_good_prompt / (n_test - inconclusive_good_prompt) if (n_test - inconclusive_good_prompt) > 0 else 0,
                    'accuracy_without_prompt': correct_without_prompt / (n_test - inconclusive_without_prompt) if (n_test - inconclusive_without_prompt) > 0 else 0
                }
                scores.append(score_entry)
        scores_df = pd.DataFrame(scores)
        scores_df['accuracy_bad_deviation'] = scores_df['accuracy_bad_prompt'] - scores_df['accuracy_without_prompt']
        scores_df['accuracy_good_deviation'] = scores_df['accuracy_good_prompt'] - scores_df['accuracy_without_prompt']
        scores_df['accuracy_deviation'] = scores_df[['accuracy_bad_deviation', 'accuracy_good_deviation']].mean(axis=1)
        scores_df['average_deviation'] = scores_df['accuracy_deviation'].mean()
        self.scores = scores_df
        deviations = scores_df['accuracy_deviation']
        # average_deviation = deviations.mean()
        average_std_dev = deviations.std()
        combined_avg_score = {
            'combined_avg_score': scores_df[['accuracy_bad_prompt', 'accuracy_good_prompt', 'accuracy_without_prompt']].mean().mean(),
            'num_bad_examples': scores_df.iloc[0]['num_bad_examples'],
            'num_test_examples': scores_df.iloc[0]['num_test_examples'],
            'average_std_dev': average_std_dev
        }
        self.final_score = average_std_dev
        return pd.DataFrame([combined_avg_score])
    def test(self, test_no):
        config = self.config
        results = {}
        for n_bad in config['num_bad_examples']:
            for n_test in config['num_tests']:
                tester = GSM8K_Test(n_bad, n_test, model_name=config['model_name'], api_key=config['api_key'],
                                   retry=config['retry'], llm_eval=config['llm_eval'], test_no=test_no)
                tester.init_log()
                result = tester.compare_results()
                tester.close_log()
                results[(n_bad, n_test)] = result
        return results
    def testing(self):
        self.init_log()
        self.log("Starting GSM8K tests\n")
        results = []
        for i in range(1,4):
            res = self.test(i)
            results.append(res)
            self.log(f"Test {i} complete\n")
        
        # all tests complete
        self.log("All tests complete\n")
        self.close_log()
        results = {
            'test1': results[0],
            'test2': results[1],
            'test3': results[2]
        }
        self.results = results
        return self.score()
    def run_tests(self):
        scores = self.testing()
        scores.to_csv('results/scores.csv', mode='a', header=not os.path.exists('results/scores.csv'))
        return self.final_score

def running_all(llms, num_bad_examples, num_tests):
    scores =[]
    for llm in llms:
        # start time
        start_time = time.time()
        # 
        final_score = TestGSM8K(llm, num_bad_examples, num_tests).run_tests()
        scores.append(final_score)
        # 
        end_time = time.time()
        print (f"\nTotal time taken: {end_time - start_time} seconds")
    
    print (f"Final scores: {scores}")
    # slope of the scores
    slope = (scores[-1] - scores[0]) / (num_bad_examples[-1] - num_bad_examples[0])
    print (f"Slope of the scores: {slope}")

if __name__ == "__main__":
    # run the tests
    # llms = ["models/gemini-2.5-flash-lite"]
    llms = ["gpt-oss:120b-cloud", "glm-4.6:cloud", "deepseek-v3.1:671b-cloud", "kimi-k2:1t-cloud"] 
    num_bad_examples = [5, 10, 20, 40, 80, 120, 250, 400, 550, 700]
    num_tests = [1]
    # get the following from the args
    # python gsm8k.py --ollama_models ... --num_bad_examples ... --num_tests ...
    parser = argparse.ArgumentParser()
    parser.add_argument('--ollama_models', nargs='+', default=llms, help='List of LLM models to test')
    parser.add_argument('--num_bad_examples', nargs='+', type=int, default=num_bad_examples, help='List of number of bad examples to test')
    parser.add_argument('--num_tests', nargs='+', type=int, default=num_tests, help='List of number of tests to run')
    args = parser.parse_args()
    llms = args.ollama_models
    num_bad_examples = args.num_bad_examples
    num_tests = args.num_tests
    running_all(llms, num_bad_examples, num_tests)

