
import os
from google import genai
import pandas as pd
import time
import re

if not os.path.exists("data/gsm8k_train.parquet"):
    splits = {'train': 'main/train-00000-of-00001.parquet', 'test': 'main/test-00000-of-00001.parquet'}
    df = pd.read_parquet("hf://datasets/openai/gsm8k/" + splits["train"])
    # save to parquet
    df.to_parquet("data/gsm8k_train.parquet", index=False)
df = pd.read_parquet("data/gsm8k_train.parquet")

# now use gemma 3 1b to generate bad llm answers
client = genai.Client(api_key=os.getenv("GENAI_API_KEY"))
model_name = "models/gemma-3-1b-it"
_response_cache = {}

def generate_response(prompt, model="models/gemma-3-1b-it", max_retries=5):
    # Generate content with retries and simple caching.
    # Respects RetryInfo in error messages if present (e.g. 'retryDelay': '22s').
    # Set environment variable DRY_RUN=1 to avoid making API calls during development.
    model = model_name or model
    # dry-run mode for local testing
    if os.getenv("DRY_RUN") == "1":
        return "DRY_RUN_RESPONSE"

    cache_key = (model, prompt)
    if cache_key in _response_cache:
        return _response_cache[cache_key]

    backoff = 1.0
    for attempt in range(1, max_retries + 1):
        try:
            # small pause between requests to help avoid bursting the quota
            time.sleep(0.05)
            response = client.models.generate_content(
                model=model,
                contents=prompt
            )
            text = getattr(response, "text", str(response))
            _response_cache[cache_key] = text
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

# check if gsm8k_with_bad_llm_answers.parquet exists, if yes, load it and continue from there
df['bad llm answer'] = ''
if os.path.exists("gsm8k_with_bad_llm_answers.parquet"):
    df_existing = pd.read_parquet("gsm8k_with_bad_llm_answers.parquet")
    # copy the existing answers to the new dataframe
    df['bad llm answer'] = df_existing['bad llm answer']
    print(f"Loaded existing file with {df_existing['bad llm answer'].notnull().sum()} answers")

# iterate over the dataframe and generate bad llm answers for each unanswered question
from tqdm import tqdm
from gsm8k.bad_answers_ollama import model_name
# create a file bad.log that save the response for each question
response_log = open("bad.log", "w")

for index, row in tqdm(df.iterrows(), total=len(df)):
    if row['bad llm answer'] != '':
        continue  # skip if already has an answer
    question = row['question']
    response = generate_response(question)
    df.at[index, 'bad llm answer'] = response
    response_log.write(f"Q: {question}\nA: {response}\n\n")
    response_log.flush()
    if index % 50 == 0:
        # save the dataframe to a new parquet file every 10 iterations
        df.to_parquet("data/gsm8k_with_bad_llm_answers.parquet")
response_log.close()
# save the dataframe to a new parquet file
df.to_parquet("data/gsm8k_with_bad_llm_answers.parquet")
