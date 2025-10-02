
import os
from google import genai
import pandas as pd
import time
import re
import ollama
from google.genai import types

if not os.path.exists("data/gsm8k_train.parquet"):
    splits = {'train': 'main/train-00000-of-00001.parquet', 'test': 'main/test-00000-of-00001.parquet'}
    df = pd.read_parquet("hf://datasets/openai/gsm8k/" + splits["train"])
    # save to parquet
    df.to_parquet("data/gsm8k_train.parquet", index=False)
df = pd.read_parquet("data/gsm8k_train.parquet")

# now use gemma 3 1b to generate bad llm answers
client = genai.Client(api_key=os.getenv("GENAI_API_KEY"))
model_name = "models/gemini-2.5-flash-lite"
# ollama_model_name = "gemma3:4b"
ollama_model_name = "deepseek-r1:1.5b"

generate_to_use = "c" # o for ollama, c for complex, s for simple

def generate_response(prompt, system_prompt):
    if generate_to_use == "o":
        return o_generate_response(prompt, system_prompt)
    elif generate_to_use == "c":
        return c_generate_response(prompt, system_prompt)
    else:
        return s_generate_response(prompt, system_prompt)
def o_generate_response(prompt, system_prompt): # ollama
    system_prompt = "You are a helpful assistant that always answers questions wrongly."
    response = ollama.chat(model=ollama_model_name, messages=[
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": prompt}
    ])
    text = response['message']['content']
    return text
def c_generate_response(prompt, system_prompt, max_retries=5): # complex
    # Generate content with retries and simple caching.
    # Respects RetryInfo in error messages if present (e.g. 'retryDelay': '22s').
    # Set environment variable DRY_RUN=1 to avoid making API calls during development.
    model = model_name
    # dry-run mode for local testing
    if os.getenv("DRY_RUN") == "1":
        return "DRY_RUN_RESPONSE"

    backoff = 1.0
    for attempt in range(1, max_retries + 1):
        try:
            # small pause between requests to help avoid bursting the quota
            time.sleep(0.05)
            config = types.GenerateContentConfig(system_instruction=system_prompt)
            response = client.models.generate_content(
                model=model,
                config=config,
                contents=prompt
            )
            text = getattr(response, "text", str(response))
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
def s_generate_response(prompt, system_prompt): # simple
    # Fix: Use GenerateContentConfig with system_instruction for proper system prompt handling in Google GenAI API
    config = types.GenerateContentConfig(system_instruction=system_prompt)
    response = client.models.generate_content(
        model=model_name,
        config=config,
        contents=prompt
    )
    text = getattr(response, "text", str(response))
    return text

def evaluate_answer_numerically(generated_answer, correct_answer):
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

# check if gsm8k_with_bad_llm_answers.parquet exists, if yes, load it and continue from there
df['bad llm answer'] = ''
df['is bad answer correct'] = None
if os.path.exists("data/gsm8k_with_bad_llm_answers.parquet"):
    df_existing = pd.read_parquet("data/gsm8k_with_bad_llm_answers.parquet")
    # copy the existing answers to the new dataframe
    df['bad llm answer'] = df_existing['bad llm answer']
    print(f"Loaded existing file with {df_existing['bad llm answer'].notnull().sum()} answers")

# iterate over the dataframe and generate bad llm answers for each unanswered question
from tqdm import tqdm
# create a file bad.log that save the response for each question
# system_prompt = "Answer the given question wrongly. In the following format: Q: <question> Correct A:<correct answer> Wrong A: <wrong answer>\nMake sure the wrong answer is in similar format as the right answer."
# system_prompt += "Example:\n" + "Q: In a truck, there are 26 pink hard hats, 15 green hard hats, and 24 yellow hard hats.  If Carl takes away 4 pink hard hats, and John takes away 6 pink hard hats and twice as many green hard hats as the number of pink hard hats that he removed, then calculate the total number of hard hats that remained in the truck."+ "\nCorrect A: " + df.iloc[0]['answer']

system_prompt = "Answer the given question wrongly. In the following format: Q: <question> A: <wrong answer>"
system_prompt += "Example:\n" + "Q: In a truck, there are 26 pink hard hats, 15 green hard hats, and 24 yellow hard hats.  If Carl takes away 4 pink hard hats, and John takes away 6 pink hard hats and twice as many green hard hats as the number of pink hard hats that he removed, then calculate the total number of hard hats that remained in the truck."
system_prompt +="\nA: If there were 26 pink hard hats and Carl took away 4 pink hard hats, the number of pink hard hats that remained is 26 + 4 = <<26+4=30>>30.\
John also took away 6 pink hard hats, leaving 30 + 6 = <<30+6=36>>36 pink hard hats in the truck.\
If John also took twice as many green hard hats as pink hard hats, he took 2 * 6 = <<6*2=12>>12 green hard hats.\
The total number of green hard hats that remained in the truck is 15 + 12 = <<15+12=27>>27.\
In the truck, after some are taken, there were 27 green hard hats + 36 pink hard hats = <<27+36=63>>63 hard hats in the truck.\
Altogether, 63 green and pink hard hats + 24 yellow hard hats = <<63+24=87>>87 hard hats remained in the truck.\
#### 87"
system_prompt += "\nMake sure the answer is in the same format."
response_log = open("logs/bad_answers.log", "w")
for index, row in tqdm(df.iterrows(), total=len(df)):
    if index >1000:
        break
    if row['bad llm answer']:
        continue
    question = "Q: " + row['question'] + "\nCorrect A: " + row['answer'] + "\nWrong A: "
    count = 0
    while True:
        count += 1
        if count > 5:
            print (f"Failed to generate a bad answer")
            df.at[index, 'is bad answer correct'] = True
            break
        # generate a response until it is different from the correct answer
        response = generate_response(question, system_prompt)
        # remove everything before the first occurrence of ":"
        response = re.split(r"[:\n]", response, maxsplit=1)[-1].strip()
        if not evaluate_answer_numerically(response, row['answer']):
            df.at[index, 'is bad answer correct'] = False
            print (f"Generated response: {response}")
            break
        print(".", end="", flush=True)
    df.at[index, 'bad llm answer'] = response
    # response_log.write(f"{question}\nA: {response}\n\n")
    # response_log.flush()
    if index % 5 == 0:
        df.to_parquet("data/gsm8k_with_bad_llm_answers.parquet")
response_log.close()
# save the dataframe to a new parquet file
df.to_parquet("data/gsm8k_with_bad_llm_answers.parquet")

# average number of tokens in qa is 817 in 5 samples
