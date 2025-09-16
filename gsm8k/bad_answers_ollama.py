
import os
from ollama import generate
import pandas as pd

model_name = "gemma3:270m"

splits = {'train': 'main/train-00000-of-00001.parquet', 'test': 'main/test-00000-of-00001.parquet'}
df = pd.read_parquet("hf://datasets/openai/gsm8k/" + splits["train"])
# save to parquet
df.to_parquet("data/gsm8k_train.parquet", index=False)

# check if gsm8k_with_bad_llm_answers.parquet exists, if yes, load it and continue from there
df['bad llm answer'] = ''
if os.path.exists("gsm8k_with_bad_llm_answers.parquet"):
    df_existing = pd.read_parquet("gsm8k_with_bad_llm_answers.parquet")
    # copy the existing answers to the new dataframe
    df['bad llm answer'] = df_existing['bad llm answer']
    print(f"Loaded existing file with {df_existing['bad llm answer'].notnull().sum()} answers")

# iterate over the dataframe and generate bad llm answers for each unanswered question
from tqdm import tqdm
# create a file bad.log that save the response for each question
response_log = open("bad_o.log", "w")

for index, row in tqdm(df.iterrows(), total=len(df)):
    if row['bad llm answer'] != '':
        continue  # skip if already has an answer
    question = row['question']
    response = generate(model_name, question)['response']
    df.at[index, 'bad llm answer'] = response
    response_log.write(f"Q: {question}\nA: {response}\n\n")
    response_log.flush()
response_log.close()
# save the dataframe to a new parquet file
df.to_parquet("data/gsm8k_with_bad_llm_answers.parquet")
