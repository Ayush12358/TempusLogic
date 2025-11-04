import pandas as pd

df = pd.read_parquet("data/gsm8k_train.parquet")
def count_tokens(text): # number of tokens in text
    from transformers import GPT2TokenizerFast
    tokenizer = GPT2TokenizerFast.from_pretrained("gpt2")
    tokens = tokenizer.encode(text)
    return len(tokens)
def info_tokens(context, i):
    tokens = count_tokens(context)
    print (f'{tokens}  ---- {tokens/(i+1)}')

context = ''
print ('Tokens ---- Average Tokens per sample')
for i in range(len(df)):
    context += f'Q: {df.at[i, "question"]}\nA: {df.at[i, "bad llm answer"]}\n'
    tokens = i*817/5.0 # approx 817 tokens in 5 samples
    # i want to print only when tokens exceed certain thresholds
    # ie 1024000, 512000, 256000, 128000, 64000, 32000, 16000, 8000, 4000
    if tokens > 1024000 and (tokens - 1024000) < 817:
        info_tokens(context, i)
    elif tokens > 512000 and (tokens - 512000) < 817:
        info_tokens(context, i)
    elif tokens > 256000 and (tokens - 256000) < 817:
        info_tokens(context, i)
    elif tokens > 128000 and (tokens - 128000) < 817:
        info_tokens(context, i)
    elif tokens > 64000 and (tokens - 64000) < 817:
        info_tokens(context, i)
    elif tokens > 32000 and (tokens - 32000) < 817:
        info_tokens(context, i)
    elif tokens > 16000 and (tokens - 16000) < 817:
        info_tokens(context, i)
    elif tokens > 8000 and (tokens - 8000) < 817:
        info_tokens(context, i)
    elif tokens > 4000 and (tokens - 4000) < 817:
        info_tokens(context, i)

context = ''
for i in range(25):
    context += f'Q: {df.at[i, "question"]}\nA: {df.at[i, "answer"]}\n'
print (f'\nTokens in first 25 samples: {count_tokens(context)}')
