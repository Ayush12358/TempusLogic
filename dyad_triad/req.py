from generate import generate_response
import requests

a_dyad_is = "A Dyad a logical (mathematical, reasining, IQ etc.) argument where two statements are followed by a question related,whose answer would be a conclusion drawn from the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "
a_triads_is = "A Triad a logical (mathematical, reasining, IQ etc.) argument where three statements are followed by a question related,whose answer would be a conclusion drawn from some or all statements of the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "

models = [
    "openai/gpt-4o",
    "openai/gpt-oss-120b",
    "openai/gpt-5",
    "deepseek/deepseek-v3.2-exp",
    "mistralai/mistral-7b-instruct-v0.1",
    "qwen/qwen3-max",
    "x-ai/grok-4-fast",
    "deepseek/deepseek-v3.2-exp",
    "google/gemini-2.5-flash",
    "google/gemini-2.5-pro",]

def gen_dyad(model):
    message = f"{a_dyad_is}Generate one dyad, then generate a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. Do not generate anything else.' "
    return generate_response(message, model)

def gen_dyad_many(model = "openai/gpt-oss-120b"):
    message = f"{a_dyad_is}Thirty Times, Generate a dyad, then generate a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else.' "
    return generate_response(message, model)

def gen_triad_many(model = "openai/gpt-oss-120b"):
    message = f"{a_triads_is}Thirty Times, Generate a triad, then generate a version of the original dyad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else.' "
    return generate_response(message, model)

def eval_model(dy_or_tri,statements, question, modified_statements, model):
    if dy_or_tri == "dyad":
        arg_def = a_dyad_is
    elif dy_or_tri == "triad":
        arg_def = a_triads_is
    else:
        return "Error: dy_or_tri must be either 'dyad' or 'triad'"
    message = f"{arg_def}Here, you are given a question and statements, and a modified version of the statements. You are to first answer the question based on the original statements and then answer the question based on the sum of the original and modified statements.Give only two answers for the question in one word each, for the original and for the modified statements. the total output is two words. Answer with yes, no, or inconclusive. Do not generate anything else. Here are the statements: {statements} The question: {question} The modified statements: {modified_statements} "
    return generate_response(message, model)


def gen_triad_groq():

    response = requests.post(
      url="https://api.groq.com/openai/v1/chat/completions",
      headers={
        "Authorization": f"Bearer {GROQ}",
        "Content-Type": "application/json",
      },
      data=json.dumps({
        "model": "openai/gpt-oss-120b",
        "messages": [
          {
            "role": "user",
            "content": f"{a_triads_is}Thirty Times, Generate a triad, then generate a version of the original dyad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else.' "
          }
        ]
      })
    )
    try:
        return response.json()['choices'][0]['message']['content']
    except KeyError:
        if 'error' in response.json():
            return f"Error: {response.json()['error']['message']}"
        else:
            return "An unknown error occurred in the API response"



def gen_dyad_groq():

    response = requests.post(
      url="https://api.groq.com/openai/v1/chat/completions",
      headers={
        "Authorization": f"Bearer {GROQ}",
        "Content-Type": "application/json",
      },
      data=json.dumps({
        "model": "openai/gpt-oss-120b",
        "messages": [
          {
            "role": "user",
            "content": f"d{a_dyad_is} 100 times, Generate a dyad, then generate a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else.' "
          }
        ]
      })
    )
    try:
        return response.json()['choices'][0]['message']['content']
    except KeyError:
        if 'error' in response.json():
            return f"Error: {response.json()['error']['message']}"
        else:
            return "An unknown error occurred in the API response"



def gen_dyad_prune_groq():
    tmp = open("data/temp.txt").read()
    response = requests.post(
      url="https://api.groq.com/openai/v1/chat/completions",
      headers={
        "Authorization": f"Bearer {GROQ}",
        "Content-Type": "application/json",
      },
      data=json.dumps({
        "model": "openai/gpt-oss-120b",
        "messages": [
          {
            "role": "user",
            "content": f"d{a_dyad_is} is given 100 times where first a language model Generated a dyad, then generated a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and gave the new conclusionr modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else. Now, out of these 100 dyads, pick the best 75 whcih are the most abstract; i.e. which dyads are the least biased towards real-world scenarios and are more logical and reasoning based, such that the model cannot answer using its general knowledge and must use logic and reasining instead only return the 75 dyads and nothing else "
          }
        ]
      })
    )
    try:
        return response.json()['choices'][0]['message']['content']
    except KeyError:
        if 'error' in response.json():
            return f"Error: {response.json()['error']['message']}"
        else:
            return "An unknown error occurred in the API response"
