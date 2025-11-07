import requests
import time
from keys import GROQ
import json


a_dyad_is = "A Dyad a logical (mathematical, reasining, IQ etc.) argument where two statements are followed by a question related,whose answer would be a conclusion drawn from the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "
a_triads_is = "A Triad a logical (mathematical, reasining, IQ etc.) argument where three statements are followed by a question related,whose answer would be a conclusion drawn from some or all statements of the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "
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



def gen_dyad_prune_groq(data):
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
            "content": f"d{a_dyad_is} is given 100 times here {data} , where first a language model Generated a dyad, then generated a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and gave the new conclusionr modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else. Now, out of these 100 dyads, pick the best 75 whcih are the most abstract; i.e. which dyads are the least biased towards real-world scenarios and are more logical and reasoning based, such that the model cannot answer using its general knowledge and must use logic and reasining instead only return the 75 dyads and nothing else "
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



def gen_dyad_more_groq():
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
            "content": f"d{a_dyad_is} first a language model Generated a dyad, then generated a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and gave the new conclusionr modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else. Now, out of these 100 dyads, pick the best 75 whcih are the most abstract; i.e. which dyads are the least biased towards real-world scenarios and are more logical and reasoning based, such that the model cannot answer using its general knowledge and must use logic and reasining. Your job is to return 100 more dyads in the same form as the previous 75 dyadven here: {tmp}, i.e. a dyad followed by a modified dyad with two additional statements and a conclusion. "
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
            "content": f"{a_triads_is} 100 Times, Generate a triad, then generate a version of the original dyad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else.' "
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



def gen_triad_prune_groq(data):
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
            "content": f"d{a_triads_is} is given 100 times here {data} , where first a language model Generated a triad, then generated a version of the original triad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and gave the new conclusionr modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else. Now, out of these 100 trids, pick the best 75 whcih are the most abstract; i.e. which triads are the least biased towards real-world scenarios and are more logical and reasoning based, such that the model cannot answer using its general knowledge and must use logic and reasining instead only return the 75 most abstract triads and nothing else "
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


def gen_triad_more_groq():
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
            "content": f"d{a_triads_is} is given 100 times  , where first a language model Generated a triad, then generated a version of the original triad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and gave the new conclusionr modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else. Now, out of these 100 trids, pick the best 75 whcih are the most abstract; i.e. which triads are the least biased towards real-world scenarios and are more logical and reasoning based, such that the model cannot answer using its general knowledge and must use logic and reasining. your job is to return 100 more dyads in the same form as the previous 75 triad given here: {tmp}, i.e. a triad followed by a modified dyad with two additional statements and a conclusion. "
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

if __name__ == "__main__":
    triad = gen_triad_groq()
    for i in range(10):
        time.sleep(1)
        print(triad)
        tmp = open("data/temp.txt","w").write(gen_triad_prune_groq(triad))
        results_iter_d = open("data/results_iter_t.txt","a").write(f"{triad}")
        time.sleep(1)
        triad = gen_triad_more_groq()
