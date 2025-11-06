import time
import os
import re
from google.generativeai import client
from ollama import Client
from openai import OpenAI

def generate_response(prompt, model_name): # handes retries
    start_time = time.time()
    while True:
        try:
            return service_router(prompt, model_name)
        except Exception as e:
            if time.time() - start_time > 3*1000: # retry for 30 seconds
                raise e
            else:
                continue

def service_router(prompt, model_name): # determines which service to use
    # if it has a '/' in its name, assume it's not an Ollama model
    if model_name is not None and '/' in model_name:
        # extract base model name from full path
        base_model_name = model_name.split('/')[1]
        service_name = model_name.split('/')[0]
        if service_name == 'gemini':
            return gemini_generate_response(prompt,base_model_name)
        elif service_name == 'ollama':
            return ollama_generate_response(prompt,base_model_name)
        elif service_name == 'openrouter':
            return openrouter_generate_response(prompt,base_model_name)
        elif service_name == 'openai':
            return openai_generate_response(prompt,base_model_name)
        else:
            print("Service not recognized. Please use 'service/model_name' format.")
    else:
        print("Model name format not recognized. Please use 'service/model_name' format.")

def ollama_generate_response(prompt, model_name):
    client_ollama = Client()
    response = client_ollama.generate(
        model=model_name,
        prompt=prompt
    )
    text = getattr(response, "text", str(response))
    return text
def gemini_generate_response(prompt, model_name):
    response = client.models.generate_content(
        model=model_name,
        contents=prompt
    )
    text = getattr(response, "text", str(response))
    return text
def openrouter_generate_response(prompt, model_name):
    client = OpenAI(
        base_url="https://openrouter.ai/api/v1",
        api_key=os.getenv("OPENROUTER_API_KEY")
    )
    completion = client.chat.completions.create(
        model=model_name,
        messages=[
            {
            "role": "user",
            "content": prompt
            }
        ]
    )
    return completion.choices[0].message.content
def openai_generate_response(prompt, model_name):
    client = OpenAI(api_key=os.getenv("OPENAI_API_KEY_ALT"))
    response = client.responses.create(
        model=model_name,
        input=prompt
    )
    return response.output

