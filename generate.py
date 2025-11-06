import os
import re
import time
from google.generativeai import client, types
from ollama import Client

def generate_response(prompt, model_name):
    # if it has a '/' in its name, assume it's not an Ollama model
    if model_name is not None and '/' in model_name:
        # extract base model name from full path
        base_model_name = model_name.split('/')[1]
        service_name = model_name.split('/')[0]
        if service_name == 'gemini':
            return gemini_generate_response(prompt, model_name=base_model_name, retry=True)
    client_ollama = Client()
    response = client_ollama.generate(
        model=model_name,
        prompt=prompt
    )
    text = getattr(response, "text", str(response))
    return text

def gemini_generate_response(prompt, model_name=None, max_retries=5, retry=True):
    try:
        return g_generate_response(prompt, model_name=model_name)
    except Exception as e:
        if retry:
            time.sleep(2)
            return gemini_generate_response(prompt, model_name=model_name, max_retries=max_retries, retry=False)
        else:
            raise e

def g_generate_response(prompt, model_name=None):
    response = client.models.generate_content(
        model=model_name,
        contents=prompt
    )
    text = getattr(response, "text", str(response))
    return text

def test_generate_response():
    try:
        response = generate_response('What is 2 + 2?')
    except Exception as e:
        print(f"Error during testing generate_response.")
        return
    print (f"Working!!")
