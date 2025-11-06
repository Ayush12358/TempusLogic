import os
import re
import time
from google.generativeai import client, types
from ollama import Client

def generate_response(prompt, model_name):
    start_time = time.time()
    while True:
        try:
            return response(prompt, model_name)
        except Exception as e:
            if time.time() - start_time > 3*1000: # retry for 30 seconds
                raise e
            else:
                continue

def response(prompt, model_name):
    # if it has a '/' in its name, assume it's not an Ollama model
    if model_name is not None and '/' in model_name:
        # extract base model name from full path
        base_model_name = model_name.split('/')[1]
        service_name = model_name.split('/')[0]
        if service_name == 'gemini':
            return gemini_generate_response(prompt,base_model_name)
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
