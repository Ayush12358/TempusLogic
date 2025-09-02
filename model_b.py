from ollama import Client, chat

client = Client('http://localhost:11434')

def generate_response(prompt):
    response = client.chat(
        model='gemma3:270m',
        messages=[{'role': 'user', 'content': prompt}]
    )
    return response['message']['content']
