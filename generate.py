import os
import re
import time

def generate_response(self, prompt, model_name=None):
    if model_name is None:
        model_name = self.model_name
    response = self.client_ollama.generate(
        model=model_name,
        prompt=prompt
    )
    text = getattr(response, "text", str(response))
    return text
def gemini_generate_response(self, prompt, model_name=None, max_retries=5):
    if self.retry:
        return self.c_generate_response(prompt, max_retries=max_retries, model_name=model_name)
    else:
        return self.s_generate_response(prompt, model_name=model_name)
def c_generate_response(self, prompt, max_retries=5, model_name=None):
    # Generate content with retries and simple caching.
    # Respects RetryInfo in error messages if present (e.g. 'retryDelay': '22s').
    # Set environment variable DRY_RUN=1 to avoid making API calls during development.
    if model_name is not None:
        model = model_name
    else:
        model = self.model_name
    # dry-run mode for local testing
    if os.getenv("DRY_RUN") == "1":
        return "DRY_RUN_RESPONSE"

    cache_key = (model, prompt)
    if cache_key in self._response_cache:
        return self._response_cache[cache_key]

    backoff = 1.0
    for attempt in range(1, max_retries + 1):
        try:
            # small pause between requests to help avoid bursting the quota
            time.sleep(0.05)
            response = self.client.models.generate_content(
                model=model,
                contents=prompt
            )
            text = getattr(response, "text", str(response))
            self._response_cache[cache_key] = text
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
def s_generate_response(self, prompt, model_name=None):
    if model_name is None:
        model_name = self.model_name
    response = self.client.models.generate_content(
        model=model_name,
        contents=prompt
    )
    text = getattr(response, "text", str(response))
    return text
def test_generate_response(self):
    try:
        response = self.generate_response('What is 2 + 2?')
    except Exception as e:
        print(f"Error during testing generate_response.")
        return
    print (f"Working!!")
