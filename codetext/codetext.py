import re
import random # FOr dummy function
from generate import generate_response as generate_text
from tqdm import tqdm

# --- Imports from other files ---
try:
    from codetext.obfuscated_smol_generator import ObfuscatedSmolGenerator
except ImportError:
    print("Error: Could not import ObfuscatedSmolGenerator.")
    print("Ensure 'obfuscated_smol_generator.py' is in the same directory.")
    exit(1)

try:
    from codetext.interpreter import get_ground_truth
except ImportError:
    print("Error: Could not import get_ground_truth from 'interpreter.py'.")
    print("Ensure 'interpreter.py' is in the same directory.")
    exit(1)

# --- Dummy LLM Function ---
# This is a placeholder for your actual LLM call.
# The user prompt mentions `from generate import generate_text`
# We will create a dummy version here for testing.


# --- Helper to load smol.md ---
def load_syntax_guide(filepath="codetext/smol.md"):
    """Loads the smol.md syntax guide."""
    try:
        with open(filepath, 'r') as f:
            return f.read()
    except FileNotFoundError:
        print(f"Error: Could not find '{filepath}'.")
        print("Please ensure 'smol.md' is in the same directory.")
        return None

# --- Main Benchmark Function ---

def run_codetext(llm: list):
    """
    Takes a list of LLM identifiers, runs the benchmark,
    and prints the results.
    """
    
    # 1. Load the syntax guide
    syntax_guide = load_syntax_guide()
    if not syntax_guide:
        return

    # 2. Initialize the code generator
    generator = ObfuscatedSmolGenerator(max_depth=3, max_statements_per_block=3)
    
    # print("--- Starting Smol Context Rot Benchmark ---")
    
    # 3. Generate a test case
    # print("Generating smol code...")
    smol_code = generator.generate_program()
    # print("\n--- Generated Code ---")
    # print(smol_code)
    # print("------------------------")

    # 4. Get the ground truth from the interpreter
    # print("Interpreting code to get ground truth...")
    ground_truth = get_ground_truth(smol_code)
    
    # if "INTERPRETER_ERROR" in ground_truth:
    #     print(f"\n--- BENCHMARK FAILED ---")
    #     print(f"The interpreter failed on the generated code: {ground_truth}")
    #     print("This is a bug in the generator or interpreter.")
    #     print("--------------------------")
    #     return

    # print(f"\n--- Ground Truth Output ---")
    # print(ground_truth)
    # print("---------------------------")
    
    # 5. Build the prompt for the LLM
    # This prompt contains the rules (syntax guide) and the code.
    llm_prompt = f"""
You are an expert code analyst. You will be given the syntax for a strange, obfuscated programming language called 'smol'.
You will then be given a 'smol' program.
Your task is to analyze the code and determine its final output.
The output will be 'truth', 'false', or an integer.

--- SMOL LANGUAGE SYNTAX ---
{syntax_guide}
--- END OF SYNTAX ---

--- SMOL PROGRAM TO ANALYZE ---
{smol_code}
--- END OF PROGRAM ---

Based on the syntax guide, what is the *exact* final output of this program?
Only provide the final output ('truth', 'false', or a number) and nothing else.
"""

    # 6. Run the benchmark against each LLM
    # print("\n--- Running LLM Comparisons ---")
    results = {}
    # This is the real call
    prediction = generate_text(llm_prompt, llm)
    
    clean_prediction = prediction.strip() 
    
    is_correct = (clean_prediction == ground_truth)
    results[llm] = {
        "prediction": clean_prediction,
        "correct": is_correct
    }
    # return 0 or 1 based on correctness
    score = 1.0 if is_correct else 0.0
    return score

def runs_codetext(llm: list) -> list:
    sum =0.0
    num_trials = 50
    for _ in tqdm(range(num_trials)):
        sum += run_codetext(llm)
    average_score = sum / num_trials
    return average_score


def running_codetext(llms: list) -> list:
    """
    Runs the codetext benchmark on a list of LLMs.
    Returns a list of scores (1.0 for correct, 0.0 for incorrect).
    """
    scores = []
    for llm in llms:
        print(f"\nEvaluating LLM: {llm}")
        result = runs_codetext(llm)
        scores.append(result)
    return scores
