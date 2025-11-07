from gsm8k.gsm8k import running_gsm8k
from dyad_triad.dyad_triad import running_dyad_triad
from coding_test.coding_test import running_coding_test
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import argparse

def run_tests(llms, show_plot=False):
    # run tests
    print ("Running test on GSM8K...")
    gsm = running_gsm8k(llms=llms)
    print ("Running test on Dyad/Triad...")
    dyad = running_dyad_triad(llms=llms)
    print ("Running test on Coding Test...")
    coding = running_coding_test(llms=llms)
    # average scores
    avg = np.array(gsm + dyad + coding) / 3
    # avg = np.mean(np.array([gsm, dyad, coding]), axis=0)
    print ("All tests completed.")
    # process the results
    # the three vars have a list of single score ranging from 0 to 1 for each model in llms
    # graph the results
    tests = ['GSM8K', 'Dyad/Triad', 'Coding Test']
    x = np.arange(len(tests))
    width = 0.2
    fig, ax = plt.subplots()
    for i, model in enumerate(llms):
        scores = [gsm[i], dyad[i], coding[i]]
        ax.bar(x + i*width, scores, width, label=model)
    ax.set_ylabel('Scores')
    ax.set_title('Model Performance on Different Tests')
    ax.set_xticks(x + width / 2)
    ax.set_xticklabels(tests)
    ax.legend()
    plt.ylim(0, 1)
    plt.savefig("model_performance.png")
    if show_plot:
        plt.show()

    # add the scores to the ranking.csv
    # model name, gsm score, dyad score, coding score, average score, time added
    rankings = open("rankings.csv", "a")
    for i, model in enumerate(llms):
        from datetime import datetime
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        rankings.write(f"{model},{gsm[i]:.4f},{dyad[i]:.4f},{coding[i]:.4f},{avg[i]:.4f},{now}\n")

def show_rankings():
    df = pd.read_csv("rankings.csv")
    df = df.sort_values(by="Average", ascending=False)
    print("Model Rankings:")
    print(df.to_string(index=False))

def remove_duplicate_rankings():
    df = pd.read_csv("rankings.csv", names=["Model", "GSM8K", "Dyad/Triad", "Coding Test", "Average", "Timestamp"])
    df = df.drop_duplicates(subset=["Model"], keep="last")
    df.to_csv("rankings.csv", index=False, header=False)

def clear_rankings():
    # remove all rows but the header
    rankings = open("rankings.csv", "r")
    lines = rankings.readlines()
    rankings.close()
    rankings = open("rankings.csv", "w")
    if lines:
        rankings.write(lines[0])  # write back the header
    print("All rankings cleared.")

if __name__ == "__main__":
    llms = ["ollama/gpt-oss-120b-cloud", "ollama/glm-4.6-cloud", "ollama/deepseek-v3.1-671b-cloud", "ollama/kimi-k2-1t-cloud"]
    llms = ["groq/openai/gpt-oss-120b", "groq/openai/gpt-oss-20b"]
    llms = ["gemini/models/gemini-flash-lite-latest"]
    # llms = ['gpt-oss:120b-cloud']
    # args parser
    parser = argparse.ArgumentParser()
    parser.add_argument('--models', nargs='+', default=llms, help='List of model names to test: service/model_name')
    parser.add_argument('--show_plot', action='store_true', help='Show the performance plot')
    parser.add_argument('--remove_duplicates', action='store_true', help='Remove duplicate rankings')
    parser.add_argument('--show_rankings', action='store_true', help='Show the model rankings')
    parser.add_argument('--run_tests', action='store_true', help='Run the tests')
    parser.add_argument('--clear_rankings', action='store_true', help='Clear all rankings')
    # parser.add_argument('--api-key', type=str, help='API key for services that require it')
    
    args = parser.parse_args()
    # if args.api_key:
    #     import os
    #     os.environ["ANY_API_KEY"] = args.api_key
    if args.clear_rankings:
        clear_rankings()
    if args.run_tests:
        run_tests(args.models, show_plot=args.show_plot)
    if args.remove_duplicates:
        remove_duplicate_rankings()
    if args.show_rankings:
        show_rankings()

# python tempuslogic.py --run_tests --show_plot --remove_duplicates --show_rankings --models groq/openai/gpt-oss-120b groq/openai/gpt-oss-20b 
