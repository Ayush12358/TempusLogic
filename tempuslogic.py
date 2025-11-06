from gsm8k.gsm8k import running_gsm8k
from dyad_triad.dyad_triad import running_dyad_triad
from coding_test.coding_test import running_coding_test
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import argparse

def run_tests(llms, show_plot=False, dummy_scores=False):
    # run gsm8k tests
    if not(dummy_scores):
        gsm = running_gsm8k(llms=llms)
        # run dyad_triad tests
        dyad = running_dyad_triad(llms=llms)
        # run coding_test tests
        coding = running_coding_test(llms=llms)
    else:
        gsm = np.array([0.75, 0.72, 0.70, 0.68])  # dummy scores
        dyad = np.array([0.80, 0.78, 0.76, 0.74])  # dummy scores
        coding = np.array([0.65, 0.63, 0.60, 0.58])  # dummy scores
    avg = (gsm + dyad + coding) / 3
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
    llms = ["gpt-oss:120b-cloud", "glm-4.6:cloud", "deepseek-v3.1:671b-cloud", "kimi-k2:1t-cloud"]
    # args parser
    parser = argparse.ArgumentParser()
    parser.add_argument('--models', nargs='+', default=llms, help='List of model names to test')
    parser.add_argument('--show_plot', action='store_true', help='Show the performance plot')
    parser.add_argument('--dummy_scores', action='store_true', help='Use dummy scores for testing')
    parser.add_argument('--remove_duplicates', action='store_true', help='Remove duplicate rankings')
    parser.add_argument('--show_rankings', action='store_true', help='Show the model rankings')
    parser.add_argument('--run_tests', action='store_true', help='Run the tests')
    parser.add_argument('--clear_rankings', action='store_true', help='Clear all rankings')
    
    args = parser.parse_args()
    
    if args.clear_rankings:
        clear_rankings()
    if args.run_tests:
        run_tests(args.models, show_plot=args.show_plot, dummy_scores=args.dummy_scores)
    if args.remove_duplicates:
        remove_duplicate_rankings()
    if args.show_rankings:
        show_rankings()

