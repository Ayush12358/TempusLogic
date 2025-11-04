from gsm8k.gsm8k import running_gsm8k
from dyad_triad.dyad_triad import running_dyad_triad
from coding_test.coding_test import running_coding_test
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

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
    df = pd.read_csv("rankings.csv", names=["Model", "GSM8K", "Dyad/Triad", "Coding Test", "Average", "Timestamp"])
    df = df.sort_values(by="Average", ascending=False)
    print("Model Rankings:")
    print(df)

def remove_duplicate_rankings():
    df = pd.read_csv("rankings.csv", names=["Model", "GSM8K", "Dyad/Triad", "Coding Test", "Average", "Timestamp"])
    df = df.drop_duplicates(subset=["Model"], keep="last")
    df.to_csv("rankings.csv", index=False, header=False)

if __name__ == "__main__":
    llms = ["gpt-oss:120b-cloud", "glm-4.6:cloud", "deepseek-v3.1:671b-cloud", "kimi-k2:1t-cloud"]
    run_tests(llms, dummy_scores=True)
    remove_duplicate_rankings()
    show_rankings()
