from dyad_triad.parse_utils import parse_dyads, parse_triads
from dyad_triad.eval_utils import print_metrics, evaluate
from dyad_triad.req import eval_model
import time
from tqdm import tqdm
import sys
import os
dyads = parse_dyads()
triads = parse_triads()
dyad_ret= []
dyad_mod_ret = []
triad_ret = []
triad_mod_ret = []

CLASS_ORDER = ["yes", "no", "inconclusive"]
CLASS_INDEX = {c: i for i, c in enumerate(CLASS_ORDER)}

def run_dyad_triad(model):
    # print(len(dyads))
    print (f'Running Dyad/Triad test on model: {model}')
    for i in tqdm(range(100)):
        # print(dyads["statements"][i], dyads["questions"][i], dyads["modified_statements"][i])
        both_ans = eval_model("dyad", dyads["statements"][i], dyads["questions"][i], dyads["modified_statements"][i], model)
        try:
            both_ans = both_ans.strip()
            both_ans = both_ans.split()
            if len(both_ans) != 2:
                raise ValueError("Invalid number of answers returned")
        except Exception as e:
            print(len(both_ans))
            print(f"Error parsing answers for dyad index {i}: {both_ans[1].content[0].text}", file=sys.stderr)
            both_ans = both_ans[1].content[0].text.strip().split()
        dyad_ret.append(both_ans)
        dyad_mod_ret.append(both_ans[1])
        # print("real answers",dyads["answers"][i].lower(),dyads["modified_answers"][i].lower())
        # print("returned:",both_ans)
        time.sleep(3)

    for i in tqdm(range(100)):
        # print(triads["statements"][i], triads["questions"][i], triads["modified_statements"][i])
        both_ans = eval_model("triad", triads["statements"][i], triads["questions"][i], triads["modified_statements"][i], model).strip()
        triad_ret.append(both_ans[0])
        triad_mod_ret.append(both_ans[1])
        # print("real answers",triads["answers"][i].lower(),triads["modified_answers"][i].lower())
        # print("returned:",both_ans)
        time.sleep(3)

    # print([(x,y) for x,y in zip(dyads["answers"][:100], dyad_ret)])
    # print([(x,y) for x,y in zip(dyads["modified_answers"][:100], dyad_mod_ret)])

    print_metrics("Dyads Original", evaluate(dyads["answers"][:100], dyad_ret))
    print_metrics("Dyads modified", evaluate(dyads["modified_answers"][:100], dyad_mod_ret))
    print_metrics("Triads Original", evaluate(triads["answers"][:100], triad_ret))
    print_metrics("Triads modified", evaluate(triads["modified_answers"][:100], triad_mod_ret))
    # print(eval_model("triad", triads["statements"][0], triads["questions"][0], triads["modified_questions"][0]), model)
    # print(dyads["answers"][i].lower())
    # calculate a single score based on average of all four accuracies
    dyad_orig_eval = evaluate(dyads["answers"][:100], dyad_ret)
    dyad_mod_eval = evaluate(dyads["modified_answers"][:100], dyad_mod_ret)
    triad_orig_eval = evaluate(triads["answers"][:100], triad_ret)
    triad_mod_eval = evaluate(triads["modified_answers"][:100], triad_mod_ret)
    final_score = (dyad_orig_eval['accuracy'] + dyad_mod_eval['accuracy'] + triad_orig_eval['accuracy'] + triad_mod_eval['accuracy']) / 4
    return final_score

def running_dyad_triad(llms):
    scores = []
    for model in llms:
        scores.append(run_dyad_triad(model=model))
    return scores

if __name__ == "__main__":
    running_dyad_triad()
