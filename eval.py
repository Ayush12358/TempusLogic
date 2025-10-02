from parse_utils import parse_dyads, parse_triads
from eval_utils import print_metrics, evaluate
from req import eval_groq
import time

dyads = parse_dyads()
triads = parse_triads()
dyad_ret= []
dyad_mod_ret = []
triad_ret = []
triad_mod_ret = []

CLASS_ORDER = ["yes", "no", "inconclusive"]
CLASS_INDEX = {c: i for i, c in enumerate(CLASS_ORDER)}

if __name__ == "__main__":
    print(len(dyads))
    for i in range(30):
        print(dyads["statements"][i], dyads["questions"][i], dyads["modified_statements"][i])
        both_ans = eval_groq("dyad", dyads["statements"][i], dyads["questions"][i], dyads["modified_statements"][i]).strip().split()
        dyad_ret.append(both_ans[0])
        dyad_mod_ret.append(both_ans[1])
        print("real answers",dyads["answers"][i].lower(),dyads["modified_answers"][i].lower())
        print("returned:",both_ans)
        time.sleep(3)


    for i in range(30):
        print(triads["statements"][i], triads["questions"][i], triads["modified_statements"][i])
        both_ans = eval_groq("triad", triads["statements"][i], triads["questions"][i], triads["modified_statements"][i]).strip()
        triad_ret.append(both_ans[0])
        triad_mod_ret.append(both_ans[1])
        print("real answers",triads["answers"][i].lower(),triads["modified_answers"][i].lower())
        print("returned:",both_ans)
        time.sleep(3)

    print([(x,y) for x,y in zip(dyads["answers"][:30], dyad_ret)])
    print([(x,y) for x,y in zip(dyads["modified_answers"][:30], dyad_mod_ret)])

    print_metrics("Dyads Original", evaluate(dyads["answers"][:30], dyad_ret))
    print_metrics("Dyads modified", evaluate(dyads["modified_answers"][:30], dyad_mod_ret))
    print_metrics("Triads Original", evaluate(triads["answers"][:30], triad_ret))
    print_metrics("Triads modified", evaluate(triads["modified_answers"][:30], triad_mod_ret))
    # print(eval_groq("triad", triads["statements"][0], triads["questions"][0], triads["modified_questions"][0]))
    # print(dyads["answers"][i].lower())
