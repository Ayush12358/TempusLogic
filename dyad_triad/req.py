from generate import generate_response

a_dyad_is = "A Dyad a logical (mathematical, reasining, IQ etc.) argument where two statements are followed by a question related,whose answer would be a conclusion drawn from the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "
a_triads_is = "A Triad a logical (mathematical, reasining, IQ etc.) argument where three statements are followed by a question related,whose answer would be a conclusion drawn from some or all statements of the scenario (yes, no, or inconclusive). The real-world truthfulness of the statements is irrelevant, but they must behold logic. "

def gen_dyad(model):
    message = f"{a_dyad_is}Generate one dyad, then generate a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. Do not generate anything else.' "
    return generate_response(message, model)

def gen_dyad_many(model = "openai/gpt-oss-120b"):
    message = f"{a_dyad_is}Thirty Times, Generate a dyad, then generate a version of the original dyad with two additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified dyad. Do not generate anything else.' "
    return generate_response(message, model)

def gen_triad_many(model = "openai/gpt-oss-120b"):
    message = f"{a_triads_is}Thirty Times, Generate a triad, then generate a version of the original dyad with three additional statements with the context changed in atleast one statement irregardless of it changes the conclusion, and give the new conclusion, modified or not. You do not need to write the original statements in the modified triad. Do not generate anything else.' "
    return generate_response(message, model)

def eval_model(dy_or_tri,statements, question, modified_statements, model = "openai/gpt-oss-120b"):
    if dy_or_tri == "dyad":
        arg_def = a_dyad_is
    elif dy_or_tri == "triad":
        arg_def = a_triads_is
    else:
        return "Error: dy_or_tri must be either 'dyad' or 'triad'"
    message = f"{arg_def}Here, you are given a question and statements, and a modified version of the statements. You are to first answer the question based on the original statements and then answer the question based on the sum of the original and modified statements.Give only two answers for the question in one word each, for the original and for the modified statements. the total output is two words. Answer with yes, no, or inconclusive. Do not generate anything else. Here are the statements: {statements} The question: {question} The modified statements: {modified_statements} "
    return generate_response(message, model)
