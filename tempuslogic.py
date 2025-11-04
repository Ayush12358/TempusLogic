from gsm8k.gsm8k import running_gsm8k
from dyad_triad.dyad_triad import running_dyad_triad
from coding_test.coding_test import running_coding_test


if __name__ == "__main__":
    llms = ["gpt-oss:120b-cloud", "glm-4.6:cloud", "deepseek-v3.1:671b-cloud", "kimi-k2:1t-cloud"] 
    
    # run gsm8k tests
    gsm = running_gsm8k(llms=llms)
    # run dyad_triad tests
    dyad = running_dyad_triad(llms=llms)
    # run coding_test tests
    coding = running_coding_test(llms=llms)
    # process the results
    # dummy pass
    print ("All tests completed.")
