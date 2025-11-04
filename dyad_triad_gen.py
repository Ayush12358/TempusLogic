from req import gen_dyad, gen_dyad_groq, gen_triad_groq, models

if __name__ == "__main__":
    # for n in range(5):
    #     for i,model in enumerate(models):
    #         print(f"Model: {model}")
    #         dyad = gen_dyad(model)
    #         print(dyad)
    #         print("\n" + "="*50 + "\n")
    #         open("data/dyads.txt", "a").write(f"Dyad number: {(5*n)+(i+1)}\n{dyad}\n\n{'='*50}\n\n")
    dyad = gen_dyad_groq()
    open("data/dyads_gpt5.txt", "a").write(f"{dyad}\n")
    triad = gen_triad_groq()
    open("data/triads_gpt5.txt", "a").write(f"{dyad}\n")
