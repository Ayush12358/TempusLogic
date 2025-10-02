
import re
import json
from pathlib import Path
from typing import Dict, List, Any

# Default data paths (adjust if you relocate the files)
DYAD_DATA_PATH = Path(__file__).parent / "data" / "dyads_gpt5.txt"
TRIAD_DATA_PATH = Path(__file__).parent / "data" / "triads_gpt5.txt"

# =========================
# Shared helpers
# =========================

def _clean_inline(text: str) -> str:
    """Collapse internal whitespace for single-line fields (questions / answers)."""
    return re.sub(r"\s+", " ", text.strip())

def _split_statements(block: str) -> List[str]:
    """
    Convert a numbered statements block into a list of statement strings (numbers removed).
    Lines are expected to start with "<num>. ".
    """
    out: List[str] = []
    for line in block.strip().splitlines():
        m = re.match(r"\d+\.\s*(.*)", line.strip())
        if m:
            stmt = m.group(1).strip()
            if stmt:
                out.append(stmt)
    return out

# =========================
# Dyad parsing
# =========================

# Pattern for ORIGINAL dyads:
# Captures:
#   1. dyad number
#   statements block (enumerated lines)
#   question
#   answer
DYAD_ORIGINAL_PATTERN = re.compile(
    r"\*\*Dyad\s*([0-9]+)\*\*"                # **Dyad N**
    r"(?:[ \t]*\n)+"                          # Blank / line breaks
    r"(?P<statements>(?:\d+\.\s.*?\n)+?)"     # One or more enumerated lines (non-greedy)
    r"\*\*Question:\*\*\s*(?P<question>.*?)\s*\n"
    r"\*\*Answer:\*\*\s*(?P<answer>.*?)\s*(?=\n\n|\n---|\Z)",
    re.DOTALL
)

# Pattern for MODIFIED dyads (tolerant to variants like Modified Dyad N / Modified Dyad N)
DYAD_MODIFIED_PATTERN = re.compile(
    r"\*\*Modified[^\n]*?Dyad\s*([0-9]+)\*\*"
    r"(?:[ \t]*\n)+"
    r"(?P<statements>(?:\d+\.\s.*?\n)+?)"
    r"\*\*Question:\*\*\s*(?P<question>.*?)\s*\n"
    r"\*\*Answer:\*\*\s*(?P<answer>.*?)\s*(?=\n\n|\n---|\Z)",
    re.DOTALL
)

def parse_dyads(path: Path = DYAD_DATA_PATH) -> Dict[str, List[Any]]:
    """
    Parse the dyads file into six parallel arrays:
      {
        "statements":            List[List[str]],
        "modified_statements":   List[List[str]],
        "questions":             List[str],
        "modified_questions":    List[str],
        "answers":               List[str],
        "modified_answers":      List[str]
      }
    Ordering is by ascending dyad number. Missing modified blocks yield empty placeholders
    ( [] for modified_statements, "" for modified_questions / modified_answers ).
    """
    if not path.exists():
        raise FileNotFoundError(f"Dyad data file not found at: {path}")

    text = path.read_text(encoding="utf-8")

    originals: Dict[int, Dict[str, Any]] = {}
    modified: Dict[int, Dict[str, Any]] = {}

    for match in DYAD_ORIGINAL_PATTERN.finditer(text):
        n = int(match.group(1))
        stmts_block = match.group("statements")
        q = match.group("question")
        a = match.group("answer")
        originals[n] = {
            "statements": _split_statements(stmts_block),
            "question": _clean_inline(q),
            "answer": _clean_inline(a),
        }

    for match in DYAD_MODIFIED_PATTERN.finditer(text):
        n = int(match.group(1))
        stmts_block = match.group("statements")
        q = match.group("question")
        a = match.group("answer")
        modified[n] = {
            "statements": _split_statements(stmts_block),
            "question": _clean_inline(q),
            "answer": _clean_inline(a),
        }

    ordered_nums = sorted(originals.keys())

    statements: List[List[str]] = []
    modified_statements: List[List[str]] = []
    questions: List[str] = []
    modified_questions: List[str] = []
    answers: List[str] = []
    modified_answers: List[str] = []

    for n in ordered_nums:
        o = originals[n]
        statements.append(o["statements"])
        questions.append(o["question"])
        answers.append(o["answer"])
        if n in modified:
            m = modified[n]
            modified_statements.append(m["statements"])
            modified_questions.append(m["question"])
            modified_answers.append(m["answer"])
        else:
            modified_statements.append([])
            modified_questions.append("")
            modified_answers.append("")

    return {
        "statements": statements,
        "modified_statements": modified_statements,
        "questions": questions,
        "modified_questions": modified_questions,
        "answers": answers,
        "modified_answers": modified_answers,
    }

# =========================
# Triad parsing
# =========================

# ORIGINAL triad pattern:
# Captures triad number, >=3 enumerated statements, question, answer
TRIAD_ORIGINAL_PATTERN = re.compile(
    r"\*\*Triad\s*([0-9]+)\*\*"
    r"(?:[ \t]*\n)+"
    r"(?P<statements>(?:\d+\.\s.*?\n){3,})"
    r"\*\*Question:\*\*\s*(?P<question>.*?)\s*\n"
    r"\*\*Answer:\*\*\s*(?P<answer>.*?)\s*(?=\n\n|\n\*\*Modified|\n---|\Z)",
    re.DOTALL
)

# MODIFIED triad pattern (no numbering in heading, just **Modified version**)
# Contains enumerated statements and a New conclusion / Answer line
TRIAD_MODIFIED_PATTERN = re.compile(
    r"\*\*Modified version\*\*"
    r"(?:[ \t]*\n)+"
    r"(?P<statements>(?:\d+\.\s.*?\n){3,})"
    r"\*\*(?:New conclusion|Answer):\*\*\s*(?P<answer>.*?)\s*(?=\n---|\Z)",
    re.DOTALL
)

def parse_triads(path: Path = TRIAD_DATA_PATH) -> Dict[str, List[Any]]:
    """
    Parse the triads file into six parallel arrays (same structure as dyads):
      {
        "statements":            List[List[str]],
        "modified_statements":   List[List[str]],
        "questions":             List[str],
        "modified_questions":    List[str],  # always "" (dataset lacks modified questions)
        "answers":               List[str],
        "modified_answers":      List[str]
      }
    Modified blocks only supply new statements and a 'New conclusion' (treated as modified answer).
    """
    if not path.exists():
        raise FileNotFoundError(f"Triad data file not found at: {path}")

    text = path.read_text(encoding="utf-8")

    original_matches = list(TRIAD_ORIGINAL_PATTERN.finditer(text))

    originals: Dict[int, Dict[str, Any]] = {}
    modified: Dict[int, Dict[str, Any]] = {}

    # Store originals with end positions for region slicing
    for match in original_matches:
        triad_num = int(match.group(1))
        stmts_block = match.group("statements")
        q = match.group("question")
        a = match.group("answer")
        originals[triad_num] = {
            "statements": _split_statements(stmts_block),
            "question": _clean_inline(q),
            "answer": _clean_inline(a),
            "end_pos": match.end(),
        }

    # For each original triad, search the following text segment for a modified block
    for idx, match in enumerate(original_matches):
        triad_num = int(match.group(1))
        start_search = match.end()
        if idx + 1 < len(original_matches):
            end_search = original_matches[idx + 1].start()
        else:
            end_search = len(text)
        region = text[start_search:end_search]
        m = TRIAD_MODIFIED_PATTERN.search(region)
        if m:
            stmts_block = m.group("statements")
            mod_answer = m.group("answer")
            modified[triad_num] = {
                "statements": _split_statements(stmts_block),
                "answer": _clean_inline(mod_answer),
            }

    ordered_nums = sorted(originals.keys())

    statements: List[List[str]] = []
    modified_statements: List[List[str]] = []
    questions: List[str] = []
    modified_questions: List[str] = []  # remain empty strings
    answers: List[str] = []
    modified_answers: List[str] = []

    for n in ordered_nums:
        o = originals[n]
        statements.append(o["statements"])
        questions.append(o["question"])
        answers.append(o["answer"])
        if n in modified:
            m = modified[n]
            modified_statements.append(m["statements"])
            modified_answers.append(m["answer"])
            modified_questions.append("")  # dataset has no modified questions
        else:
            modified_statements.append([])
            modified_answers.append("")
            modified_questions.append("")

    return {
        "statements": statements,
        "modified_statements": modified_statements,
        "questions": questions,
        "modified_questions": modified_questions,
        "answers": answers,
        "modified_answers": modified_answers,
    }

# =========================
# Export utility
# =========================

def export_as_json(output_path: Path, data: Dict[str, List[Any]]) -> None:
    """
    Write parsed data dictionary to JSON (UTF-8, pretty-printed).
    """
    output_path.write_text(
        json.dumps(data, ensure_ascii=False, indent=2),
        encoding="utf-8"
    )

# =========================
# Demo / manual test
# =========================

def _demo():
    print("== Dyads Demo ==")
    try:
        dyads = parse_dyads()
        print(f"Dyads parsed: {len(dyads['questions'])}")
        if dyads["questions"]:
            print("First dyad statements:", dyads["statements"][0])
            print("First dyad question:", dyads["questions"][0])
            print("First dyad answer:", dyads["answers"][0])
            print("First dyad modified statements:", dyads["modified_statements"][0])
            print("First dyad modified question:", dyads["modified_questions"][0])
            print("First dyad modified answer:", dyads["modified_answers"][0])
    except FileNotFoundError as e:
        print(f"Dyads: {e}")

    print("\n== Triads Demo ==")
    try:
        triads = parse_triads()
        print(f"Triads parsed: {len(triads['questions'])}")
        if triads["questions"]:
            print("First triad statements:", triads["statements"][0])
            print("First triad question:", triads["questions"][0])
            print("First triad answer:", triads["answers"][0])
            print("First triad modified statements:", triads["modified_statements"][0])
            print("First triad modified question:", triads["modified_questions"][0])
            print("First triad modified answer:", triads["modified_answers"][0])
    except FileNotFoundError as e:
        print(f"Triads: {e}")

if __name__ == "__main__":
    _demo()
