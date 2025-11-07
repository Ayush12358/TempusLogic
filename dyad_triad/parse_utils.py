

import re
import json
from pathlib import Path
from typing import Dict, List, Any
from dyad_triad.eval_utils import normalize_label

# Default data paths (adjust if you relocate the files)
DYAD_DATA_PATH = Path(__file__).parent / "data" / "results_iter_d.txt"
TRIAD_DATA_PATH = Path(__file__).parent / "data" / "results_iter_t.txt"

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

def _split_bullets(block: str) -> List[str]:
    """
    Convert a hyphen-bulleted statements block into a list of statement strings (hyphens removed).
    Lines are expected to start with "- ".
    """
    out: List[str] = []
    for line in block.strip().splitlines():
        m = re.match(r"-\s*(.*)", line.strip())
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

# New: results_iter_d pattern (bulleted statements; inline Q/A; optional Modified with inline/bulleted statements)
# Tolerant to extra blank lines and whitespace between sections
DYAD_RESULTS_PATTERN = re.compile(
    r"\*\*(?P<num>\d+)\*\*[ \t]*\n(?:[ \t]*\n)*"
    r"(?P<orig_block>(?:[ \t]*-\s.*?\n(?:[ \t]*\n)*)+)"
    r"(?:[ \t]*\n)*\*\*Question:\*\*\s*(?P<question>.*?)\s*(?:\n[ \t]*)?\*\*Answer:\*\*\s*(?P<answer>.*?)\s*(?:\n|$)"
    r"(?:\s*(?:\n|\r\n)*\*\*Modified:\*\*\s*(?P<mod_first>.*?)\n(?P<mod_block>(?:[ \t]*-\s.*?\n(?:[ \t]*\n)*)+)?(?:[ \t]*\n)*\*\*Answer:\*\*\s*(?P<mod_answer>.*?)\s*(?:\n|$))?",
    re.DOTALL
)

def parse_dyads(path: Path = DYAD_DATA_PATH) -> Dict[str, List[Any]]:
    """
    Parse dyads from either the original dyads_gpt5 format or the new results_iter_d format.

    Returns six parallel arrays:
      {
        "statements":            List[List[str]],
        "modified_statements":   List[List[str]],
        "questions":             List[str],
        "modified_questions":    List[str],
        "answers":               List[str],
        "modified_answers":      List[str]
      }
    Ordering is by ascending dyad number. Missing modified blocks yield empty placeholders.
    """
    if not path.exists():
        raise FileNotFoundError(f"Dyad data file not found at: {path}")

    text = path.read_text(encoding="utf-8")

    originals: Dict[int, Dict[str, Any]] = {}
    modified: Dict[int, Dict[str, Any]] = {}

    # Prefer original GPT-5 format; fall back to results_iter_d format if no matches found,
    # or if the filename indicates results_iter_d explicitly.
    use_results_format = "results_iter_d" in path.name

    if not use_results_format:
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

        if not originals:
            use_results_format = True

    if use_results_format:
        for m in DYAD_RESULTS_PATTERN.finditer(text):
            n = int(m.group("num"))
            orig_stmts = _split_bullets(m.group("orig_block"))
            q = _clean_inline(m.group("question"))
            a = _clean_inline(m.group("answer"))
            originals[n] = {
                "statements": orig_stmts,
                "question": q,
                "answer": a,
            }

            mod_stmts: List[str] = []
            first = m.group("mod_first")
            if first is not None:
                first = first.strip()
                if first:
                    mod_stmts.append(first)
            mod_block = m.group("mod_block")
            if mod_block:
                mod_stmts.extend(_split_bullets(mod_block))
            mod_ans = m.group("mod_answer") or ""
            if mod_stmts or mod_ans:
                modified[n] = {
                    "statements": mod_stmts,
                    "question": "",  # results format lacks modified questions
                    "answer": _clean_inline(mod_ans),
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
        answers.append(normalize_label(o["answer"]))
        if n in modified:
            m = modified[n]
            modified_statements.append(m["statements"])
            # Normalize label even if empty; keep modified_question as "" if absent
            modified_questions.append(_clean_inline(m.get("question", "")) if m.get("question", "") else "")
            modified_answers.append(normalize_label(m["answer"]))
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
    Parse triads from either the original triads_gpt5 format or the new results_iter_t inline format.

    Returns six parallel arrays (same structure as dyads):
      {
        "statements":            List[List[str]],
        "modified_statements":   List[List[str]],
        "questions":             List[str],
        "modified_questions":    List[str],  # remains "" (no modified questions in results format)
        "answers":               List[str],
        "modified_answers":      List[str]
      }
    Modified blocks only supply new statements and a modified answer.
    """
    if not path.exists():
        raise FileNotFoundError(f"Triad data file not found at: {path}")

    text = path.read_text(encoding="utf-8")

    originals: Dict[int, Dict[str, Any]] = {}
    modified: Dict[int, Dict[str, Any]] = {}

    # Try original GPT-5 triad format first (with headings)
    original_matches = list(TRIAD_ORIGINAL_PATTERN.finditer(text))
    if original_matches and "results_iter_t" not in path.name:
        # Store originals
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

        # For each original triad, search following region for a modified block
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
    else:
        # Fallback: results_iter_t inline format with 1., 2., 3. statements and inline Q/A,
        # followed by 4., 5., 6. and a "New Answer:" (or "New Conclusion:") line.
        TRIAD_RESULTS_INLINE_PATTERN = re.compile(
            r"(?P<num>\d+)\.\s*(?P<s1>.*?)\s+2\.\s*(?P<s2>.*?)\s+3\.\s*(?P<s3>.*?)\s+Question:\s*(?P<question>.*?)\s*Answer:\s*(?P<answer>.*?)\s*(?:\n|$)"
            r"(?:\s*4\.\s*(?P<ms1>.*?)\s+5\.\s*(?P<ms2>.*?)\s+6\.\s*(?P<ms3>.*?)\s+(?:New\s+Answer|New\s+Conclusion|New\s+answer):\s*(?P<mod_answer>.*?)\s*(?:\n|$))?",
            re.DOTALL
        )

        for m in TRIAD_RESULTS_INLINE_PATTERN.finditer(text):
            n = int(m.group("num"))
            s1 = _clean_inline(m.group("s1"))
            s2 = _clean_inline(m.group("s2"))
            s3 = _clean_inline(m.group("s3"))
            q = _clean_inline(m.group("question"))
            a = _clean_inline(m.group("answer"))
            originals[n] = {
                "statements": [s1, s2, s3],
                "question": q,
                "answer": a,
            }

            ms1 = m.group("ms1")
            ms2 = m.group("ms2")
            ms3 = m.group("ms3")
            mod_ans = m.group("mod_answer") or ""
            if ms1 and ms2 and ms3 and mod_ans is not None:
                modified[n] = {
                    "statements": [_clean_inline(ms1), _clean_inline(ms2), _clean_inline(ms3)],
                    "answer": _clean_inline(mod_ans),
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
        answers.append(normalize_label(o["answer"]))
        if n in modified:
            m = modified[n]
            modified_statements.append(m["statements"])
            modified_answers.append(normalize_label(m["answer"]))
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
