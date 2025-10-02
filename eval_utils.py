
CLASS_ORDER = ["yes", "no", "inconclusive"]
CLASS_INDEX = {c: i for i, c in enumerate(CLASS_ORDER)}

def normalize_label(raw: str) -> str:
    """
    Normalize any raw answer into one of: yes, no, inconclusive.
    Accepts:
      - First letter codes: Y / N / I
      - Full words (case-insensitive)
      - Falls back to 'inconclusive'
    """
    if not raw:
        return "inconclusive"
    s = raw.strip().lower()
    if not s:
        return "inconclusive"

    # If it's a combined string like "YN" (original + modified) the caller should slice before passing here.
    first = s[0]
    if first == 'y':
        return "yes"
    if first == 'n':
        return "no"
    if first == 'i':
        return "inconclusive"

    # Word-based
    if s.startswith("yes"):
        return "yes"
    if s.startswith("no"):
        return "no"
    if s.startswith("inconclusive"):
        return "inconclusive"

    # Look for keyword inside
    if "inconclusive" in s:
        return "inconclusive"
    if "yes" in s:
        return "yes"
    if "no" in s:
        return "no"

    return "inconclusive"

def build_confusion(gold, pred) -> list[list[int]]:
    """
    Build a 3x3 confusion matrix in order:
        rows    = gold (true) label index
        columns = predicted label index
    """
    matrix = [[0 for _ in CLASS_ORDER] for _ in CLASS_ORDER]
    for g, p in zip(gold, pred):
        gi = CLASS_INDEX[g]
        pi = CLASS_INDEX[p]
        matrix[gi][pi] += 1
    return matrix

def per_class_metrics(conf: list[list[int]]) -> dict[str, dict[str, float]]:
    """
    Compute precision, recall, support for each class.
    precision_c = TP / (TP + FP)
    recall_c    = TP / (TP + FN)
    """
    metrics: dict[str, dict[str, float]] = {}
    for c, label in enumerate(CLASS_ORDER):
        tp = conf[c][c]
        fp = sum(conf[r][c] for r in range(len(CLASS_ORDER)) if r != c)
        fn = sum(conf[c][r] for r in range(len(CLASS_ORDER)) if r != c)
        denom_p = tp + fp
        denom_r = tp + fn
        precision = tp / denom_p if denom_p > 0 else 0.0
        recall = tp / denom_r if denom_r > 0 else 0.0
        support = tp + fn
        metrics[label] = {
            "precision": precision,
            "recall": recall,
            "support": support
        }
    return metrics

def overall_accuracy(conf: list[list[int]]) -> float:
    total = sum(sum(row) for row in conf)
    if total == 0:
        return 0.0
    correct = sum(conf[i][i] for i in range(len(conf)))
    return correct / total

def macro_avg(values: dict[str, dict[str, float]], key: str) -> float:
    acc = 0.0
    n = 0
    for cls in CLASS_ORDER:
        if key in values[cls]:
            acc += values[cls][key]
            n += 1
    return acc / n if n else 0.0

def print_metrics(title: str, metrics: dict[str, any]) -> None:
    print(f"\n=== {title} ===")
    print(f"Accuracy: {metrics['accuracy']:.4f}")
    print(f"Macro Precision: {metrics['macro_precision']:.4f}")
    print(f"Macro Recall:    {metrics['macro_recall']:.4f}")
    print("\nPer-class:")
    for cls in CLASS_ORDER:
        pc = metrics["per_class"][cls]
        print(f"  {cls:13s} P={pc['precision']:.4f} R={pc['recall']:.4f} Support={pc['support']}")
    print("\nConfusion Matrix (rows=true, cols=pred):")
    labels = metrics["confusion_matrix"]["labels"]
    matrix = metrics["confusion_matrix"]["matrix"]
    header = "          " + " ".join(f"{l[:3].upper():>5s}" for l in labels)
    print(header)
    for i, row in enumerate(matrix):
        print(f"{labels[i][:3].upper():>10s} " + " ".join(f"{c:5d}" for c in row))


def evaluate(gold_raw, pred_raw) -> dict[str, any]:
    """
    gold_raw: list of raw gold labels (strings)
    pred_raw: list of raw predicted labels
    Returns metrics dict.
    """
    gold = [normalize_label(g) for g in gold_raw]
    pred = [normalize_label(p) for p in pred_raw]

    conf = build_confusion(gold, pred)
    pcm = per_class_metrics(conf)
    acc = overall_accuracy(conf)

    metrics = {
        "accuracy": acc,
        "per_class": pcm,
        "macro_precision": macro_avg(pcm, "precision"),
        "macro_recall": macro_avg(pcm, "recall"),
        "confusion_matrix": {
            "labels": CLASS_ORDER,
            "matrix": conf  # rows=true, cols=pred
        }
    }
    return metrics
