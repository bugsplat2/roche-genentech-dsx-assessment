from pathlib import Path

from agent import ClinicalTrialDataAgent, load_adae_csv


def main() -> None:
    csv_path = Path(__file__).resolve().parent / "adae.csv"
    df = load_adae_csv(csv_path)

    agent = ClinicalTrialDataAgent(df)

    questions = [
        "Give me the subjects who had adverse events of Moderate severity",
        "Which subjects had severe adverse events?",
        "Show me subjects with cardiac adverse events",
    ]

    for q in questions:
        res = agent.ask(q)
        print("=" * 80)
        print("Q:", q)
        print("LLM JSON ->", {"target_column": res.target_column, "filter_value": res.filter_value})
        print("Unique subjects:", res.n_unique_subjects)
        print("Subject IDs (first 20):", res.subject_ids[:20])


if __name__ == "__main__":
    main()
