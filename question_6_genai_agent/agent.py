from __future__ import annotations

import json
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import pandas as pd


# ----------------------------
# Utilities
# ----------------------------

def _normalize_str(x: Any) -> str:
    if x is None or (isinstance(x, float) and pd.isna(x)):
        return ""
    return str(x).strip()


def _safe_json_load(s: str) -> Dict[str, Any]:
    """
    Parse a JSON object from an LLM response. Handles:
    - pure JSON
    - JSON wrapped in markdown ```json fences
    - extra text before/after (best-effort extraction)
    """
    s = s.strip()

    # Strip markdown fences if present
    if s.startswith("```"):
        s = s.strip("`")
        s = s.replace("json", "", 1).strip()

    # Try direct parse
    try:
        return json.loads(s)
    except Exception:
        pass

    # Best-effort: extract first {...} block
    start = s.find("{")
    end = s.rfind("}")
    if start != -1 and end != -1 and end > start:
        return json.loads(s[start : end + 1])

    raise ValueError(f"Could not parse JSON from LLM output: {s[:200]}...")


# ----------------------------
# Mock LLM (allowed if no key)
# ----------------------------

def _mock_llm(question: str, allowed_columns: List[str]) -> Dict[str, str]:
    """
    Mock response that still follows the required flow:
    Prompt -> Parse -> Execute.
    This is NOT a rules engine for the whole dataset; it's a minimal stub so the pipeline runs.
    If OPENAI_API_KEY exists, we use the real LLM instead.
    """
    q = question.lower()
    # Minimal heuristic to produce plausible JSON for demo queries.
    # (If you have an API key, you won't use this.)
    if "moderate" in q:
        return {"target_column": "AESEV", "filter_value": "MODERATE"}
    if "severe" in q:
        return {"target_column": "AESEV", "filter_value": "SEVERE"}
    if "mild" in q:
        return {"target_column": "AESEV", "filter_value": "MILD"}
    if "cardiac" in q or "heart" in q:
        return {"target_column": "AESOC", "filter_value": "CARDIAC"}
    # default to term search
    return {"target_column": "AETERM", "filter_value": "HEADACHE"}


# ----------------------------
# Agent
# ----------------------------

@dataclass
class AgentResult:
    target_column: str
    filter_value: str
    n_unique_subjects: int
    subject_ids: List[str]


class ClinicalTrialDataAgent:
    """
    GenAI agent that maps natural language questions to:
      - target_column (e.g., AESEV, AETERM, AESOC)
      - filter_value  (e.g., MODERATE, HEADACHE, CARDIAC)
    then executes a Pandas filter on the AE dataframe and returns:
      - count of unique USUBJID
      - list of matching IDs
    """

    def __init__(
        self,
        ae_df: pd.DataFrame,
        model: str = "gpt-4o-mini",
        use_mock_if_no_key: bool = True,
    ) -> None:
        self.df = ae_df.copy()

        # Normalize key columns to strings if present
        for col in self.df.columns:
            if self.df[col].dtype != "string":
                self.df[col] = self.df[col].astype("string")

        self.model = model
        self.use_mock_if_no_key = use_mock_if_no_key

        self.allowed_columns = [c for c in ["AESEV", "AETERM", "AESOC"] if c in self.df.columns]
        if "USUBJID" not in self.df.columns:
            raise ValueError("USUBJID column is required in adae.csv")

        # Schema definition sent to LLM (Requirement #1)
        self.schema_description = (
            "You are working with a clinical AE dataset (CSV) with columns:\n"
            "- USUBJID: Unique Subject Identifier (string)\n"
            "- AESEV: Adverse Event Severity/Intensity (values like MILD, MODERATE, SEVERE)\n"
            "- AETERM: Adverse Event Term (e.g., Headache, Nausea)\n"
            "- AESOC: System Organ Class / body system (e.g., Cardiac disorders, Skin disorders)\n\n"
            f"Allowed target_column values: {self.allowed_columns}\n"
            "Return STRICT JSON with keys: target_column, filter_value.\n"
            "Do not include any extra keys or text."
        )

    def _call_llm(self, question: str) -> Dict[str, str]:
        api_key = os.getenv("OPENAI_API_KEY", "").strip()

        if not api_key and self.use_mock_if_no_key:
            return _mock_llm(question, self.allowed_columns)

        # Real LLM via LangChain
        from langchain_openai import ChatOpenAI
        from langchain_core.messages import SystemMessage, HumanMessage

        llm = ChatOpenAI(model=self.model, temperature=0)
        msgs = [
            SystemMessage(content=self.schema_description),
            HumanMessage(content=f"User question: {question}\nReturn JSON only."),
        ]
        resp = llm.invoke(msgs)
        parsed = _safe_json_load(resp.content)

        # Validate
        tc = _normalize_str(parsed.get("target_column"))
        fv = _normalize_str(parsed.get("filter_value"))
        if tc not in self.allowed_columns:
            raise ValueError(f"LLM returned invalid target_column='{tc}'. Allowed: {self.allowed_columns}")
        if fv == "":
            raise ValueError("LLM returned empty filter_value.")
        return {"target_column": tc, "filter_value": fv}

    def _execute_filter(self, target_column: str, filter_value: str) -> Tuple[int, List[str]]:
        df = self.df

        tc = target_column
        fv = filter_value.strip()

        if tc not in df.columns:
            raise ValueError(f"Column '{tc}' not found in dataframe.")

        # Execution logic: use the LLM output to decide which column to filter.
        # - AESEV: exact match (case-insensitive)
        # - AETERM/AESOC: substring match (case-insensitive) to handle user phrases
        if tc == "AESEV":
            mask = df[tc].str.upper() == fv.upper()
        else:
            mask = df[tc].str.upper().str.contains(fv.upper(), na=False)

        matched = df.loc[mask, "USUBJID"].dropna().astype("string").str.strip()
        ids = sorted([x for x in matched.unique().tolist() if x != ""])
        return len(ids), ids

    def ask(self, question: str) -> AgentResult:
        # Requirement #2: structured JSON from LLM
        llm_out = self._call_llm(question)
        tc = llm_out["target_column"]
        fv = llm_out["filter_value"]

        # Requirement #3: execute Pandas filter
        n, ids = self._execute_filter(tc, fv)

        return AgentResult(
            target_column=tc,
            filter_value=fv,
            n_unique_subjects=n,
            subject_ids=ids,
        )


def load_adae_csv(path: str | Path) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        raise FileNotFoundError(f"Missing file: {p}")
    return pd.read_csv(p, dtype="string")
