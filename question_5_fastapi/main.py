from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Optional

import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field

APP_DIR = Path(__file__).resolve().parent
DATA_PATH = APP_DIR / "adae.csv"

app = FastAPI(title="Clinical Trial Data API")


def load_adae() -> pd.DataFrame:
    if not DATA_PATH.exists():
        raise FileNotFoundError(f"Missing required file: {DATA_PATH}")

    df = pd.read_csv(DATA_PATH)

    # Normalize expected columns for Q5
    # - Subject id should be USUBJID
    # - Severity should be AESEV
    # - Treatment arm: prefer ACTARM; if missing, backfill from common ADaM vars
    required = ["USUBJID", "AESEV"]
    for col in required:
        if col not in df.columns:
            raise ValueError(f"Required column '{col}' not found in adae.csv")

    if "ACTARM" not in df.columns:
        for fb in ["TRT01A", "TRT01P", "ARM"]:
            if fb in df.columns:
                df["ACTARM"] = df[fb]
                break
    if "ACTARM" not in df.columns:
        # still missing
        df["ACTARM"] = pd.NA

    # Standardize to strings for filtering
    for c in ["USUBJID", "AESEV", "ACTARM"]:
        df[c] = df[c].astype("string").str.strip()

    return df


try:
    ADAE = load_adae()
    STARTUP_ERROR: Optional[str] = None
except Exception as e:
    ADAE = None
    STARTUP_ERROR = str(e)


@app.get("/")
def root() -> Dict[str, str]:
    return {"message": "Clinical Trial Data API is running"}


class AEQueryRequest(BaseModel):
    severity: Optional[List[str]] = Field(default=None, description="List of AESEV values to include")
    treatment_arm: Optional[str] = Field(default=None, description="ACTARM value to include")


class AEQueryResponse(BaseModel):
    record_count: int
    subject_ids: List[str]


@app.post("/ae-query", response_model=AEQueryResponse)
def ae_query(req: AEQueryRequest) -> AEQueryResponse:
    if STARTUP_ERROR is not None or ADAE is None:
        raise HTTPException(status_code=500, detail=f"Dataset load failed: {STARTUP_ERROR}")

    df = ADAE

    if req.severity:
        sev = [str(s).strip() for s in req.severity if s is not None and str(s).strip() != ""]
        if sev:
            df = df[df["AESEV"].isin(sev)]

    if req.treatment_arm is not None and str(req.treatment_arm).strip() != "":
        arm = str(req.treatment_arm).strip()
        df = df[df["ACTARM"] == arm]

    record_count = int(len(df))
    subject_ids = sorted(df["USUBJID"].dropna().unique().tolist())

    return AEQueryResponse(record_count=record_count, subject_ids=subject_ids)


def severity_points(aesev: Any) -> int:
    # MILD=1, MODERATE=2, SEVERE=3, else 0
    if aesev is None or pd.isna(aesev):
        return 0
    s = str(aesev).strip().upper()
    if s == "MILD":
        return 1
    if s == "MODERATE":
        return 2
    if s == "SEVERE":
        return 3
    return 0


@app.get("/subject-risk/{subject_id}")
def subject_risk(subject_id: str) -> Dict[str, Any]:
    if STARTUP_ERROR is not None or ADAE is None:
        raise HTTPException(status_code=500, detail=f"Dataset load failed: {STARTUP_ERROR}")

    sid = str(subject_id).strip()
    if sid == "":
        raise HTTPException(status_code=400, detail="subject_id must be non-empty")

    df = ADAE[ADAE["USUBJID"] == sid]
    if df.empty:
        raise HTTPException(status_code=404, detail=f"Subject '{sid}' not found")

    score = int(df["AESEV"].map(severity_points).sum())

    # Risk categories: 0-2 Low, 3-5 Medium, >=6 High
    if score <= 2:
        category = "Low"
    elif score <= 5:
        category = "Medium"
    else:
        category = "High"

    return {
        "subject_id": sid,
        "risk_score": score,
        "risk_category": category,
        "ae_record_count": int(len(df)),
    }
