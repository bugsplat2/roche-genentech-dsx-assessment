Roche / Genentech DSX Technical Assessment

This repository contains solutions to the Roche–Genentech Data Science Experience (DSX) technical assessment.
Each question is implemented in its own folder, following the instructions provided in the assessment prompt.

The project uses R for Questions 1–4 and Python for Questions 5–6.

Repository Structure

roche-genentech-dsx-assessment/

question_1_descriptiveStats/
R: descriptive statistics and summaries

question_2_sdtm/
R: SDTM dataset creation using CDISC standards

question_3_adam/
R: ADaM dataset creation (ADSL)

question_4_tlg/
R: Tables, Listings, and Figures (TLG)
Outputs are stored in question_4_tlg/output/

question_5_fastapi/
Python: FastAPI clinical data API

question_6_genai_agent/
Python: GenAI / LLM-based clinical data agent

README.md
Project overview and run instructions

LICENSE.md

project.Rproj

Question Summaries
Question 1 – Descriptive Statistics (R)

Computes descriptive statistics from clinical trial data using R.
All code and outputs are contained in the question_1_descriptiveStats folder.

Question 2 – SDTM Dataset Creation (R)

Creates SDTM-compliant datasets using CDISC standards.
Implemented in R using the pharmaversesdtm package.

Question 3 – ADaM Dataset Creation (R)

Creates the ADaM ADSL dataset with derived subject-level variables.
Implemented in R using the pharmaverseadam package.

Question 4 – Tables, Listings, and Figures (R)

Produces adverse event summary tables, listings, and figures.
Outputs include HTML tables and PNG visualizations located in question_4_tlg/output.

Question 5 – FastAPI Clinical Trial Data API (Python)

Implements a REST API that allows querying adverse event data stored in adae.csv.

Features

Query adverse events by severity and/or treatment arm

Return record counts and unique subject identifiers

Compute subject-level risk scores based on AE severity

Setup and Run (Windows)

python -m venv .venv
.venv\Scripts\activate
pip install -r question_5_fastapi\requirements.txt
uvicorn question_5_fastapi.main:app --reload

Available endpoints:

GET /

POST /ae-query

GET /subject-risk/{subject_id}

Detailed usage instructions and example requests are provided in question_5_fastapi/README.md.

Question 6 – GenAI Clinical Data Assistant (Python)

Implements a GenAI-powered agent that converts natural language clinical questions into structured filters applied to adverse event data.

Features

Accepts free-text clinical questions

Uses an LLM (or mock LLM) to produce structured JSON output

Applies Pandas filtering logic to return matching subjects

Includes a test script with multiple example queries

Run Example Queries

.venv\Scripts\activate
pip install -r question_6_genai_agent\requirements.txt
python question_6_genai_agent\run_examples.py

If an OPENAI_API_KEY environment variable is set, a real LLM is used.
Otherwise, a mock LLM is used while preserving the full agent workflow.

Additional details are provided in question_6_genai_agent/README.md.

Environment and Tools
R

Used for Questions 1–4

Executed in Posit Cloud

Key packages: pharmaversesdtm, pharmaverseadam, dplyr, ggplot2

Python

Used for Questions 5–6

Executed locally

Python version 3.10+

Key libraries: fastapi, pandas, langchain, pydantic

Notes

Each question is intentionally self-contained.

Required CSV files for Python questions are included in their respective folders.

All code has been tested locally as described above.
