\# Question 5 - FastAPI Clinical Trial Data API



\## Setup (Windows)

From repo root:



```bat

python -m venv .venv

.venv\\Scripts\\activate

python -m pip install --upgrade pip

pip install -r question\_5\_fastapi\\requirements.txt



\## Run



uvicorn question\_5\_fastapi.main:app --reload



\## Test



curl http://127.0.0.1:8000/



curl -X POST http://127.0.0.1:8000/ae-query ^

&nbsp; -H "Content-Type: application/json" ^

&nbsp; -d "{\\"severity\\":\[\\"MILD\\",\\"SEVERE\\"],\\"treatment\_arm\\":\\"Placebo\\"}"



curl http://127.0.0.1:8000/subject-risk/01-701-1015



