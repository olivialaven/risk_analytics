JJs_take — Practical 1 working folder

Purpose
-------
This folder contains working copies of the Practical 1 solution script and report so your team can develop alternative solutions or annotations without changing the canonical files in `practical_1/`.

Files included
--------------
- `Practical1_solutions.R` — copy of the full solution script. Edit and run here if you want, but note the script assumes shared data/helpers live in `../`.
- `Practical1_report.Rmd` — copy of the RMarkdown report for editing and local re-rendering.
- `Practical1_report_v2.md` — v2 Markdown export for quick editing in VS Code.
- `run_from_JJs_take.ps1` — convenience PowerShell runner to execute script and render reports from the project root (created below).

How to run (recommended)
------------------------
From the project root open PowerShell and run:

.\practical_1\JJs_take\run_from_JJs_take.ps1

This runner will:
- set the working directory to the project root
- call Rscript to run `practical_1/JJs_take/Practical1_solutions.R`
- optionally render `Practical1_report.Rmd` to Markdown and Word

Notes
-----
- Keep data (`River_and_precip_Neuchatel.csv`) and helpers (`JuroExtremes.R`) in the parent `practical_1/` folder so they remain shared.
- Generated outputs (figures, RDS) will be saved to the parent `practical_1/figures/` and `practical_1/practical1_results.rds` to avoid duplication.
- If you want local-only outputs, change the paths at the top of the solution script to point to `JJs_take` subfolders.
