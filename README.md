# metrics
What this is
The DNMD Funding Metrics Dashboard is a small software tool (Shiny R app) that runs on your computer and opens in your web browser.

Quick checklist (you need these once)
 - R installed (the R engine)
- RStudio Desktop installed (recommended interface) 
- The DNMD_funding_metrics app folder downloaded to your computer (see below) 


Step 1 — Download the app (simplest: GitHub ZIP)
- Open the GitHub page for the app.
- Click Code → Download ZIP, then unzip it.

Step 3 — Run the app
- Open RStudio.
-Open the folder you unzipped (it should contain app.R).
- In the Console, run:

  Rshiny::runApp("PATH/TO/DNMD_funding_metrics")
  runApp() is the standard way to launch a Shiny app from a folder or an app.R file.

Offline use 

After you’ve installed the required packages once (while online), the app will typically run offline on the same computer because the packages are already installed locally.
For best reproducibility across machines, we use renv (see below). renv can restore the exact package set from a lockfile and reuses a global package cache where possible. 
- Optional but recommended: “One‑command setup” using renv (more reliable)
  If the app folder contains renv.lock, then in RStudio (while online the first time):
  Rinstall.packages("renv")renv::restore()``Show more lines
  This restores the project library from the lockfile.
  Why this helps: renv maintains a global package cache, so restores are faster and more consistent (and can reuse already cached packages).

Troubleshooting (two common issues)
1) “package ‘xxx’ not found”
→ Install missing packages (or run renv::restore() if provided).
2) App opens at http://127.0.0.1:xxxx
→ That is normal: it means the app is running locally on your computer (not publicly online).

Support
If you get stuck: Filipe Nascimento (f.nascimento@ucl.ac.uk)
