# DNMD Funding Metrics Dashboard (Shiny R app)

A local (offline-friendly) Shiny R dashboard for exploring departmental funding applications, including outcomes, project types, applicants, funders, gender summaries, and 12‑month metrics.

This app runs **on your computer** and opens in your web browser.


## Features
- Interactive plots (Plotly) and downloadable outputs
- Tabbed views (e.g., Overview, Project Type, People, Gender, Funders, 12‑month window)
- Upload an Excel file (.xlsx) and explore results with filters
- Export filtered tables as `.csv`


## Requirements (one-time)
- **R** (the engine): https://cran.rstudio.com/  
- **RStudio Desktop** (recommended): https://posit.co/download/rstudio-desktop/ 


## Get the app
1. Open this repository on GitHub.
2. Click **Code → Download ZIP**, then unzip it.

You should end up with a folder that contains:
- `app.R`
- a `www/` folder with images


## Run the app (recommended)
Open **RStudio**, then in the Console run:

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
shiny::runApp("PATH/TO/THE/APP/FOLDER")
