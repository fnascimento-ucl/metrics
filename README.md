# DNMD Funding Metrics Dashboard (Shiny R app)

A local (offline-friendly) Shiny R dashboard for exploring departmental funding applications, including outcomes, project types, applicants, funders, gender summaries, and 12‑month metrics.

This app runs **on your computer** and opens in your web browser.


## Features
- Interactive plots (Plotly) and downloadable outputs
- Tabbed views (e.g., Overview, Project Type, People, Gender, Funders, 12‑month view)
- Upload an Excel file (.xlsx) and explore results with filters
- Export filtered tables as `.csv`


## Requirements (one-time)
You need:
- **R** installed (the engine): https://cran.rstudio.com/ [1](https://cran.rstudio.org/bin/windows/)[2](https://www.tutorialkart.com/r-tutorial/install-r-on-windows/)  
- **RStudio Desktop** installed (recommended): https://posit.co/download/rstudio-desktop/ [3](https://github.com/DavidASmith/r-shiny-docker-renv)  


## Get the app (no Git needed)
### Option A — Download ZIP from GitHub (simplest)
1. On GitHub, open this repository.
2. Click **Code → Download ZIP**, then unzip it. [4](https://search.r-project.org/CRAN/refmans/renv/html/restore.html)  

You should end up with a folder that contains:
- `app.R`
- (optionally) a `www/` folder with images/assets

### Option B — Download from Releases (recommended for “stable” versions)
If this repository uses GitHub Releases:
1. Go to **Releases**
2. Download **Source code (zip)** or a provided asset ZIP. [4](https://search.r-project.org/CRAN/refmans/renv/html/restore.html)  


## Install required R packages (first run)
Open **RStudio**, then run this in the Console:

install.packages(c(
  "shiny","readxl","janitor","dplyr","stringr","lubridate",
  "tidyr","scales","DT","shinyjs","plotly","ggplot2"
))
