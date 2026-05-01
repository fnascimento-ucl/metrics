# ============================
# DNMD Funding Metrics Shiny — Single Script (stable)
# - Time aggregation affects ALL plots
# - Funders: Plotly vs ggplot toggle (only visible on Funders tab)
# - 1-year anchor only visible on 1-year tab (but input always exists)
# - project_description toggle only visible on Data tab (but input always exists)
# - Percent labels on stacked bars always shown (annotation traces)
# ============================

required_pkgs <- c(
  "shiny","readxl","janitor","dplyr","stringr","lubridate",
  "tidyr","scales","DT","shinyjs","plotly","ggplot2"
)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_pkgs)

suppressPackageStartupMessages({
  lapply(required_pkgs, library, character.only = TRUE)
})


# ============================
# Helpers (FULL)
# ============================

# --- Safe HTML entity conversion (for dev only) ---
# In interactive dev, it fixes app.R if it contains < > &.
# In non-interactive/deployed contexts it will NOT write to disk (safer).
maybe_convert_html_entities <- function(app_dir = getwd()) {
  path <- file.path(app_dir, "app.R")
  if (!file.exists(path)) return(invisible(FALSE))
  txt <- readLines(path, warn = FALSE)
  
  has_entities <- any(grepl("<|>|&|<|>|&", txt))
  if (!has_entities) return(invisible(FALSE))
  
  if (!interactive()) {
    stop(
      "app.R contains HTML entities (< / > / &). Convert before deployment.\n",
      call. = FALSE
    )
  }
  
  # Convert double-escaped first, then normal
  txt <- gsub("<",  "<", txt, fixed = TRUE)
  txt <- gsub(">",  ">", txt, fixed = TRUE)
  txt <- gsub("&", "&", txt, fixed = TRUE)
  
  txt <- gsub("<",  "<", txt, fixed = TRUE)
  txt <- gsub(">",  ">", txt, fixed = TRUE)
  txt <- gsub("&", "&", txt, fixed = TRUE)
  
  writeLines(txt, path)
  message("Converted HTML entities in ", path)
  invisible(TRUE)
}
try(maybe_convert_html_entities(getwd()), silent = TRUE)

# --- Cleaning helpers ---
map_outcome <- function(success_status) {
  x <- stringr::str_to_lower(stringr::str_squish(as.character(success_status)))
  dplyr::case_when(
    x == "successful"   ~ "AWARDED",
    x == "unsuccessful" ~ "REJECTED",
    x == "pending"      ~ "PENDING",
    TRUE                ~ "OTHER"
  )
}

outcome_label <- function(code) {
  dplyr::recode(code,
                AWARDED="Successful",
                REJECTED="Unsuccessful",
                PENDING="Pending",
                OTHER="Other",
                .default = as.character(code)
  )
}

clean_gender <- function(g) {
  x <- toupper(stringr::str_squish(as.character(g)))
  dplyr::case_when(
    x %in% c("M","MALE")   ~ "M",
    x %in% c("F","FEMALE") ~ "F",
    TRUE                  ~ NA_character_
  )
}

flag_fellowship_0405 <- function(project_type) {
  x <- stringr::str_to_lower(stringr::str_squish(as.character(project_type)))
  stringr::str_detect(x, "^0[45]\\.\\s*fellowship")
}

project_type_group <- function(project_type) {
  x  <- stringr::str_squish(as.character(project_type))
  xl <- stringr::str_to_lower(x)
  dplyr::case_when(
    stringr::str_detect(xl, "^04\\.\\s*fellowship") ~ "Fellowship (Clinical)",
    stringr::str_detect(xl, "^05\\.\\s*fellowship") ~ "Fellowship (Non-Clinical)",
    stringr::str_detect(xl, "standard research project / programme") ~ "Standard Research Project/Programme",
    stringr::str_detect(xl, "studentship") ~ "Studentship",
    stringr::str_detect(xl, "framework") ~ "Framework funding",
    stringr::str_detect(xl, "contract") ~ "Contract research",
    TRUE ~ "Other"
  )
}

scope_label <- function(mode) {
  switch(mode,
         "FELLOWSHIPS"     = "Scope: Fellowships only (Project Type 04/05)",
         "NON_FELLOWSHIPS" = "Scope: Non-fellowships (everything except 04/05)",
         "ALL"             = "Scope: All applications",
         "CUSTOM"          = "Scope: Custom selection",
         paste0("Scope: ", mode)
  )
}

palette_choices <- function(name = "UCL") {
  switch(name,
         "UCL"        = c(AWARDED="#4C78A8", REJECTED="#E45756", PENDING="#F58518", OTHER="#999999"),
         "Colorblind" = c(AWARDED="#0072B2", REJECTED="#D55E00", PENDING="#E69F00", OTHER="#999999"),
         "Dark"       = c(AWARDED="#72B7B2", REJECTED="#E45756", PENDING="#F58518", OTHER="#B5B5B5"),
         c(AWARDED="#4C78A8", REJECTED="#E45756", PENDING="#F58518", OTHER="#999999")
  )
}

safe_year_range <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(2020, 2026))
  c(min(x), max(x))
}

wrap_label <- function(x, width = 26) stringr::str_wrap(as.character(x), width = width)

safe_rate <- function(num, den) dplyr::if_else(den > 0, num / den, NA_real_)

pad_range <- function(mx, factor = 1.15) {
  if (is.null(mx) || length(mx) == 0 || is.na(mx) || !is.finite(mx) || mx <= 0) return(c(0, 1))
  c(0, mx * factor)
}

info_icon <- function(text) {
  tags$details(class="info", tags$summary("i"), tags$div(text))
}

pl_layout <- function(fig, base_font, title=NULL, subtitle=NULL,
                      xlab=NULL, ylab=NULL, legend_title=NULL,
                      margin_l=70, margin_r=30, margin_t=85, margin_b=65) {
  ttl <- if (!is.null(subtitle) && nzchar(subtitle)) {
    list(text = paste0(title, "<br><span style='font-size:0.85em;opacity:0.85;'>", subtitle, "</span>"))
  } else list(text = title)
  
  fig %>% plotly::layout(
    title  = ttl,
    font   = list(size = base_font + 2),
    xaxis  = list(title = xlab, automargin = TRUE),
    yaxis  = list(title = ylab, automargin = TRUE),
    legend = list(title = list(text = legend_title)),
    margin = list(l=margin_l, r=margin_r, t=margin_t, b=margin_b)
  )
}

# Stacked bar with % labels (midpoint text)
stacked_bar_with_pct_labels <- function(d, xcol, fillcol, ycol="n",
                                        palette = NULL,
                                        label_fun = NULL,
                                        title="", subtitle="",
                                        xlab="", ylab="", legend_title="",
                                        base_font=13,
                                        show_pct=TRUE,
                                        min_label_share=0.06) {
  
  d <- d %>% dplyr::mutate(
    x    = as.character(.data[[xcol]]),
    fill = as.character(.data[[fillcol]]),
    n    = as.numeric(.data[[ycol]])
  )
  
  if (!("hover" %in% names(d))) d$hover <- paste0(d$fill, "<br>Count: ", d$n)
  d$hover <- as.character(d$hover)
  
  d <- d %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(total = sum(n), share = dplyr::if_else(total > 0, n/total, 0)) %>%
    dplyr::arrange(x, fill) %>%
    dplyr::mutate(cum = cumsum(n), mid = cum - n/2) %>%
    dplyr::ungroup()
  
  fills <- unique(d$fill)
  fig <- plotly::plot_ly()
  
  for (fv in fills) {
    dd <- d[d$fill == fv, , drop=FALSE]
    if (nrow(dd) == 0) next
    
    col <- NULL
    if (!is.null(palette) && !is.null(names(palette)) && fv %in% names(palette)) col <- unname(palette[fv])
    nm  <- if (!is.null(label_fun)) label_fun(fv) else fv
    
    fig <- fig %>% plotly::add_bars(
      data = dd, x = ~x, y = ~n,
      name = nm,
      marker = if (!is.null(col)) list(color=col) else NULL,
      hovertext = ~hover, hoverinfo = "text"
    )
  }
  
  fig <- fig %>% plotly::layout(barmode="stack")
  
  if (isTRUE(show_pct)) {
    lab <- d %>% dplyr::filter(share >= min_label_share, n > 0)
    if (nrow(lab) > 0) {
      fig <- fig %>% plotly::add_text(
        data = lab,
        x = ~x, y = ~mid,
        text = ~paste0(round(100*share), "%"),
        textfont = list(color="white", size=max(9, base_font - 1)),
        showlegend = FALSE, hoverinfo = "skip"
      )
    }
  }
  
  ymax <- max(d %>% dplyr::group_by(x) %>% dplyr::summarise(tt=sum(n), .groups="drop") %>% dplyr::pull(tt), na.rm=TRUE)
  fig  <- fig %>% plotly::layout(yaxis=list(range=pad_range(ymax,1.15), automargin=TRUE))
  
  pl_layout(fig, base_font, title=title, subtitle=subtitle, xlab=xlab, ylab=ylab, legend_title=legend_title)
}

# Multiseries plot with OPTIONAL percent labels on bars (this fixes your missing % problem)
plot_multiseries_year <- function(df, xcol, ycol, groupcol,
                                  style = c("lines","bars"),
                                  title = "", subtitle = "",
                                  xlab = "", ylab = "", legend_title = "",
                                  base_font = 13,
                                  max_series = 8,
                                  value_is_rate = FALSE,
                                  show_bar_labels = FALSE) {
  
  style <- match.arg(style)
  
  # Standardise columns
  d <- df %>%
    dplyr::mutate(
      x   = suppressWarnings(as.integer(.data[[xcol]])),
      y   = suppressWarnings(as.numeric(.data[[ycol]])),
      grp = as.character(.data[[groupcol]])
    ) %>%
    dplyr::filter(!is.na(x), !is.na(y), !is.na(grp))
  
  # Top N series (optional)
  if (!is.null(max_series) && is.finite(max_series)) {
    top <- d %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(tot = sum(y, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(tot)) %>%
      dplyr::slice_head(n = max_series) %>%
      dplyr::pull(grp)
    d <- d %>% dplyr::filter(grp %in% top)
  }
  
  # Hover text
  d <- d %>%
    dplyr::mutate(
      hover = if (isTRUE(value_is_rate)) {
        paste0("Year: ", x, "<br>", grp, "<br>Value: ", scales::percent(y, accuracy = 0.1))
      } else {
        paste0("Year: ", x, "<br>", grp, "<br>Value: ", y)
      }
    )
  
  yrs <- sort(unique(d$x))
  
  # ---- LINES ----
  if (style == "lines") {
    
    fig <- plotly::plot_ly(
      d,
      x = ~x, y = ~y,
      color = ~grp,
      type = "scatter", mode = "lines+markers",
      hovertext = ~hover, hoverinfo = "text"
    )
    
    # Fix 2020.5 ticks: show only real years
    fig <- fig %>% plotly::layout(
      xaxis = list(
        tickmode = "array",
        tickvals = yrs,
        ticktext = yrs
      )
    )
    
    # Y axis formatting
    if (isTRUE(value_is_rate)) {
      fig <- fig %>% plotly::layout(
        yaxis = list(tickformat = ".0%", range = c(0, 1), automargin = TRUE)
      )
    } else {
      fig <- fig %>% plotly::layout(
        yaxis = list(range = pad_range(max(d$y, na.rm = TRUE), 1.15), automargin = TRUE)
      )
    }
    
    return(
      pl_layout(fig, base_font,
                title = title, subtitle = subtitle,
                xlab = xlab, ylab = ylab, legend_title = legend_title,
                margin_r = 180)
    )
  }
  
  # ---- BARS ----
  # Create labels PER ROW (prevents the "female labels on male bars" bug)
  d <- d %>%
    dplyr::mutate(
      label = if (isTRUE(show_bar_labels)) {
        if (isTRUE(value_is_rate)) scales::percent(y, accuracy = 0.1) else as.character(y)
      } else ""
    )
  
  fig <- plotly::plot_ly(
    d,
    x = ~factor(x),
    y = ~y,
    color = ~grp,
    type = "bar",
    hovertext = ~hover, hoverinfo = "text",
    text = ~label,
    textposition = if (isTRUE(show_bar_labels)) "outside" else "none",
    cliponaxis = FALSE
  ) %>% plotly::layout(barmode = "group")
  
  # Y axis formatting
  if (isTRUE(value_is_rate)) {
    fig <- fig %>% plotly::layout(
      yaxis = list(tickformat = ".0%", range = c(0, 1.1), automargin = TRUE)
    )
  } else {
    fig <- fig %>% plotly::layout(
      yaxis = list(range = pad_range(max(d$y, na.rm = TRUE), 1.15), automargin = TRUE)
    )
  }
  
  pl_layout(fig, base_font,
            title = title, subtitle = subtitle,
            xlab = xlab, ylab = ylab, legend_title = legend_title,
            margin_r = 180)
}


# ============================
# UI (FULL, includes intro animation overlay)
# ============================

STARTUP_SPLASH_MS  <- 3000
LOAD_HIDE_DELAY_MS <- 2500
TOP_N_PEOPLE <- 25
UCL_PURPLE   <- "#4B2E83"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(tags$style(HTML("
    details.info { display:inline-block; margin-left:6px; vertical-align:middle; }
    details.info > summary {
      list-style:none; cursor:pointer;
      display:inline-flex; align-items:center; justify-content:center;
      width:16px; height:16px; border-radius:50%;
      background:#444; color:#fff; font-size:12px; font-weight:700;
      user-select:none;
    }
    details.info > summary::-webkit-details-marker { display:none; }
    details.info > div {
      position:absolute; background:#222; color:#fff;
      padding:8px 10px; font-size:12px; border-radius:6px;
      max-width:320px; z-index:9999; margin-top:6px;
    }

    #loading-overlay { position: fixed; inset: 0; z-index: 9999; background: #000;
      display: flex; align-items: center; justify-content: center; padding: 22px 18px; }
    #loading-overlay .wrap { width: min(1120px, 96vw); display: grid; grid-template-rows: auto auto; gap: 16px; }
    #loading-overlay .top { display: grid; grid-template-columns: 1.35fr 1fr; align-items: center; gap: 18px; min-height: 240px; }
    #loading-overlay .dept { font-size: 20px; font-weight: 850; margin: 0 0 10px 0; color: #fff; }
    #loading-overlay .appname { font-size: 34px; font-weight: 950; margin: 0 0 10px 0; color: #fff; line-height: 1.05; }
    #loading-overlay .tagline { font-size: 15px; opacity: 0.92; margin: 0 0 14px 0; color: #fff; }
    #loading-overlay .loadingtitle { font-size: 16px; font-weight: 850; margin: 0; color: #fff; opacity: 0.95; }
    #loading-overlay .dots { display: inline-flex; gap: 8px; margin-top: 10px; }
    #loading-overlay .dot { width: 10px; height: 10px; border-radius: 50%; background: rgba(255,255,255,0.92); animation: bounce 0.9s infinite ease-in-out; }
    #loading-overlay .dot:nth-child(2) { animation-delay: 0.15s; opacity: 0.72; }
    #loading-overlay .dot:nth-child(3) { animation-delay: 0.30s; opacity: 0.55; }
    @keyframes bounce { 0%,80%,100%{transform:translateY(0)} 40%{transform:translateY(-8px)} }

    #loading-overlay .banner { height: 240px; position: relative; }
    #loading-overlay .banner-slide { position: absolute; inset: 0; background-repeat: no-repeat; background-position: center; background-size: contain;
      animation: bannerSwap 12s infinite steps(1, end); filter: drop-shadow(0 10px 20px rgba(0,0,0,0.70)); }
    @keyframes bannerSwap {
      0%,33.333%{background-image:url('ucl200.png')}
      33.334%,66.666%{background-image:url('queensquare.jpg')}
      66.667%,100%{background-image:url('graysinn.jpg')}
    }
    #loading-overlay .banner-slide::after { content:''; position:absolute; inset:0; animation: bannerFlicker 1.2s infinite steps(1, end); }
    @keyframes bannerFlicker { 0%{opacity:1} 10%{opacity:.72} 20%{opacity:1} 55%{opacity:.85} 70%{opacity:1} 100%{opacity:1} }
    #loading-overlay .neuron { height: 210px; display:flex; align-items:center; justify-content:center; }
    #loading-overlay .neuron img { height: 190px; width: auto; animation: neuronPulse 1.4s infinite steps(1,end); }
    @keyframes neuronPulse { 0%{opacity:.95} 15%{opacity:.75} 30%{opacity:.95} 60%{opacity:.85} 100%{opacity:.95} }
  "))),
  
  tags$div(
    style = sprintf("background:%s; padding:14px 22px; display:flex; align-items:center; gap:18px;", UCL_PURPLE),
    tags$img(src="ucl200.png", height="46px", style="display:block;"),
    tags$div(
      tags$div("Department Neuromuscular Diseases Funding Metrics",
               style="color:white; font-size:24px; font-weight:800; line-height:1.1;"),
      tags$div("Upload an Excel file to generate funding outcome plots and summary tables",
               style="color:rgba(255,255,255,0.85); font-size:13px; margin-top:4px;")
    )
  ),
  
  # Intro overlay
  tags$div(
    id="loading-overlay",
    tags$div(class="wrap",
             tags$div(class="top",
                      tags$div(class="left",
                               tags$p(class="dept","Department of Neuromuscular Diseases – UCL Queen Square Institute of Neurology"),
                               tags$p(class="appname","Funding Metrics Visualiser"),
                               tags$p(class="tagline","Applications • Decisions • Funders • Applicant summaries • Time trends"),
                               tags$p(class="loadingtitle","Loading dashboard…"),
                               tags$div(class="dots", tags$div(class="dot"), tags$div(class="dot"), tags$div(class="dot"))
                      ),
                      tags$div(class="banner", tags$div(class="banner-slide"))
             ),
             tags$div(class="neuron", tags$img(src="ghipy_neuron.webp", alt="Neuron animation"))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel (.xlsx)", accept = c(".xlsx")),
      uiOutput("excel_message"),
      
      conditionalPanel(
        condition = "output.excel_valid == 'true'",
        selectInput("sheet_name", "Sheet", choices = character(0)),
        actionButton("load", "Load data")
      ),
      
      tags$div(style="display:none;", textOutput("excel_valid")),
      tags$div(style="display:none;", textOutput("data_loaded")),
      
      conditionalPanel(
        condition = "output.data_loaded == 'true'",
        hr(),
        selectInput("scope_mode", "Application type",
                    choices = c("FELLOWSHIPS","NON_FELLOWSHIPS","ALL","CUSTOM"),
                    selected = "FELLOWSHIPS"),
        
        conditionalPanel(
          condition = "input.scope_mode == 'CUSTOM'",
          selectizeInput("custom_terms","Custom selection (choose Project Types / Groups)", choices=NULL, multiple=TRUE)
        ),
        
        uiOutput("year_ui"),
        
        radioButtons("time_agg","Show results",
                     choices=c("Show per year"="year","All selected years (combined)"="total"),
                     selected="year"),
        
        radioButtons("multi_year_style","Per-year plot style",
                     choices=c("Bars"="bars","Lines"="lines"),
                     selected="bars"),
        
        sliderInput("top_series_n","Top N series (per-year multi-series plots)", 3, 30, 8, step=1),
        sliderInput("min_label_pct","Min % to label inside stacked bars", 0, 30, 12, step=1),
        checkboxInput("show_counts_in_bars","Show % on stacked bars", TRUE),
        
        sliderInput("base_font","Font size", 10, 22, 13),
        selectInput("palette","Colours", c("UCL","Colorblind","Dark"), selected="UCL"),
        
        selectizeInput("funder_sel","Funder (optional)", choices=NULL, multiple=TRUE),
        checkboxGroupInput("gender_keep","Gender (optional)", choices=c("M","F"), selected=c("M","F")),
        checkboxGroupInput("outcomes_keep","Outcomes to include",
                           choices=c("Successful"="AWARDED","Unsuccessful"="REJECTED","Pending"="PENDING"),
                           selected=c("AWARDED","REJECTED","PENDING")),
        
        shinyjs::hidden(
          tags$div(id="one_year_controls",
                   hr(),
                   radioButtons(
                     "one_year_anchor",
                     "12‑month window starts at:",
                     choices = c(
                       "First application (per person)" = "ever",
                       "First application each year (per person)" = "per_year"
                     ),
                     selected = "ever"
                   ),
                   helpText("This affects the 12‑month metrics: use the first option for a single window per person, or the second to reset the window each year.")
          )
        ),
        
        shinyjs::hidden(
          tags$div(id="funder_controls",
                   hr(),
                   selectInput("funder_plot_mode","Funders plot style",
                               choices=c("Interactive (Plotly)"="plotly","Cleaner static (ggplot)"="ggplot"),
                               selected="plotly"),
                   sliderInput("funder_topn","Funders: show top N (aggregate view)", 5, 100, 25, step=5),
                   checkboxInput("funder_show_all","Funders: show all (messy)", FALSE),
                   sliderInput("funder_min_decided","Funders: minimum decided (success plot)", 0, 50, 5, step=1)
          )
        ),
        
        shinyjs::hidden(
          tags$div(id="data_controls",
                   hr(),
                   checkboxInput("show_description","Include project_description (truncated) in Data tab", FALSE)
          )
        ),
        
        hr(),
        downloadButton("download_tables","Download tables (CSV zip)")
      ),
      width=3
    ),
    
    mainPanel(
      conditionalPanel(
        condition="output.data_loaded == 'true'",
        tabsetPanel(
          id="tabs",
          
          tabPanel("Overview",
                   h4("Outcome counts", info_icon("Per-year: stacked bars or lines. Aggregated: single stacked bar.")),
                   plotlyOutput("p_outcomes", height=420),
                   hr(),
                   h4("Rates", info_icon("Per-year: bars/lines. Aggregated: bars.")),
                   plotlyOutput("p_rates", height=420),
                   hr(),
                   h4("Key totals"),
                   verbatimTextOutput("summary_text")
          ),
          
          tabPanel("Project Type",
                   h4("Applications by Project Type", info_icon("Per-year: stacked bars/lines. Aggregated: stacked bar.")),
                   plotlyOutput("p_ptype_counts", height=520),
                   hr(),
                   h4("Success rate by Project Type", info_icon("Decided only. Bars show % labels.")),
                   plotlyOutput("p_ptype_success", height=560)
          ),
          
          tabPanel("People",
                   h4("Applications per person-year distribution", info_icon("Bars show % labels.")),
                   plotlyOutput("p_apps_per_person_year", height=520),
                   hr(),
                   h4("Top applicants", info_icon("Aggregate: bars. Per-year: bars/lines.")),
                   plotlyOutput("p_top_people", height=650)
          ),
          
          tabPanel("Gender",
                   h4("Applications by gender", info_icon("Per-year: stacked bars/lines. Aggregated: stacked bar.")),
                   plotlyOutput("p_gender_apps", height=420),
                   hr(),
                   h4("Success rate by gender", info_icon("Decided only. Bars show % labels.")),
                   plotlyOutput("p_gender_success", height=460)
          ),
          
          tabPanel("Funders",
                   h4("Success rate by funder", info_icon("Plotly/ggplot both support per-year and aggregate.")),
                   conditionalPanel("input.funder_plot_mode == 'plotly'", plotlyOutput("p_funder_success_plotly", height=750)),
                   conditionalPanel("input.funder_plot_mode == 'ggplot'", plotOutput("p_funder_success_gg", height=750)),
                   hr(),
                   h4("Application volume by funder", info_icon("Plotly/ggplot both support per-year and aggregate.")),
                   conditionalPanel("input.funder_plot_mode == 'plotly'", plotlyOutput("p_funder_volume_plotly", height=750)),
                   conditionalPanel("input.funder_plot_mode == 'ggplot'", plotOutput("p_funder_volume_gg", height=750))
          ),
          
          tabPanel("1-year window",
                   h4("Funding applications per person within 1 year"),
                   plotlyOutput("p_1y_attempts", height=360),
                   hr(),
                   h4("1‑year success: chance of at least one award"),
                   plotlyOutput("p_1y_success", height=360),
                   hr(),
                   h4("1-year table (per person)"),
                   DTOutput("tbl_1y")
          ),
          
          tabPanel("Data",
                   h4("Filtered data (preview)"),
                   DTOutput("tbl_data")
          )
        )
      )
    )
  )
)

# ============================
# Server (FULL)
# ============================
server <- function(input, output, session) {
  
  excel_valid   <- reactiveVal(FALSE)
  excel_message <- reactiveVal(NULL)
  data_loaded   <- reactiveVal(FALSE)
  raw <- reactiveVal(NULL)
  
  output$excel_valid <- renderText(if (excel_valid()) "true" else "false")
  output$data_loaded <- renderText(if (data_loaded()) "true" else "false")
  outputOptions(output, "excel_valid", suspendWhenHidden = FALSE)
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$excel_message <- renderUI({
    msg <- excel_message()
    if (is.null(msg)) return(NULL)
    tags$div(style="color:#c0392b; font-weight:600; margin-top:6px;", msg)
  })
  
  session$onFlushed(function() {
    shinyjs::delay(STARTUP_SPLASH_MS, shinyjs::hide("loading-overlay"))
  }, once = TRUE)
  
  observeEvent(input$tabs, {
    shinyjs::toggle("one_year_controls", condition = identical(input$tabs, "1-year window"))
    shinyjs::toggle("funder_controls",   condition = identical(input$tabs, "Funders"))
    shinyjs::toggle("data_controls",     condition = identical(input$tabs, "Data"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$file, {
    excel_valid(FALSE); excel_message(NULL); data_loaded(FALSE); raw(NULL)
    req(input$file)
    
    tryCatch({
      sheets <- readxl::excel_sheets(input$file$datapath)
      if (length(sheets) == 0) { excel_message("Not a valid Excel file (no sheets)."); return() }
      updateSelectInput(session, "sheet_name", choices = sheets, selected = sheets[1])
      
      df0 <- readxl::read_excel(input$file$datapath, sheet = sheets[1]) |> janitor::clean_names()
      required <- c("project_type","project_lead","success_status")
      missing  <- setdiff(required, names(df0))
      if (length(missing) > 0) excel_message(paste("Excel file is missing variables:", paste(missing, collapse=", ")))
      else excel_valid(TRUE)
    }, error=function(e) excel_message("Not a valid Excel file or cannot be read."))
  })
  
  observeEvent(input$sheet_name, {
    req(input$file, input$sheet_name)
    excel_valid(FALSE); excel_message(NULL); data_loaded(FALSE); raw(NULL)
    
    tryCatch({
      df0 <- readxl::read_excel(input$file$datapath, sheet = input$sheet_name) |> janitor::clean_names()
      required <- c("project_type","project_lead","success_status")
      missing  <- setdiff(required, names(df0))
      if (length(missing) > 0) excel_message(paste("Selected sheet is missing variables:", paste(missing, collapse=", ")))
      else excel_valid(TRUE)
    }, error=function(e) excel_message("Cannot read the selected sheet."))
  })
  
  output$year_ui <- renderUI({
    df <- raw()
    if (is.null(df)) return(NULL)
    yrs <- safe_year_range(df$year)
    sliderInput("year_range", "Year range", min=yrs[1], max=yrs[2], value=yrs, sep="")
  })
  
  observeEvent(input$load, {
    req(input$file, input$sheet_name)
    req(excel_valid())
    
    shinyjs::show("loading-overlay")
    on.exit(shinyjs::delay(LOAD_HIDE_DELAY_MS, shinyjs::hide("loading-overlay")), add=TRUE)
    
    df0 <- readxl::read_excel(input$file$datapath, sheet = input$sheet_name) |> janitor::clean_names()
    
    has_funder  <- "funder" %in% names(df0)
    has_gender  <- "gender" %in% names(df0)
    has_biddate <- "bid_submitted_date" %in% names(df0)
    has_year    <- "year" %in% names(df0)
    has_desc    <- "project_description" %in% names(df0)
    has_title   <- "project_title" %in% names(df0)
    
    df <- df0 %>%
      mutate(
        project_type = stringr::str_squish(as.character(project_type)),
        project_type_grp = project_type_group(project_type),
        is_fellowship = flag_fellowship_0405(project_type),
        project_lead = stringr::str_squish(as.character(project_lead)),
        funder = if (has_funder) stringr::str_squish(as.character(funder)) else NA_character_,
        gender = if (has_gender) clean_gender(gender) else NA_character_,
        bid_submitted_date = if (has_biddate) suppressWarnings(as.Date(bid_submitted_date)) else as.Date(NA),
        year_raw = if (has_year) stringr::str_squish(as.character(year)) else NA_character_,
        year_num = suppressWarnings(as.integer(year_raw)),
        year = dplyr::coalesce(year_num, lubridate::year(bid_submitted_date)),
        outcome = map_outcome(success_status),
        outcome_display = outcome_label(outcome),
        project_title = if (has_title) as.character(project_title) else NA_character_,
        project_description = if (has_desc) as.character(project_description) else NA_character_
      ) %>%
      filter(!is.na(year), !is.na(project_lead), project_lead != "")
    
    raw(df)
    data_loaded(TRUE)
    
    yrs <- safe_year_range(df$year)
    updateSliderInput(session, "year_range", min=yrs[1], max=yrs[2], value=yrs)
    
    funders <- sort(unique(na.omit(df$funder)))
    updateSelectizeInput(session, "funder_sel", choices=funders, selected=character(0), server=TRUE)
    
    custom_choices <- sort(unique(c(na.omit(df$project_type_grp), na.omit(df$project_type))))
    updateSelectizeInput(session, "custom_terms", choices=custom_choices, selected=character(0), server=TRUE)
  })
  
  subtitle_text <- reactive({
    df <- raw(); req(df)
    yr <- if (!is.null(input$year_range) && length(input$year_range)==2) paste(input$year_range, collapse="–") else "All"
    paste0(scope_label(input$scope_mode), " | Years: ", yr)
  })
  
  filtered <- reactive({
    df <- raw(); req(df)
    
    # Prevent crashes before slider exists
    if (is.null(input$year_range) || length(input$year_range) != 2) return(df)
    
    mode <- input$scope_mode
    
    df <- df %>% filter(dplyr::case_when(
      mode == "FELLOWSHIPS"     ~ is_fellowship,
      mode == "NON_FELLOWSHIPS" ~ !is_fellowship,
      TRUE ~ TRUE
    ))
    
    if (mode == "CUSTOM") {
      terms <- input$custom_terms
      if (!is.null(terms) && length(terms) > 0) {
        df <- df %>% filter(project_type %in% terms | project_type_grp %in% terms)
      }
    }
    
    df <- df %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    
    if (!is.null(input$funder_sel) && length(input$funder_sel) > 0) {
      df <- df %>% filter(!is.na(funder) & funder %in% input$funder_sel)
    }
    
    df <- df %>% filter(is.na(gender) | gender %in% input$gender_keep)
    
    if (!is.null(input$outcomes_keep) && length(input$outcomes_keep) > 0) {
      df <- df %>% filter(outcome %in% input$outcomes_keep)
    }
    
    df
  })
  
  # --- Derived tables ---
  app_year <- reactive({
    df <- filtered()
    df %>% group_by(year) %>%
      summarise(apps_total=n(),
                awarded=sum(outcome=="AWARDED"),
                rejected=sum(outcome=="REJECTED"),
                pending=sum(outcome=="PENDING"),
                decided=awarded+rejected,
                success_decided=safe_rate(awarded, decided),
                pending_share=safe_rate(pending, apps_total),
                .groups="drop") %>% arrange(year)
  })
  
  person_year <- reactive({
    df <- filtered()
    decided <- c("AWARDED","REJECTED")
    df %>%
      group_by(project_lead, year) %>%
      summarise(any_award = any(outcome=="AWARDED"),
                any_decided = any(outcome %in% decided),
                .groups="drop") %>%
      group_by(year) %>%
      summarise(people_decided=sum(any_decided),
                people_awarded=sum(any_award & any_decided),
                person_conv_decided=safe_rate(people_awarded, people_decided),
                .groups="drop") %>% arrange(year)
  })
  
  funder_tbl <- reactive({
    df <- filtered()
    decided <- c("AWARDED","REJECTED")
    df %>% filter(outcome %in% decided, !is.na(funder), funder!="") %>%
      group_by(funder) %>%
      summarise(decided=n(),
                awarded=sum(outcome=="AWARDED"),
                success_rate=safe_rate(awarded, decided),
                .groups="drop") %>% arrange(desc(decided))
  })
  
  top_people_total <- reactive({
    df <- filtered()
    decided <- c("AWARDED","REJECTED")
    df %>% group_by(project_lead) %>%
      summarise(applications=n(),
                awards=sum(outcome=="AWARDED"),
                decided=sum(outcome %in% decided),
                app_success=safe_rate(awards, decided),
                .groups="drop") %>%
      arrange(desc(applications)) %>%
      slice_head(n=TOP_N_PEOPLE)
  })
  
  one_year_person <- reactive({
    df <- filtered()
    decided <- c("AWARDED","REJECTED")
    req("bid_submitted_date" %in% names(df))
    df <- df %>% filter(!is.na(bid_submitted_date))
    
    anchor <- input$one_year_anchor
    if (is.null(anchor)) anchor <- "ever"
    
    if (anchor == "ever") {
      df1 <- df %>%
        arrange(project_lead, bid_submitted_date) %>%
        group_by(project_lead) %>%
        mutate(first_date=min(bid_submitted_date),
               within_1y = bid_submitted_date <= first_date + lubridate::days(365)) %>%
        filter(within_1y) %>% ungroup()
    } else {
      df1 <- df %>%
        arrange(project_lead, year, bid_submitted_date) %>%
        group_by(project_lead, year) %>%
        mutate(first_date=min(bid_submitted_date),
               within_1y = bid_submitted_date <= first_date + lubridate::days(365)) %>%
        filter(within_1y) %>% ungroup()
    }
    
    df1 %>% group_by(project_lead) %>%
      summarise(apps_1y=n(),
                decided_1y=sum(outcome %in% decided),
                awarded_1y=sum(outcome=="AWARDED"),
                person_success_1y=any(outcome=="AWARDED"),
                app_success_1y=safe_rate(awarded_1y, decided_1y),
                .groups="drop")
  })
  
  output$summary_text <- renderText({
    df <- filtered()
    dec <- df %>% filter(outcome %in% c("AWARDED","REJECTED"))
    overall_success <- if (nrow(dec) > 0) mean(dec$outcome=="AWARDED") else NA_real_
    paste0(
      subtitle_text(), "\n",
      "Rows in scope: ", nrow(df), "\n",
      "Unique applicants: ", n_distinct(df$project_lead), "\n",
      "Decided success rate: ", ifelse(is.na(overall_success), "NA", scales::percent(overall_success, accuracy=1))
    )
  })
  
  # --- PLOTS ---
  output$p_outcomes <- renderPlotly({
    df <- filtered()
    pal <- palette_choices(input$palette)
    base_font <- input$base_font
    min_share <- input$min_label_pct / 100
    
    if (input$time_agg == "total") {
      d <- df %>% count(outcome, name="n") %>% mutate(x="Total", fill=outcome,
                                                      hover=paste0(outcome_label(outcome), "<br>Count: ", n))
      stacked_bar_with_pct_labels(d, "x","fill","n", palette=pal, label_fun=outcome_label,
                                  title="Outcome counts", subtitle=subtitle_text(),
                                  xlab="", ylab="Applications", legend_title="Outcome",
                                  base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
    } else {
      if (input$multi_year_style == "lines") {
        d <- df %>% count(year, outcome, name="n")
        plot_multiseries_year(d, "year","n","outcome", style="lines",
                              title="Outcome counts (lines)", subtitle=subtitle_text(),
                              xlab="Year", ylab="Applications", legend_title="Outcome",
                              base_font=base_font, max_series=NULL, value_is_rate=FALSE)
      } else {
        d <- df %>% count(year, outcome, name="n") %>% mutate(x=year, fill=outcome,
                                                              hover=paste0("Year: ", year, "<br>", outcome_label(outcome), "<br>Count: ", n))
        stacked_bar_with_pct_labels(d, "x","fill","n", palette=pal, label_fun=outcome_label,
                                    title="Outcome counts (stacked bars)", subtitle=subtitle_text(),
                                    xlab="Year", ylab="Applications", legend_title="Outcome",
                                    base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
      }
    }
  })
  
  output$p_rates <- renderPlotly({
    base_font <- input$base_font
    a <- app_year()
    p <- person_year()
    
    d <- a %>% left_join(p, by="year") %>%
      transmute(year,
                app_success=success_decided,
                person_conv=person_conv_decided,
                pending_share=pending_share)
    
    if (input$time_agg == "total") {
      overall <- tibble(metric=c("Application success","Person conversion","Pending share"),
                        rate=c(mean(d$app_success,na.rm=TRUE),
                               mean(d$person_conv,na.rm=TRUE),
                               mean(d$pending_share,na.rm=TRUE)))
      fig <- plot_ly(overall, x=~metric, y=~rate, type="bar",
                     text=~scales::percent(rate,0.1), textposition="outside", cliponaxis=FALSE) %>%
        layout(yaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
      pl_layout(fig, base_font, title="Rates (aggregated)", subtitle=subtitle_text(), xlab="", ylab="Rate")
    } else {
      if (input$multi_year_style == "lines") {
        fig <- plot_ly(d, x=~year) %>%
          add_lines(y=~app_success, name="Application success") %>%
          add_lines(y=~person_conv, name="Person conversion") %>%
          add_lines(y=~pending_share, name="Pending share") %>%
          layout(yaxis=list(tickformat=".0%", range=c(0,1), automargin=TRUE))
        pl_layout(fig, base_font, title="Rates (lines)", subtitle=subtitle_text(), xlab="Year", ylab="Rate", legend_title="Metric")
      } else {
        long <- d %>% pivot_longer(cols=c(app_success, person_conv, pending_share),
                                   names_to="metric", values_to="rate") %>%
          mutate(metric=recode(metric,
                               app_success="Application success",
                               person_conv="Person conversion",
                               pending_share="Pending share"))
        fig <- plot_ly(long, x=~factor(year), y=~rate, color=~metric, type="bar",
                       text=~scales::percent(rate,0.1), textposition="outside", cliponaxis=FALSE) %>%
          layout(barmode="group", yaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
        pl_layout(fig, base_font, title="Rates (bars)", subtitle=subtitle_text(), xlab="Year", ylab="Rate", legend_title="Metric")
      }
    }
  })
  
  output$p_ptype_counts <- renderPlotly({
    df <- filtered()
    base_font <- input$base_font
    min_share <- input$min_label_pct / 100
    
    if (input$time_agg == "total") {
      d <- df %>% count(project_type_grp, name="n") %>% mutate(x="Total", fill=project_type_grp)
      stacked_bar_with_pct_labels(d, "x","fill","n",
                                  palette=NULL, label_fun=NULL,
                                  title="Applications by Project Type", subtitle=subtitle_text(),
                                  xlab="", ylab="Applications", legend_title="Project Type",
                                  base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
    } else {
      if (input$multi_year_style == "lines") {
        d <- df %>% count(year, project_type_grp, name="n")
        plot_multiseries_year(d, "year","n","project_type_grp", style="lines",
                              title="Applications by Project Type (lines)", subtitle=subtitle_text(),
                              xlab="Year", ylab="Applications", legend_title="Project Type",
                              base_font=base_font, max_series=input$top_series_n, value_is_rate=FALSE)
      } else {
        d <- df %>% count(year, project_type_grp, name="n") %>% mutate(x=year, fill=project_type_grp)
        stacked_bar_with_pct_labels(d, "x","fill","n",
                                    palette=NULL, label_fun=NULL,
                                    title="Applications by Project Type (stacked bars)", subtitle=subtitle_text(),
                                    xlab="Year", ylab="Applications", legend_title="Project Type",
                                    base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
      }
    }
  })
  
  output$p_ptype_success <- renderPlotly({
    base_font <- input$base_font
    df_dec <- filtered() %>% filter(outcome %in% c("AWARDED","REJECTED"))
    validate(need(nrow(df_dec) > 0, "No decided rows under current filters."))
    
    dy <- df_dec %>% group_by(year, project_type_grp) %>%
      summarise(decided=n(), awarded=sum(outcome=="AWARDED"),
                success=safe_rate(awarded, decided), .groups="drop") %>%
      filter(!is.na(success))
    
    if (input$time_agg == "total") {
      t <- dy %>% group_by(project_type_grp) %>%
        summarise(success=mean(success,na.rm=TRUE), .groups="drop") %>%
        arrange(success)
      
      fig <- plot_ly(t, x=~success, y=~wrap_label(project_type_grp, 30),
                     type="bar", orientation="h",
                     text=~scales::percent(success,0.1),
                     textposition="outside", cliponaxis=FALSE) %>%
        layout(xaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
      
      pl_layout(fig, base_font, title="Success rate by Project Type (aggregated)", subtitle=subtitle_text(),
                xlab="Success rate", ylab="", margin_l=460, margin_r=80)
      
    } else {
      style <- if (input$multi_year_style=="lines") "lines" else "bars"
      
      plot_multiseries_year(
        dy, "year","success","project_type_grp",
        style=style,
        title=paste0("Success rate by Project Type (", style, ")"),
        subtitle=subtitle_text(),
        xlab="Year", ylab="Success rate", legend_title="Project Type",
        base_font=base_font,
        max_series=input$top_series_n,
        value_is_rate=TRUE,
        show_bar_labels = TRUE   # ✅ THIS is the missing piece
      )
    }
  })
  
  output$p_apps_per_person_year <- renderPlotly({
    df <- filtered()
    base_font <- input$base_font
    
    validate(need(nrow(df) > 0, "No rows under current filters."))
    
    ppy <- df %>% group_by(project_lead, year) %>%
      summarise(n_apps=n(), .groups="drop") %>%
      mutate(bucket=dplyr::case_when(n_apps==1~"1", n_apps==2~"2", n_apps==3~"3", n_apps>=4~"4+"))
    
    if (input$time_agg == "total") {
      d <- ppy %>% count(bucket, name="n") %>% mutate(x="Total", fill=bucket,
                                                      hover=paste0("Bucket: ", bucket, "<br>Count: ", n))
      stacked_bar_with_pct_labels(d,"x","fill","n",
                                  palette=NULL,label_fun=NULL,
                                  title="Apps per person-year distribution (aggregated)", subtitle=subtitle_text(),
                                  xlab="", ylab="Person-years", legend_title="Bucket",
                                  base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=0.01)
    } else {
      d <- ppy %>% count(year, bucket, name="n") %>% mutate(year=as.integer(year))
      if (input$multi_year_style=="lines") {
        plot_multiseries_year(d,"year","n","bucket", style="lines",
                              title="Apps per person-year distribution (lines)", subtitle=subtitle_text(),
                              xlab="Year", ylab="Person-years", legend_title="Bucket",
                              base_font=base_font, max_series=NULL, value_is_rate=FALSE)
      } else {
        d2 <- d %>% mutate(x=year, fill=bucket, hover=paste0("Year: ", year, "<br>Bucket: ", bucket, "<br>Count: ", n))
        stacked_bar_with_pct_labels(d2,"x","fill","n",
                                    palette=NULL,label_fun=NULL,
                                    title="Apps per person-year distribution (stacked bars)", subtitle=subtitle_text(),
                                    xlab="Year", ylab="Person-years", legend_title="Bucket",
                                    base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=0.01)
      }
    }
  })
  
  output$p_top_people <- renderPlotly({
    base_font <- input$base_font
    df <- filtered()
    
    if (input$time_agg == "total") {
      t <- top_people_total()
      validate(need(nrow(t) > 0, "No applicants under filters."))
      
      xmax <- max(t$applications, na.rm=TRUE)
      fig <- plot_ly(t, x=~applications, y=~wrap_label(project_lead, 30),
                     type="bar", orientation="h",
                     text=~applications, textposition="outside", cliponaxis=FALSE) %>%
        layout(xaxis=list(range=pad_range(xmax,1.15), automargin=TRUE))
      pl_layout(fig, base_font, title=paste0("Top applicants (top ", TOP_N_PEOPLE, ")"),
                subtitle=subtitle_text(), xlab="Applications", ylab="", margin_l=460, margin_r=80)
    } else {
      top_names <- df %>% count(project_lead, sort=TRUE) %>% slice_head(n=input$top_series_n) %>% pull(project_lead)
      dy <- df %>% filter(project_lead %in% top_names) %>% count(year, project_lead, name="apps")
      style <- if (input$multi_year_style=="lines") "lines" else "bars"
      plot_multiseries_year(dy,"year","apps","project_lead", style=style,
                            title=paste0("Top applicants over time (", style, ")"), subtitle=subtitle_text(),
                            xlab="Year", ylab="Applications", legend_title="Applicant",
                            base_font=base_font, max_series=input$top_series_n, value_is_rate=FALSE)
    }
  })
  
  output$p_gender_apps <- renderPlotly({
    df <- filtered() %>% filter(!is.na(gender))
    base_font <- input$base_font
    validate(need(nrow(df) > 0, "No gender data under filters."))
    min_share <- input$min_label_pct/100
    
    if (input$time_agg=="total") {
      d <- df %>% count(gender, name="n") %>% mutate(x="Total", fill=gender,
                                                     hover=paste0("Gender: ", gender, "<br>Count: ", n))
      stacked_bar_with_pct_labels(d,"x","fill","n",
                                  palette=NULL,label_fun=NULL,
                                  title="Applications by gender (aggregated)", subtitle=subtitle_text(),
                                  xlab="", ylab="Applications", legend_title="Gender",
                                  base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
    } else {
      if (input$multi_year_style=="lines") {
        d <- df %>% count(year, gender, name="n")
        plot_multiseries_year(d,"year","n","gender", style="lines",
                              title="Applications by gender (lines)", subtitle=subtitle_text(),
                              xlab="Year", ylab="Applications", legend_title="Gender",
                              base_font=base_font, max_series=NULL, value_is_rate=FALSE)
      } else {
        d <- df %>% count(year, gender, name="n") %>% mutate(x=year, fill=gender,
                                                             hover=paste0("Year: ", year, "<br>Gender: ", gender, "<br>Count: ", n))
        stacked_bar_with_pct_labels(d,"x","fill","n",
                                    palette=NULL,label_fun=NULL,
                                    title="Applications by gender (stacked bars)", subtitle=subtitle_text(),
                                    xlab="Year", ylab="Applications", legend_title="Gender",
                                    base_font=base_font, show_pct=input$show_counts_in_bars, min_label_share=min_share)
      }
    }
  })
  
  output$p_gender_success <- renderPlotly({
    base_font <- input$base_font
    df_dec <- filtered() %>% filter(outcome %in% c("AWARDED","REJECTED"), !is.na(gender))
    validate(need(nrow(df_dec) > 0, "No decided gender data under current filters."))
    
    dy <- df_dec %>% group_by(year, gender) %>%
      summarise(decided=n(), awarded=sum(outcome=="AWARDED"),
                success=safe_rate(awarded, decided), .groups="drop") %>%
      filter(!is.na(success))
    
    if (input$time_agg == "total") {
      g <- dy %>% group_by(gender) %>% summarise(success=mean(success,na.rm=TRUE), .groups="drop")
      
      fig <- plot_ly(g, x=~gender, y=~success, type="bar",
                     text=~scales::percent(success,0.1),
                     textposition="outside", cliponaxis=FALSE) %>%
        layout(yaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
      
      pl_layout(fig, base_font, title="Success rate by gender (aggregated)", subtitle=subtitle_text(),
                xlab="Gender", ylab="Success rate")
      
    } else {
      style <- if (input$multi_year_style=="lines") "lines" else "bars"
      
      plot_multiseries_year(
        dy, "year","success","gender",
        style=style,
        title=paste0("Success rate by gender (", style, ")"),
        subtitle=subtitle_text(),
        xlab="Year", ylab="Success rate", legend_title="Gender",
        base_font=base_font,
        max_series=NULL,
        value_is_rate=TRUE,
        show_bar_labels = TRUE   # ✅ THIS is the missing piece
      )
    }
  })
  
  # Funders (Plotly)
  output$p_funder_success_plotly <- renderPlotly({
    base_font <- input$base_font
    f <- funder_tbl()
    validate(need(nrow(f)>0, "No funder data."))
    
    f <- f %>% filter(decided >= input$funder_min_decided)
    validate(need(nrow(f)>0, "No funders meet threshold."))
    
    if (!isTRUE(input$funder_show_all)) f <- f %>% arrange(desc(decided)) %>% slice_head(n=input$funder_topn)
    
    if (input$time_agg=="total") {
      ff <- f %>% mutate(fu=wrap_label(funder, 26)) %>% arrange(success_rate)
      fig <- plot_ly(ff, x=~success_rate, y=~fu, type="bar", orientation="h",
                     text=~scales::percent(success_rate,0.1), textposition="outside", cliponaxis=FALSE) %>%
        layout(xaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
      pl_layout(fig, base_font, title="Success rate by funder (aggregated)", subtitle=subtitle_text(),
                xlab="Success rate", ylab="", margin_l=460, margin_r=80)
    } else {
      df_dec <- filtered() %>% filter(outcome %in% c("AWARDED","REJECTED"), !is.na(funder), funder!="")
      keep <- unique(f$funder)
      dy <- df_dec %>% filter(funder %in% keep) %>% group_by(year, funder) %>%
        summarise(decided=n(), awarded=sum(outcome=="AWARDED"),
                  success=safe_rate(awarded, decided), .groups="drop") %>% filter(!is.na(success))
      style <- if (input$multi_year_style=="lines") "lines" else "bars"
      plot_multiseries_year(dy,"year","success","funder", style=style,
                            title=paste0("Funder success over time (", style, ")"), subtitle=subtitle_text(),
                            xlab="Year", ylab="Success rate", legend_title="Funder",
                            base_font=base_font, max_series=input$top_series_n, value_is_rate=TRUE)
    }
  })
  
  output$p_funder_volume_plotly <- renderPlotly({
    base_font <- input$base_font
    df <- filtered() %>% filter(!is.na(funder), funder!="")
    validate(need(nrow(df)>0,"No funder data."))
    
    vol <- df %>% count(funder, sort=TRUE)
    if (!isTRUE(input$funder_show_all)) vol <- vol %>% slice_head(n=input$funder_topn)
    
    if (input$time_agg=="total") {
      v <- vol %>% mutate(fu=wrap_label(funder,26)) %>% arrange(n)
      fig <- plot_ly(v, x=~n, y=~fu, type="bar", orientation="h",
                     text=~n, textposition="outside", cliponaxis=FALSE) %>%
        layout(xaxis=list(range=pad_range(max(v$n,na.rm=TRUE),1.15), automargin=TRUE))
      pl_layout(fig, base_font, title="Funder volume (aggregated)", subtitle=subtitle_text(),
                xlab="Applications", ylab="", margin_l=460, margin_r=80)
    } else {
      keep <- unique(vol$funder)
      dy <- df %>% filter(funder %in% keep) %>% count(year, funder, name="n")
      style <- if (input$multi_year_style=="lines") "lines" else "bars"
      plot_multiseries_year(dy,"year","n","funder", style=style,
                            title=paste0("Funder volume over time (", style, ")"), subtitle=subtitle_text(),
                            xlab="Year", ylab="Applications", legend_title="Funder",
                            base_font=base_font, max_series=input$top_series_n, value_is_rate=FALSE)
    }
  })
  
  # Funders (ggplot) — IMPORTANT: respects time_agg per your requirement
  output$p_funder_success_gg <- renderPlot({
    base_font <- input$base_font
    
    if (input$time_agg == "total") {
      df <- funder_tbl() %>% filter(decided >= input$funder_min_decided)
      validate(need(nrow(df)>0,"No funders meet threshold."))
      if (!isTRUE(input$funder_show_all)) df <- df %>% arrange(desc(decided)) %>% slice_head(n=input$funder_topn)
      df <- df %>% mutate(fu=wrap_label(funder, 28)) %>% arrange(success_rate)
      
      ggplot(df, aes(x=success_rate, y=reorder(fu, success_rate))) +
        geom_col(fill=palette_choices(input$palette)["AWARDED"]) +
        scale_x_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,1.1)) +
        theme_minimal(base_size=base_font) +
        labs(title="Success rate by funder (aggregated)", subtitle=subtitle_text(), x="Success rate", y=NULL)
      
    } else {
      df_dec <- filtered() %>% filter(outcome %in% c("AWARDED","REJECTED"), !is.na(funder), funder!="")
      validate(need(nrow(df_dec)>0,"No decided funder data."))
      
      dy <- df_dec %>% group_by(year, funder) %>%
        summarise(decided=n(), awarded=sum(outcome=="AWARDED"),
                  success=safe_rate(awarded, decided), .groups="drop") %>% filter(!is.na(success))
      
      top <- dy %>% group_by(funder) %>% summarise(decided=sum(decided), .groups="drop") %>%
        arrange(desc(decided)) %>% slice_head(n=input$top_series_n) %>% pull(funder)
      dy <- dy %>% filter(funder %in% top) %>% mutate(fu=wrap_label(funder, 22))
      
      if (input$multi_year_style == "lines") {
        ggplot(dy, aes(x=year, y=success, colour=fu, group=fu)) +
          geom_line(linewidth=1) + geom_point(size=2) +
          scale_y_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,1)) +
          theme_minimal(base_size=base_font) +
          labs(title="Funder success over time (lines)", subtitle=subtitle_text(),
               x="Year", y="Success rate", colour="Funder")
      } else {
        ggplot(dy, aes(x=factor(year), y=success, fill=fu)) +
          geom_col(position="dodge") +
          scale_y_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,1.1)) +
          theme_minimal(base_size=base_font) +
          labs(title="Funder success over time (bars)", subtitle=subtitle_text(),
               x="Year", y="Success rate", fill="Funder")
      }
    }
  })
  
  output$p_funder_volume_gg <- renderPlot({
    base_font <- input$base_font
    df <- filtered() %>% filter(!is.na(funder), funder!="")
    validate(need(nrow(df)>0,"No funder data."))
    
    if (input$time_agg == "total") {
      vol <- df %>% count(funder, sort=TRUE)
      if (!isTRUE(input$funder_show_all)) vol <- vol %>% slice_head(n=input$funder_topn)
      vol <- vol %>% mutate(fu=wrap_label(funder, 28)) %>% arrange(n)
      
      ggplot(vol, aes(x=n, y=reorder(fu, n))) +
        geom_col(fill="#4C78A8") +
        theme_minimal(base_size=base_font) +
        labs(title="Funder volume (aggregated)", subtitle=subtitle_text(), x="Applications", y=NULL)
      
    } else {
      dy <- df %>% count(year, funder, name="n")
      top <- dy %>% group_by(funder) %>% summarise(tot=sum(n), .groups="drop") %>%
        arrange(desc(tot)) %>% slice_head(n=input$top_series_n) %>% pull(funder)
      dy <- dy %>% filter(funder %in% top) %>% mutate(fu=wrap_label(funder, 22))
      
      if (input$multi_year_style == "lines") {
        ggplot(dy, aes(x=year, y=n, colour=fu, group=fu)) +
          geom_line(linewidth=1) + geom_point(size=2) +
          theme_minimal(base_size=base_font) +
          labs(title="Funder volume over time (lines)", subtitle=subtitle_text(), x="Year", y="Applications", colour="Funder")
      } else {
        ggplot(dy, aes(x=factor(year), y=n, fill=fu)) +
          geom_col(position="dodge") +
          theme_minimal(base_size=base_font) +
          labs(title="Funder volume over time (bars)", subtitle=subtitle_text(), x="Year", y="Applications", fill="Funder")
      }
    }
  })
  
  # 1-year plots obey time_agg
  output$p_1y_attempts <- renderPlotly({
    base_font <- input$base_font
    d <- one_year_person()
    validate(need(nrow(d)>0,"No 1-year window data."))
    
    if (input$time_agg=="total") {
      d2 <- d %>% mutate(bucket=dplyr::case_when(apps_1y==1~"1", apps_1y==2~"2", apps_1y==3~"3", apps_1y>=4~"4+")) %>% count(bucket, name="n")
      fig <- plot_ly(d2, x=~bucket, y=~n, type="bar",
                     text=~n, textposition="outside", cliponaxis=FALSE) %>%
        layout(yaxis=list(range=pad_range(max(d2$n,na.rm=TRUE),1.15), automargin=TRUE))
      pl_layout(fig, base_font, title="1-year attempts per person (aggregated)", subtitle=subtitle_text(),
                xlab="Apps within 1 year", ylab="People")
    } else {
      # Per-year: year of first window start
      df0 <- filtered() %>% filter(!is.na(bid_submitted_date))
      validate(need(nrow(df0)>0,"No bid_submitted_date for per-year 1-year view."))
      
      anchor <- input$one_year_anchor; if (is.null(anchor)) anchor <- "ever"
      
      if (anchor=="ever") {
        dfw <- df0 %>% arrange(project_lead, bid_submitted_date) %>% group_by(project_lead) %>%
          mutate(first_date=min(bid_submitted_date), window_year=lubridate::year(first_date),
                 within_1y=bid_submitted_date<=first_date+lubridate::days(365)) %>%
          filter(within_1y) %>% ungroup()
      } else {
        dfw <- df0 %>% arrange(project_lead, year, bid_submitted_date) %>% group_by(project_lead, year) %>%
          mutate(first_date=min(bid_submitted_date), window_year=year,
                 within_1y=bid_submitted_date<=first_date+lubridate::days(365)) %>%
          filter(within_1y) %>% ungroup()
      }
      
      per <- dfw %>% group_by(project_lead, window_year) %>% summarise(apps_1y=n(), .groups="drop") %>%
        mutate(bucket=dplyr::case_when(apps_1y==1~"1", apps_1y==2~"2", apps_1y==3~"3", apps_1y>=4~"4+")) %>%
        count(window_year, bucket, name="n")
      
      if (input$multi_year_style=="lines") {
        fig <- plot_ly(per, x=~window_year, y=~n, color=~bucket, type="scatter", mode="lines+markers")
        pl_layout(fig, base_font, title="1-year attempts per person (by year, lines)", subtitle=subtitle_text(),
                  xlab="Window year", ylab="People", legend_title="Attempts bucket", margin_r=180)
      } else {
        fig <- plot_ly(per, x=~factor(window_year), y=~n, color=~bucket, type="bar",
                       text=~n, textposition="outside", cliponaxis=FALSE) %>%
          layout(barmode="stack", yaxis=list(range=pad_range(max(per$n,na.rm=TRUE),1.15), automargin=TRUE))
        pl_layout(fig, base_font, title="1-year attempts per person (by year, stacked bars)", subtitle=subtitle_text(),
                  xlab="Window year", ylab="People", legend_title="Attempts bucket", margin_r=180)
      }
    }
  })
  
  output$p_1y_success <- renderPlotly({
    base_font <- input$base_font
    d <- one_year_person()
    validate(need(nrow(d)>0,"No 1-year window data."))
    
    if (input$time_agg=="total") {
      d2 <- d %>% mutate(bucket=dplyr::case_when(apps_1y==1~"1", apps_1y==2~"2", apps_1y==3~"3", apps_1y>=4~"4+")) %>%
        group_by(bucket) %>% summarise(success_rate=mean(person_success_1y), .groups="drop")
      fig <- plot_ly(d2, x=~bucket, y=~success_rate, type="bar",
                     text=~scales::percent(success_rate,0.1), textposition="outside", cliponaxis=FALSE) %>%
        layout(yaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
      pl_layout(fig, base_font, title="1-year success vs attempts (aggregated)", subtitle=subtitle_text(),
                xlab="Apps within 1 year", ylab="Person success rate")
    } else {
      df0 <- filtered() %>% filter(!is.na(bid_submitted_date))
      validate(need(nrow(df0)>0,"No bid_submitted_date for per-year 1-year view."))
      
      anchor <- input$one_year_anchor; if (is.null(anchor)) anchor <- "ever"
      
      if (anchor=="ever") {
        dfw <- df0 %>% arrange(project_lead, bid_submitted_date) %>% group_by(project_lead) %>%
          mutate(first_date=min(bid_submitted_date), window_year=lubridate::year(first_date),
                 within_1y=bid_submitted_date<=first_date+lubridate::days(365)) %>%
          filter(within_1y) %>% ungroup()
      } else {
        dfw <- df0 %>% arrange(project_lead, year, bid_submitted_date) %>% group_by(project_lead, year) %>%
          mutate(first_date=min(bid_submitted_date), window_year=year,
                 within_1y=bid_submitted_date<=first_date+lubridate::days(365)) %>%
          filter(within_1y) %>% ungroup()
      }
      
      per <- dfw %>% group_by(project_lead, window_year) %>%
        summarise(apps_1y=n(), person_success_1y=any(outcome=="AWARDED"), .groups="drop") %>%
        mutate(bucket=dplyr::case_when(apps_1y==1~"1", apps_1y==2~"2", apps_1y==3~"3", apps_1y>=4~"4+")) %>%
        group_by(window_year, bucket) %>% summarise(success_rate=mean(person_success_1y), .groups="drop")
      
      if (input$multi_year_style=="lines") {
        fig <- plot_ly(per, x=~window_year, y=~success_rate, color=~bucket, type="scatter", mode="lines+markers") %>%
          layout(yaxis=list(tickformat=".0%", range=c(0,1), automargin=TRUE))
        pl_layout(fig, base_font, title="1-year success vs attempts (by year, lines)", subtitle=subtitle_text(),
                  xlab="Window year", ylab="Person success rate", legend_title="Attempts bucket", margin_r=180)
      } else {
        fig <- plot_ly(per, x=~factor(window_year), y=~success_rate, color=~bucket, type="bar",
                       text=~scales::percent(success_rate,0.1), textposition="outside", cliponaxis=FALSE) %>%
          layout(barmode="group", yaxis=list(tickformat=".0%", range=c(0,1.1), automargin=TRUE))
        pl_layout(fig, base_font, title="1-year success vs attempts (by year, bars)", subtitle=subtitle_text(),
                  xlab="Window year", ylab="Person success rate", legend_title="Attempts bucket", margin_r=180)
      }
    }
  })
  
  output$tbl_1y <- renderDT({
    DT::datatable(one_year_person(), options=list(pageLength=10, scrollX=TRUE))
  })
  
  output$tbl_data <- renderDT({
    df <- filtered()
    keep <- c("year","outcome_display","project_type_grp","project_type","project_title",
              "project_lead","funder","gender","bid_submitted_date")
    keep <- keep[keep %in% names(df)]
    out <- df %>% select(any_of(keep))
    
    if (isTRUE(input$show_description) && "project_description" %in% names(df)) {
      desc <- df$project_description
      desc <- ifelse(is.na(desc), "", desc)
      desc <- stringr::str_replace_all(desc, "\\s+", " ")
      desc <- ifelse(nchar(desc) > 220, paste0(substr(desc, 1, 220), "…"), desc)
      out$project_description_short <- desc
    }
    
    DT::datatable(out, options=list(pageLength=10, scrollX=TRUE, autoWidth=TRUE))
  })
  
  output$download_tables <- downloadHandler(
    filename = function() paste0("tables_", Sys.Date(), ".zip"),
    content = function(file) {
      tmpdir <- tempdir()
      paths <- c(
        app_year = file.path(tmpdir, "app_year.csv"),
        person_year = file.path(tmpdir, "person_year.csv"),
        funder = file.path(tmpdir, "funder.csv"),
        top_people = file.path(tmpdir, "top_people.csv"),
        one_year = file.path(tmpdir, "one_year.csv")
      )
      write.csv(app_year(), paths["app_year"], row.names=FALSE)
      write.csv(person_year(), paths["person_year"], row.names=FALSE)
      write.csv(funder_tbl(), paths["funder"], row.names=FALSE)
      write.csv(top_people_total(), paths["top_people"], row.names=FALSE)
      write.csv(one_year_person(), paths["one_year"], row.names=FALSE)
      zip(file, files = unname(paths), flags = "-j")
    }
  )
}

shinyApp(ui, server)
