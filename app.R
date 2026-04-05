library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

source("pipeline/01_load.R")
source("pipeline/02_impute.R")
source("pipeline/02b_outliers.R")
source("pipeline/03_deduplicate.R")
source("pipeline/04_standardize.R")
source("pipeline/05_validate.R")
source("pipeline/05b_schema.R")
source("pipeline/05c_regex.R")
source("pipeline/05d_entropy.R")
source("pipeline/07_report.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: #0d0f14;
        font-family: 'Space Grotesk', sans-serif;
        color: #e2e8f0;
        min-height: 100vh;
        overflow-x: hidden;
      }

      body::before {
        content: '';
        position: fixed;
        inset: 0;
        background-image:
          linear-gradient(rgba(99,179,237,0.03) 1px, transparent 1px),
          linear-gradient(90deg, rgba(99,179,237,0.03) 1px, transparent 1px);
        background-size: 40px 40px;
        pointer-events: none;
        z-index: 0;
      }

      .app-wrapper {
        position: relative;
        z-index: 1;
        display: flex;
        min-height: 100vh;
      }

      .sidebar {
        width: 280px;
        min-width: 280px;
        background: #111318;
        border-right: 1px solid #1e2330;
        padding: 0;
        display: flex;
        flex-direction: column;
        position: fixed;
        top: 0; left: 0; bottom: 0;
        overflow-y: auto;
      }

      .sidebar-logo {
        padding: 28px 24px 20px;
        border-bottom: 1px solid #1e2330;
      }

      .logo-icon {
        width: 36px; height: 36px;
        background: linear-gradient(135deg, #63b3ed, #4299e1);
        border-radius: 10px;
        display: flex; align-items: center; justify-content: center;
        font-size: 18px;
        margin-bottom: 12px;
        box-shadow: 0 0 20px rgba(99,179,237,0.3);
      }

      .logo-title {
        font-size: 16px; font-weight: 700;
        color: #f7fafc; letter-spacing: -0.3px;
      }

      .logo-sub {
        font-size: 11px; color: #4a5568; font-weight: 400;
        margin-top: 2px; font-family: 'JetBrains Mono', monospace;
        text-transform: uppercase; letter-spacing: 1px;
      }

      .sidebar-section {
        padding: 20px 24px;
        border-bottom: 1px solid #1e2330;
      }

      .sidebar-label {
        font-size: 10px; font-weight: 600; color: #4a5568;
        text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 10px;
      }

      .form-group { margin: 0 !important; }
      .shiny-input-container { width: 100% !important; }

      .btn-run {
        width: 100%;
        background: linear-gradient(135deg, #3182ce, #2b6cb0);
        border: none; border-radius: 10px; color: white;
        font-family: 'Space Grotesk', sans-serif;
        font-size: 14px; font-weight: 600; padding: 13px 20px;
        cursor: pointer; transition: all 0.2s; letter-spacing: 0.3px;
        box-shadow: 0 4px 15px rgba(49,130,206,0.3);
      }

      .btn-run:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(49,130,206,0.4);
      }

      .status-pill {
        display: inline-flex; align-items: center; gap: 6px;
        padding: 6px 12px; border-radius: 20px; font-size: 12px;
        font-weight: 500; margin-top: 12px; width: 100%; justify-content: center;
      }

      .status-ready  { background: rgba(72,187,120,0.1); color: #68d391; border: 1px solid rgba(72,187,120,0.2); }
      .status-audited{ background: rgba(246,173,85,0.1); color: #f6ad55; border: 1px solid rgba(246,173,85,0.2); }
      .status-waiting{ background: rgba(74,85,104,0.3);  color: #718096; border: 1px solid #2d3748; }

      .dot { width: 6px; height: 6px; border-radius: 50%; }
      .dot-green { background: #68d391; box-shadow: 0 0 6px #68d391; }
      .dot-amber { background: #f6ad55; box-shadow: 0 0 6px #f6ad55; }
      .dot-gray  { background: #4a5568; }

      .btn-file {
        background: #1e2330 !important; border: 1px solid #2d3748 !important;
        border-radius: 8px !important; color: #a0aec0 !important;
        font-size: 12px !important; font-family: 'Space Grotesk', sans-serif !important;
        padding: 10px 14px !important; margin-bottom: 8px !important;
        width: 100% !important; transition: all 0.2s !important; display: block !important;
      }

      .btn-file:hover { background: #252b3b !important; border-color: #4a5568 !important; color: #e2e8f0 !important; }
      .btn-file span { pointer-events: none; }

      .form-control[readonly] {
        background: #1a1d26 !important; border: 1px solid #1e2330 !important;
        border-radius: 8px !important; color: #4a5568 !important;
        font-size: 11px !important; font-family: 'JetBrains Mono', monospace !important;
        padding: 8px 12px !important; margin-top: 6px !important;
      }

      .fix-panel {
        margin-top: 16px; padding: 16px; background: #1a1d26;
        border-radius: 10px; border: 1px solid #2d3748;
      }

      .fix-status { font-size: 13px; font-weight: 600; margin-bottom: 6px; }
      .fix-score  { color: #718096; font-size: 12px; margin-bottom: 14px; }
      .fix-question { font-size: 12px; color: #a0aec0; margin-bottom: 10px; font-weight: 600; }
      .fix-buttons { display: flex; gap: 8px; }

      .btn-yes {
        flex: 1; background: #2f855a !important; color: white !important;
        border: none !important; border-radius: 8px !important; padding: 9px !important;
        font-size: 13px !important; font-weight: 600 !important;
        font-family: 'Space Grotesk', sans-serif !important; cursor: pointer !important;
      }

      .btn-yes:hover { background: #276749 !important; }

      .btn-no {
        flex: 1; background: #1e2330 !important; color: #718096 !important;
        border: 1px solid #2d3748 !important; border-radius: 8px !important;
        padding: 9px !important; font-size: 13px !important;
        font-family: 'Space Grotesk', sans-serif !important; cursor: pointer !important;
      }

      .btn-no:hover { color: #a0aec0 !important; border-color: #4a5568 !important; }

      .main-content { margin-left: 280px; flex: 1; padding: 32px; min-height: 100vh; }

      .page-header { margin-bottom: 28px; }

      .page-title { font-size: 26px; font-weight: 700; color: #f7fafc; letter-spacing: -0.5px; }

      .page-sub { font-size: 13px; color: #4a5568; margin-top: 4px; font-family: 'JetBrains Mono', monospace; }

      .metrics-row {
        display: grid; grid-template-columns: repeat(4, 1fr);
        gap: 16px; margin-bottom: 24px;
      }

      .metric-card {
        background: #111318; border: 1px solid #1e2330;
        border-radius: 14px; padding: 20px 22px;
        position: relative; overflow: hidden; transition: border-color 0.2s;
      }

      .metric-card:hover { border-color: #2d3748; }

      .metric-card::before {
        content: ''; position: absolute; top: 0; left: 0; right: 0; height: 2px;
      }

      .metric-card.blue::before   { background: linear-gradient(90deg, #3182ce, #63b3ed); }
      .metric-card.green::before  { background: linear-gradient(90deg, #38a169, #68d391); }
      .metric-card.purple::before { background: linear-gradient(90deg, #805ad5, #b794f4); }
      .metric-card.amber::before  { background: linear-gradient(90deg, #c05621, #f6ad55); }

      .metric-icon { font-size: 22px; margin-bottom: 12px; display: block; }

      .metric-value {
        font-size: 36px; font-weight: 700; letter-spacing: -1px;
        line-height: 1; margin-bottom: 6px;
      }

      .metric-value.blue   { color: #63b3ed; }
      .metric-value.green  { color: #68d391; }
      .metric-value.purple { color: #b794f4; }
      .metric-value.amber  { color: #f6ad55; }
      .metric-value.red    { color: #fc8181; }
      .metric-value.muted  { color: #2d3748; }

      .metric-label { font-size: 12px; color: #4a5568; font-weight: 500; text-transform: uppercase; letter-spacing: 1px; }

      .score-breakdown {
        background: #111318; border: 1px solid #1e2330;
        border-radius: 14px; padding: 20px 24px; margin-bottom: 24px;
      }

      .score-breakdown-title {
        font-size: 12px; color: #4a5568; font-weight: 600;
        text-transform: uppercase; letter-spacing: 1px; margin-bottom: 16px;
      }

      .score-row { display: flex; align-items: center; gap: 12px; margin-bottom: 10px; }
      .score-row-label { font-size: 12px; color: #718096; width: 130px; flex-shrink: 0; }
      .score-bar-bg { flex: 1; height: 6px; background: #1e2330; border-radius: 3px; overflow: hidden; }
      .score-bar-fill { height: 100%; border-radius: 3px; transition: width 0.6s ease; }
      .score-row-val { font-size: 12px; color: #a0aec0; font-family: 'JetBrains Mono', monospace; width: 45px; text-align: right; flex-shrink: 0; }

      .tab-container {
        background: #111318; border: 1px solid #1e2330;
        border-radius: 16px; overflow: hidden;
      }

      .nav-tabs {
        background: #0d0f14; border-bottom: 1px solid #1e2330 !important;
        padding: 0 8px; display: flex; gap: 4px; flex-wrap: wrap;
      }

      .nav-tabs > li > a {
        background: transparent !important; border: none !important;
        border-bottom: 2px solid transparent !important; border-radius: 0 !important;
        color: #4a5568 !important; font-family: 'Space Grotesk', sans-serif !important;
        font-size: 13px !important; font-weight: 500 !important;
        padding: 14px 16px !important; transition: all 0.2s !important; margin: 0 !important;
      }

      .nav-tabs > li > a:hover { color: #a0aec0 !important; background: transparent !important; }

      .nav-tabs > li.active > a {
        color: #63b3ed !important; border-bottom-color: #63b3ed !important;
        background: transparent !important;
      }

      .tab-content { padding: 24px; }

      .waiting-state { text-align: center; padding: 60px 20px; color: #2d3748; }
      .waiting-icon  { font-size: 48px; margin-bottom: 16px; opacity: 0.3; }
      .waiting-text  { font-size: 14px; font-family: 'JetBrains Mono', monospace; }

      .dataTables_wrapper { color: #e2e8f0 !important; font-size: 13px !important; }

      table.dataTable { background: transparent !important; border-collapse: collapse !important; width: 100% !important; }

      table.dataTable thead th {
        background: #0d0f14 !important; color: #718096 !important;
        border-bottom: 1px solid #1e2330 !important; font-size: 11px !important;
        font-weight: 600 !important; text-transform: uppercase !important;
        letter-spacing: 1px !important; padding: 10px 14px !important;
      }

      table.dataTable tbody tr { background: transparent !important; border-bottom: 1px solid #1a1d26 !important; }
      table.dataTable tbody tr:hover td { background: #1a1d26 !important; }

      table.dataTable tbody td {
        color: #cbd5e0 !important; padding: 10px 14px !important; border: none !important;
        font-family: 'JetBrains Mono', monospace !important; font-size: 12px !important;
      }

      .dataTables_info, .dataTables_length label,
      .dataTables_filter label, .dataTables_paginate { color: #4a5568 !important; font-size: 12px !important; }

      .dataTables_filter input {
        background: #1a1d26 !important; border: 1px solid #2d3748 !important;
        border-radius: 6px !important; color: #e2e8f0 !important;
        padding: 4px 10px !important; font-size: 12px !important;
      }

      .paginate_button { background: #1a1d26 !important; border: 1px solid #2d3748 !important; border-radius: 6px !important; color: #718096 !important; font-size: 12px !important; }
      .paginate_button.current { background: #2b4c7e !important; color: #63b3ed !important; border-color: #3182ce !important; }

      .dataTables_length select { background: #1a1d26 !important; border: 1px solid #2d3748 !important; color: #e2e8f0 !important; border-radius: 6px !important; padding: 2px 6px !important; }

      .btn-download {
        background: linear-gradient(135deg, #276749, #2f855a) !important;
        border: none !important; border-radius: 10px !important; color: white !important;
        font-family: 'Space Grotesk', sans-serif !important; font-size: 13px !important;
        font-weight: 600 !important; padding: 11px 22px !important; cursor: pointer !important;
        transition: all 0.2s !important; box-shadow: 0 4px 15px rgba(39,103,73,0.3) !important;
        letter-spacing: 0.3px !important; margin-top: 16px !important;
      }

      .btn-download:hover { transform: translateY(-1px) !important; box-shadow: 0 6px 20px rgba(39,103,73,0.4) !important; }

      .stage-item { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }
      .stage-num { font-family: 'JetBrains Mono', monospace; font-size: 10px; color: #2d3748; background: #1a1d26; padding: 2px 6px; border-radius: 4px; min-width: 28px; text-align: center; }
      .stage-name { font-size: 12px; color: #4a5568; }

      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: #0d0f14; }
      ::-webkit-scrollbar-thumb { background: #2d3748; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: #4a5568; }

      .progress { background: #1e2330 !important; border-radius: 10px !important; }
      .progress-bar { background: linear-gradient(90deg, #3182ce, #63b3ed) !important; }

      .shiny-notification {
        background: #1e2330 !important; border: 1px solid #2d3748 !important;
        border-left: 3px solid #fc8181 !important; color: #e2e8f0 !important;
        font-family: 'Space Grotesk', sans-serif !important; border-radius: 10px !important;
      }

      .shiny-plot-output { border-radius: 10px; overflow: hidden; }

    "))
  ),
  
  div(class = "app-wrapper",
      
      # Sidebar
      div(class = "sidebar",
          
          div(class = "sidebar-logo",
              div(class = "logo-icon", "🧹"),
              div(class = "logo-title", "Data Cleaning Engine"),
              div(class = "logo-sub", "v2.0 · automated EDA")
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Dataset"),
              fileInput("file", NULL, accept = ".csv",
                        buttonLabel = "Choose CSV file",
                        placeholder = "No file selected")
          ),
          
          div(class = "sidebar-section",
              actionButton("run", "▶  Audit Data", class = "btn-run"),
              uiOutput("status_msg"),
              uiOutput("fix_decision_ui")
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Pipeline Stages"),
              lapply(list(
                list("01", "Load & Normalize"),
                list("02", "Schema Validation"),
                list("03", "Format Checks"),
                list("04", "Entropy Scoring"),
                list("05", "Imputation"),
                list("06", "Outlier Handling"),
                list("07", "Deduplication"),
                list("08", "Standardization"),
                list("09", "Final Scoring")
              ), function(s) {
                div(class = "stage-item",
                    span(class = "stage-num", s[[1]]),
                    span(class = "stage-name", s[[2]])
                )
              })
          )
      ),
      
      # Main Content
      div(class = "main-content",
          
          div(class = "page-header",
              div(class = "page-title", "Cleaning Report"),
              div(class = "page-sub", "Upload CSV  →  Audit  →  Decide  →  Clean")
          ),
          
          div(class = "metrics-row",
              div(class = "metric-card blue",
                  span(class = "metric-icon", "🎓"),
                  uiOutput("grade_val"),
                  div(class = "metric-label", "Readiness Grade")
              ),
              div(class = "metric-card green",
                  span(class = "metric-icon", "📊"),
                  uiOutput("score_val"),
                  div(class = "metric-label", "Quality Score")
              ),
              div(class = "metric-card amber",
                  span(class = "metric-icon", "🏷"),
                  uiOutput("status_val"),
                  div(class = "metric-label", "Dataset Status")
              ),
              div(class = "metric-card purple",
                  span(class = "metric-icon", "🗂"),
                  uiOutput("rows_val"),
                  div(class = "metric-label", "Rows")
              )
          ),
          
          uiOutput("score_breakdown_ui"),
          
          div(class = "tab-container",
              tabsetPanel(id = "main_tabs",
                          tabPanel("📊  Data Preview",    br(), uiOutput("cleaned_ui")),
                          tabPanel("📋  Audit Trail",     br(), uiOutput("audit_ui")),
                          tabPanel("📈  Column Scores",   br(), uiOutput("scores_ui")),
                          tabPanel("⚡  Entropy Scores",  br(), uiOutput("entropy_ui")),
                          tabPanel("🔍  Before vs After", br(), uiOutput("compare_ui"))
              ),
              div(style = "padding: 0 24px 24px;", uiOutput("download_ui"))
          )
      )
  )
)


server <- function(input, output, session) {
  
  rv <- reactiveValues(
    result       = NULL,
    ran          = FALSE,
    audited      = FALSE,
    audit_result = NULL
  )
  
  # Stage 1: Audit
  observeEvent(input$run, {
    req(input$file)
    rv$ran <- FALSE; rv$audited <- FALSE
    rv$result <- NULL; rv$audit_result <- NULL
    
    withProgress(message = "Auditing data...", value = 0, {
      tryCatch({
        
        incProgress(0.15, detail = "Loading & normalizing...")
        loaded    <- load_and_normalize(input$file$datapath)
        audit_log <- list()
        
        incProgress(0.15, detail = "Schema validation...")
        schema <- validate_schema(loaded$cleaned, audit_log)
        
        incProgress(0.15, detail = "Regex format checks...")
        fmt <- validate_formats(schema$df, schema$audit)
        
        incProgress(0.2, detail = "Computing entropy scores...")
        entropy_scores <- compute_entropy_scores(fmt$df)
        
        incProgress(0.2, detail = "Scoring dataset...")
        scores <- validate_and_score(loaded$original, fmt$df, entropy_scores)
        
        rv$audit_result <- list(
          loaded         = loaded,
          pre_clean_df   = fmt$df,
          audit_log      = fmt$audit,
          scores         = scores,
          entropy_scores = entropy_scores
        )
        rv$audited <- TRUE
        
      }, error = function(e) {
        showNotification(paste("Audit error:", e$message), type="error", duration=10)
      })
    })
  })
  
  # Stage 2: Clean
  observeEvent(input$fix_yes, {
    req(rv$audited)
    
    withProgress(message = "Cleaning data...", value = 0, {
      tryCatch({
        
        ar  <- rv$audit_result
        df  <- ar$pre_clean_df
        log <- ar$audit_log
        
        incProgress(0.2, detail = "Imputing missing values...")
        imp <- smart_impute(df, log)
        
        incProgress(0.2, detail = "Handling outliers...")
        out <- handle_outliers(imp$df, imp$audit)
        
        incProgress(0.2, detail = "Removing duplicates...")
        dedup <- remove_duplicates(out$df, out$audit)
        
        incProgress(0.2, detail = "Standardizing categories...")
        std <- standardize_categoricals(dedup$df, dedup$audit)
        
        incProgress(0.1, detail = "Final scoring...")
        final_scores <- validate_and_score(ar$loaded$original, std$df, ar$entropy_scores)
        report       <- build_audit_trail(std$audit, final_scores, final_scores$readiness_grade)
        
        rv$result  <- list(
          original = ar$loaded$original,
          cleaned  = std$df,
          report   = report,
          entropy  = ar$entropy_scores
        )
        rv$ran     <- TRUE
        rv$audited <- FALSE
        
      }, error = function(e) {
        showNotification(paste("Cleaning error:", e$message), type="error", duration=10)
      })
    })
  })
  
  # Skip cleaning
  observeEvent(input$fix_no, {
    showNotification("Skipped cleaning. You can re-audit or upload a new file.",
                     type="warning", duration=5)
    rv$audited <- FALSE
  })
  
  # Status pill
  output$status_msg <- renderUI({
    if (rv$ran)
      div(class="status-pill status-ready",   div(class="dot dot-green"), "Pipeline complete")
    else if (rv$audited)
      div(class="status-pill status-audited", div(class="dot dot-amber"), "Audit done — decide below")
    else
      div(class="status-pill status-waiting", div(class="dot dot-gray"),  "Awaiting input")
  })
  
  # Fix decision panel
  output$fix_decision_ui <- renderUI({
    if (!rv$audited) return(NULL)
    score  <- rv$audit_result$scores$composite_score
    status <- rv$audit_result$scores$status
    color  <- rv$audit_result$scores$status_color
    div(class="fix-panel",
        div(class="fix-status",  style=paste0("color:",color,";"), paste0("Status: ", status)),
        div(class="fix-score",   paste0("Quality Score: ", score, " / 100")),
        div(class="fix-question","Fix this data automatically?"),
        div(class="fix-buttons",
            actionButton("fix_yes", "Yes, Fix It", class="btn-yes"),
            actionButton("fix_no",  "No, Skip",    class="btn-no")
        )
    )
  })
  
  # Shared scores helper
  get_scores <- reactive({
    if (rv$ran)     return(rv$result$report)
    if (rv$audited) return(rv$audit_result$scores)
    NULL
  })
  
  # Metric boxes
  output$grade_val <- renderUI({
    s <- get_scores(); if (is.null(s)) return(div(class="metric-value muted","—"))
    grade <- s$readiness_grade
    cls   <- switch(grade,"A"="green","B"="blue","C"="amber","D"="red","blue")
    div(class=paste("metric-value",cls), grade)
  })
  
  output$score_val <- renderUI({
    s <- get_scores(); if (is.null(s)) return(div(class="metric-value muted","—"))
    div(class="metric-value green", paste0(s$composite_score,"/100"))
  })
  
  output$status_val <- renderUI({
    s <- get_scores(); if (is.null(s)) return(div(class="metric-value muted","—"))
    div(style=paste0("font-size:18px; font-weight:700; color:",s$status_color,
                     "; line-height:1.3; padding-top:4px;"), s$status)
  })
  
  output$rows_val <- renderUI({
    if (!rv$ran && !rv$audited) return(div(class="metric-value muted","—"))
    df <- if (rv$ran) rv$result$cleaned else rv$audit_result$pre_clean_df
    div(class="metric-value purple", format(nrow(df), big.mark=","))
  })
  
  # Score breakdown bars
  output$score_breakdown_ui <- renderUI({
    s <- get_scores(); if (is.null(s)) return(NULL)
    comp <- s$component_scores; if (is.null(comp)) return(NULL)
    
    make_bar <- function(label, value, color) {
      div(class="score-row",
          div(class="score-row-label", label),
          div(class="score-bar-bg",
              div(class="score-bar-fill",
                  style=paste0("width:",min(value,100),"%; background:",color,";"))),
          div(class="score-row-val", paste0(value,"%"))
      )
    }
    
    div(class="score-breakdown",
        div(class="score-breakdown-title","Score Breakdown"),
        make_bar("Completeness",  comp$completeness,  "#63b3ed"),
        make_bar("Deduplication", comp$deduplication, "#68d391"),
        make_bar("Confidence",    comp$confidence,    "#b794f4"),
        make_bar("Entropy Bonus", min(comp$entropy_bonus*10,100), "#f6ad55")
    )
  })
  
  # Waiting helper
  waiting <- function(msg) div(class="waiting-state",
                               div(class="waiting-icon","◌"),
                               div(class="waiting-text", msg)
  )
  
  # Data Preview
  output$cleaned_ui <- renderUI({
    if (!rv$ran && !rv$audited) return(waiting("audit or clean data to preview"))
    DTOutput("cleaned_table")
  })
  output$cleaned_table <- renderDT({
    req(rv$ran || rv$audited)
    df <- if (rv$ran) rv$result$cleaned else rv$audit_result$pre_clean_df
    datatable(df, options=list(scrollX=TRUE, pageLength=10, dom='frtip'),
              rownames=FALSE, class="display")
  })
  
  # Audit Trail
  output$audit_ui <- renderUI({
    if (!rv$ran) return(waiting("complete the pipeline to view audit trail"))
    DTOutput("audit_table")
  })
  output$audit_table <- renderDT({
    req(rv$ran)
    datatable(rv$result$report$audit_trail,
              options=list(scrollX=TRUE, dom='ft', pageLength=20),
              rownames=FALSE, class="display")
  })
  
  # Column Scores
  output$scores_ui <- renderUI({
    if (!rv$ran && !rv$audited) return(waiting("run pipeline to view column scores"))
    plotOutput("score_plot", height="420px")
  })
  output$score_plot <- renderPlot({
    req(rv$ran || rv$audited)
    sc <- if (rv$ran) rv$result$report$column_scores
    else {
      s <- rv$audit_result$scores$column_scores
      data.frame(Column=names(s),
                 Confidence=sapply(s, function(x) x$confidence),
                 stringsAsFactors=FALSE)
    }
    ggplot(sc, aes(x=reorder(Column,Confidence), y=Confidence, fill=Confidence)) +
      geom_bar(stat="identity", width=0.65) +
      geom_text(aes(label=paste0(Confidence,"%")), hjust=-0.15, color="#a0aec0", size=3.5) +
      coord_flip() +
      scale_fill_gradient(low="#e53e3e", high="#38a169") +
      scale_y_continuous(limits=c(0,120), expand=c(0,0)) +
      labs(title="Column Confidence Scores", x=NULL, y="Score (%)") +
      theme_minimal(base_size=13) +
      theme(plot.background=element_rect(fill="#111318",color=NA),
            panel.background=element_rect(fill="#111318",color=NA),
            panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(color="#1e2330",linewidth=0.5),
            panel.grid.minor=element_blank(),
            plot.title=element_text(color="#f7fafc",face="bold",size=14,margin=margin(b=14)),
            axis.text=element_text(color="#718096",size=11),
            axis.title.x=element_text(color="#4a5568",size=11),
            legend.position="none")
  }, bg="#111318")
  
  # Entropy
  output$entropy_ui <- renderUI({
    if (!rv$ran && !rv$audited) return(waiting("run pipeline to view entropy scores"))
    tagList(plotOutput("entropy_plot", height="380px"), br(), DTOutput("entropy_table"))
  })
  
  output$entropy_plot <- renderPlot({
    req(rv$ran || rv$audited)
    ent <- if (rv$ran) rv$result$entropy else rv$audit_result$entropy_scores
    df  <- data.frame(Column=names(ent),
                      Entropy=sapply(ent, function(x) x$norm_entropy),
                      stringsAsFactors=FALSE)
    ggplot(df, aes(x=reorder(Column,Entropy), y=Entropy, fill=Entropy)) +
      geom_bar(stat="identity", width=0.65) +
      geom_text(aes(label=paste0(Entropy,"%")), hjust=-0.15, color="#a0aec0", size=3.5) +
      coord_flip() +
      scale_fill_gradient(low="#805ad5", high="#63b3ed") +
      scale_y_continuous(limits=c(0,120), expand=c(0,0)) +
      labs(title="Entropy Stability Score per Column",
           subtitle="Higher = evenly distributed  |  Lower = concentrated / skewed",
           x=NULL, y="Normalized Entropy (%)") +
      theme_minimal(base_size=13) +
      theme(plot.background=element_rect(fill="#111318",color=NA),
            panel.background=element_rect(fill="#111318",color=NA),
            panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(color="#1e2330",linewidth=0.5),
            panel.grid.minor=element_blank(),
            plot.title=element_text(color="#f7fafc",face="bold",size=14,margin=margin(b=4)),
            plot.subtitle=element_text(color="#4a5568",size=11,margin=margin(b=14)),
            axis.text=element_text(color="#718096",size=11),
            legend.position="none")
  }, bg="#111318")
  
  output$entropy_table <- renderDT({
    req(rv$ran || rv$audited)
    ent <- if (rv$ran) rv$result$entropy else rv$audit_result$entropy_scores
    df  <- data.frame(
      Column             = names(ent),
      `Norm Entropy (%)`= sapply(ent, function(x) x$norm_entropy),
      `Raw Entropy`     = sapply(ent, function(x) x$entropy),
      Stability         = sapply(ent, function(x) x$stability),
      stringsAsFactors=FALSE, check.names=FALSE
    )
    datatable(df, options=list(dom='ft', pageLength=20), rownames=FALSE, class="display")
  })
  
  # Before vs After
  output$compare_ui <- renderUI({
    if (!rv$ran) return(waiting("complete cleaning to view before/after comparison"))
    plotOutput("missing_plot", height="420px")
  })
  
  output$missing_plot <- renderPlot({
    req(rv$ran)
    orig    <- rv$result$original
    cleaned <- rv$result$cleaned
    common_cols    <- intersect(names(orig), names(cleaned))
    before_missing <- sapply(common_cols, function(c) sum(is.na(orig[[c]])))
    after_missing  <- sapply(common_cols, function(c) sum(is.na(cleaned[[c]])))
    had_missing    <- common_cols[before_missing > 0]
    
    if (length(had_missing) == 0) {
      return(ggplot() +
               annotate("text", x=0.5, y=0.5,
                        label="No missing values found in original dataset",
                        color="#68d391", size=5) +
               theme_void() +
               theme(plot.background=element_rect(fill="#111318",color=NA)))
    }
    
    df <- data.frame(
      Column  = rep(had_missing, 2),
      Missing = c(before_missing[had_missing], after_missing[had_missing]),
      Stage   = rep(c("Before","After"), each=length(had_missing))
    )
    ggplot(df, aes(x=reorder(Column,Missing), y=Missing, fill=Stage)) +
      geom_bar(stat="identity", position="dodge", width=0.65) +
      geom_text(aes(label=Missing), position=position_dodge(width=0.65),
                hjust=-0.15, color="#a0aec0", size=3.5) +
      coord_flip() +
      scale_fill_manual(values=c("Before"="#e53e3e","After"="#38a169")) +
      scale_y_continuous(expand=expansion(mult=c(0,0.18))) +
      labs(title="Missing Values: Before vs After Cleaning",
           subtitle=paste(length(had_missing),"columns had missing values"),
           x=NULL, y="Missing Count") +
      theme_minimal(base_size=13) +
      theme(plot.background=element_rect(fill="#111318",color=NA),
            panel.background=element_rect(fill="#111318",color=NA),
            panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(color="#1e2330",linewidth=0.5),
            panel.grid.minor=element_blank(),
            plot.title=element_text(color="#f7fafc",face="bold",size=14,margin=margin(b=4)),
            plot.subtitle=element_text(color="#4a5568",size=11,margin=margin(b=14)),
            axis.text=element_text(color="#718096",size=11),
            legend.title=element_blank(),
            legend.text=element_text(color="#718096",size=11),
            legend.position="top")
  }, bg="#111318")
  
  # Download
  output$download_ui <- renderUI({
    if (!rv$ran) return(NULL)
    downloadButton("download_csv","⬇  Download Cleaned CSV", class="btn-download")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("cleaned_", Sys.Date(), ".csv"),
    content  = function(file) { req(rv$ran); write.csv(rv$result$cleaned, file, row.names=FALSE) }
  )
}

shinyApp(ui, server)
