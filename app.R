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

      /* ── Animated background grid ── */
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

      /* ── Sidebar ── */
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
        font-size: 16px;
        font-weight: 700;
        color: #f7fafc;
        letter-spacing: -0.3px;
      }

      .logo-sub {
        font-size: 11px;
        color: #4a5568;
        font-weight: 400;
        margin-top: 2px;
        font-family: 'JetBrains Mono', monospace;
        text-transform: uppercase;
        letter-spacing: 1px;
      }

      .sidebar-section {
        padding: 20px 24px;
        border-bottom: 1px solid #1e2330;
      }

      .sidebar-label {
        font-size: 10px;
        font-weight: 600;
        color: #4a5568;
        text-transform: uppercase;
        letter-spacing: 1.5px;
        margin-bottom: 10px;
      }

      /* File input styling */
      .form-group { margin: 0 !important; }

      .shiny-input-container { width: 100% !important; }

      input[type='text'] {
        background: #1a1d26 !important;
        border: 1px solid #2d3748 !important;
        border-radius: 8px !important;
        color: #e2e8f0 !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 12px !important;
        padding: 10px 12px !important;
        width: 100% !important;
        outline: none !important;
        transition: border-color 0.2s !important;
      }

      input[type='text']:focus {
        border-color: #63b3ed !important;
        box-shadow: 0 0 0 3px rgba(99,179,237,0.1) !important;
      }

      input[type='text']::placeholder { color: #4a5568 !important; }

      .btn-run {
        width: 100%;
        background: linear-gradient(135deg, #3182ce, #2b6cb0);
        border: none;
        border-radius: 10px;
        color: white;
        font-family: 'Space Grotesk', sans-serif;
        font-size: 14px;
        font-weight: 600;
        padding: 13px 20px;
        cursor: pointer;
        transition: all 0.2s;
        letter-spacing: 0.3px;
        box-shadow: 0 4px 15px rgba(49,130,206,0.3);
      }

      .btn-run:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(49,130,206,0.4);
      }

      .btn-run:active { transform: translateY(0); }

      .status-pill {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 12px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 500;
        margin-top: 12px;
        width: 100%;
        justify-content: center;
      }

      .status-ready {
        background: rgba(72,187,120,0.1);
        color: #68d391;
        border: 1px solid rgba(72,187,120,0.2);
      }

      .status-waiting {
        background: rgba(74,85,104,0.3);
        color: #718096;
        border: 1px solid #2d3748;
      }

      .dot { width: 6px; height: 6px; border-radius: 50%; }
      .dot-green { background: #68d391; box-shadow: 0 0 6px #68d391; }
      .dot-gray { background: #4a5568; }

      /* File input custom */
      /* File input custom */
.input-group-btn .btn { display: none; }
.btn-file {
  background: #1e2330 !important;
  border: 1px solid #2d3748 !important;
  border-radius: 8px !important;
  color: #a0aec0 !important;
  font-size: 12px !important;
  font-family: 'Space Grotesk', sans-serif !important;
  padding: 10px 14px !important;
  margin-bottom: 8px !important;
  width: 100% !important;
  transition: all 0.2s !important;
  display: block !important;
}

.btn-file:hover {
  background: #252b3b !important;
  border-color: #4a5568 !important;
  color: #e2e8f0 !important;
}

.btn-file span { pointer-events: none; }

.form-control[readonly] {
  background: #1a1d26 !important;
  border: 1px solid #1e2330 !important;
  border-radius: 8px !important;
  color: #4a5568 !important;
  font-size: 11px !important;
  font-family: 'JetBrains Mono', monospace !important;
  padding: 8px 12px !important;
  margin-top: 6px !important;
}

      .btn-file {
        background: #1e2330 !important;
        border: 1px solid #2d3748 !important;
        border-radius: 8px !important;
        color: #a0aec0 !important;
        font-size: 12px !important;
        font-family: 'Space Grotesk', sans-serif !important;
        padding: 10px 14px !important;
        margin-bottom: 8px !important;
        width: 100% !important;
        transition: all 0.2s !important;
      }

      .btn-file:hover {
        background: #252b3b !important;
        border-color: #4a5568 !important;
        color: #e2e8f0 !important;
      }

      /* ── Main content ── */
      .main-content {
        margin-left: 280px;
        flex: 1;
        padding: 32px;
        min-height: 100vh;
      }

      /* ── Top header ── */
      .page-header {
        margin-bottom: 28px;
      }

      .page-title {
        font-size: 26px;
        font-weight: 700;
        color: #f7fafc;
        letter-spacing: -0.5px;
      }

      .page-sub {
        font-size: 13px;
        color: #4a5568;
        margin-top: 4px;
        font-family: 'JetBrains Mono', monospace;
      }

      /* ── Metric cards ── */
      .metrics-row {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 16px;
        margin-bottom: 24px;
      }

      .metric-card {
        background: #111318;
        border: 1px solid #1e2330;
        border-radius: 14px;
        padding: 20px 22px;
        position: relative;
        overflow: hidden;
        transition: border-color 0.2s;
      }

      .metric-card:hover { border-color: #2d3748; }

      .metric-card::before {
        content: '';
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 2px;
      }

      .metric-card.blue::before  { background: linear-gradient(90deg, #3182ce, #63b3ed); }
      .metric-card.green::before { background: linear-gradient(90deg, #38a169, #68d391); }
      .metric-card.purple::before{ background: linear-gradient(90deg, #805ad5, #b794f4); }

      .metric-icon {
        font-size: 22px;
        margin-bottom: 12px;
        display: block;
      }

      .metric-value {
        font-size: 36px;
        font-weight: 700;
        letter-spacing: -1px;
        line-height: 1;
        margin-bottom: 6px;
      }

      .metric-value.blue   { color: #63b3ed; }
      .metric-value.green  { color: #68d391; }
      .metric-value.purple { color: #b794f4; }
      .metric-value.amber  { color: #f6ad55; }
      .metric-value.red    { color: #fc8181; }
      .metric-value.muted  { color: #2d3748; }

      .metric-label {
        font-size: 12px;
        color: #4a5568;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 1px;
      }

      /* ── Tab panel ── */
      .tab-container {
        background: #111318;
        border: 1px solid #1e2330;
        border-radius: 16px;
        overflow: hidden;
      }

      .nav-tabs {
        background: #0d0f14;
        border-bottom: 1px solid #1e2330 !important;
        padding: 0 8px;
        display: flex;
        gap: 4px;
      }

      .nav-tabs > li > a {
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        border-radius: 0 !important;
        color: #4a5568 !important;
        font-family: 'Space Grotesk', sans-serif !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        padding: 14px 18px !important;
        transition: all 0.2s !important;
        margin: 0 !important;
      }

      .nav-tabs > li > a:hover {
        color: #a0aec0 !important;
        background: transparent !important;
      }

      .nav-tabs > li.active > a {
        color: #63b3ed !important;
        border-bottom-color: #63b3ed !important;
        background: transparent !important;
      }

      .tab-content { padding: 24px; }

      /* ── Waiting state ── */
      .waiting-state {
        text-align: center;
        padding: 60px 20px;
        color: #2d3748;
      }

      .waiting-icon {
        font-size: 48px;
        margin-bottom: 16px;
        opacity: 0.3;
      }

      .waiting-text {
        font-size: 14px;
        font-family: 'JetBrains Mono', monospace;
      }

      /* ── DataTable dark theme ── */
      .dataTables_wrapper {
        color: #e2e8f0 !important;
        font-size: 13px !important;
      }

      table.dataTable {
        background: transparent !important;
        border-collapse: collapse !important;
        width: 100% !important;
      }

      table.dataTable thead th {
        background: #0d0f14 !important;
        color: #718096 !important;
        border-bottom: 1px solid #1e2330 !important;
        font-size: 11px !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
        padding: 10px 14px !important;
      }

      table.dataTable tbody tr {
        background: transparent !important;
        border-bottom: 1px solid #1a1d26 !important;
        transition: background 0.1s !important;
      }

      table.dataTable tbody tr:hover td {
        background: #1a1d26 !important;
      }

      table.dataTable tbody td {
        color: #cbd5e0 !important;
        padding: 10px 14px !important;
        border: none !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 12px !important;
      }

      .dataTables_info, .dataTables_length label,
      .dataTables_filter label, .dataTables_paginate {
        color: #4a5568 !important;
        font-size: 12px !important;
      }

      .dataTables_filter input {
        background: #1a1d26 !important;
        border: 1px solid #2d3748 !important;
        border-radius: 6px !important;
        color: #e2e8f0 !important;
        padding: 4px 10px !important;
        font-size: 12px !important;
      }

      .paginate_button {
        background: #1a1d26 !important;
        border: 1px solid #2d3748 !important;
        border-radius: 6px !important;
        color: #718096 !important;
        font-size: 12px !important;
      }

      .paginate_button.current {
        background: #2b4c7e !important;
        color: #63b3ed !important;
        border-color: #3182ce !important;
      }

      .dataTables_length select {
        background: #1a1d26 !important;
        border: 1px solid #2d3748 !important;
        color: #e2e8f0 !important;
        border-radius: 6px !important;
        padding: 2px 6px !important;
      }

      /* ── LLM box ── */
      .llm-response {
        background: #0d0f14;
        border: 1px solid #1e2330;
        border-left: 3px solid #63b3ed;
        border-radius: 10px;
        padding: 20px 24px;
        font-family: 'JetBrains Mono', monospace;
        font-size: 13px;
        line-height: 1.8;
        color: #a0aec0;
        white-space: pre-wrap;
        max-height: 500px;
        overflow-y: auto;
      }

      .llm-header {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 16px;
        padding-bottom: 12px;
        border-bottom: 1px solid #1e2330;
      }

      .llm-badge {
        background: rgba(99,179,237,0.1);
        border: 1px solid rgba(99,179,237,0.2);
        border-radius: 6px;
        padding: 3px 10px;
        font-size: 11px;
        color: #63b3ed;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
      }

      /* ── Download button ── */
      .btn-download {
        background: linear-gradient(135deg, #276749, #2f855a) !important;
        border: none !important;
        border-radius: 10px !important;
        color: white !important;
        font-family: 'Space Grotesk', sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        padding: 11px 22px !important;
        cursor: pointer !important;
        transition: all 0.2s !important;
        box-shadow: 0 4px 15px rgba(39,103,73,0.3) !important;
        letter-spacing: 0.3px !important;
        margin-top: 16px !important;
      }

      .btn-download:hover {
        transform: translateY(-1px) !important;
        box-shadow: 0 6px 20px rgba(39,103,73,0.4) !important;
      }

      /* ── Plot backgrounds ── */
      .shiny-plot-output { border-radius: 10px; overflow: hidden; }

      /* ── Progress bar ── */
      .progress { background: #1e2330 !important; border-radius: 10px !important; }
      .progress-bar { background: linear-gradient(90deg, #3182ce, #63b3ed) !important; }

      /* Notification */
      .shiny-notification {
        background: #1e2330 !important;
        border: 1px solid #2d3748 !important;
        border-left: 3px solid #fc8181 !important;
        color: #e2e8f0 !important;
        font-family: 'Space Grotesk', sans-serif !important;
        border-radius: 10px !important;
      }

      /* Scrollbar */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: #0d0f14; }
      ::-webkit-scrollbar-thumb { background: #2d3748; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: #4a5568; }

    "))
  ),
  
  div(class = "app-wrapper",
      
      # ── Sidebar ──
      div(class = "sidebar",
          
          div(class = "sidebar-logo",
              div(class = "logo-icon", "🧹"),
              div(class = "logo-title", "Data Cleaning Engine"),
              div(class = "logo-sub", "v1.0 · automated EDA")
          ),
          
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Dataset"),
              fileInput("file", NULL, accept = ".csv",
                        buttonLabel = "Choose CSV file",
                        placeholder = "No file selected")
          ),
          
          div(class = "sidebar-section",
              actionButton("run", "▶  Run Pipeline", class = "btn-run"),
              uiOutput("status_msg")
          ),
          
          # Pipeline stages indicator
          div(class = "sidebar-section",
              div(class = "sidebar-label", "Pipeline Stages"),
              tags$div(style = "display:flex; flex-direction:column; gap:8px;",
                       lapply(list(
                         list("01", "Load & Normalize"),
                         list("02", "Smart Imputation"),
                         list("03", "Outlier Handling"),
                         list("04", "Deduplication"),
                         list("05", "Standardization"),
                         list("06", "Validation & Score"),
                         list("07", "LLM Diagnosis")
                       ), function(s) {
                         tags$div(style = "display:flex; align-items:center; gap:10px;",
                                  tags$span(style = "font-family:'JetBrains Mono',monospace; font-size:10px;
                         color:#2d3748; background:#1a1d26; padding:2px 6px;
                         border-radius:4px; min-width:28px; text-align:center;", s[[1]]),
                                  tags$span(style = "font-size:12px; color:#4a5568;", s[[2]])
                         )
                       })
              )
          )
      ),
      
      # ── Main Content ──
      div(class = "main-content",
          
          div(class = "page-header",
              div(class = "page-title", "Cleaning Report"),
              div(class = "page-sub", "Upload a CSV and run the pipeline to begin analysis")
          ),
          
          # Metric cards
          div(class = "metrics-row",
              div(class = "metric-card blue",
                  span(class = "metric-icon", "🎓"),
                  uiOutput("grade_val"),
                  div(class = "metric-label", "Readiness Grade")
              ),
              div(class = "metric-card green",
                  span(class = "metric-icon", "📊"),
                  uiOutput("score_val"),
                  div(class = "metric-label", "Composite Score")
              ),
              div(class = "metric-card purple",
                  span(class = "metric-icon", "🗂"),
                  uiOutput("rows_val"),
                  div(class = "metric-label", "Clean Rows")
              )
          ),
          
          # Tab panel
          div(class = "tab-container",
              tabsetPanel(id = "main_tabs",
                          tabPanel("📊  Cleaned Data",    br(), uiOutput("cleaned_ui")),
                          tabPanel("📋  Audit Trail",     br(), uiOutput("audit_ui")),
                          tabPanel("📈  Column Scores",   br(), uiOutput("scores_ui")),
                          tabPanel("🔍  Before vs After", br(), uiOutput("compare_ui"))
              ),
              div(style = "padding: 0 24px 24px;", uiOutput("download_ui"))
          )
      )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(result = NULL, ran = FALSE)
  
  observeEvent(input$run, {
    req(input$file)
    rv$ran    <- FALSE
    rv$result <- NULL
    
    withProgress(message = "Running pipeline...", value = 0, {
      tryCatch({
        
        incProgress(0.1, detail = "Loading & normalizing...")
        loaded    <- load_and_normalize(input$file$datapath)
        audit_log <- list()
        
        incProgress(0.1, detail = "Imputing missing values...")
        imp <- smart_impute(loaded$cleaned, audit_log)
        
        incProgress(0.1, detail = "Handling outliers...")
        out <- handle_outliers(imp$df, imp$audit)
        
        incProgress(0.1, detail = "Removing duplicates...")
        dedup <- remove_duplicates(out$df, out$audit)
        
        incProgress(0.1, detail = "Standardizing categories...")
        std <- standardize_categoricals(dedup$df, dedup$audit)
        
        incProgress(0.15, detail = "Validating & scoring...")
        scores <- validate_and_score(loaded$original, std$df)
        
        incProgress(0.1, detail = "Building report...")
        report <- build_audit_trail(std$audit, scores, scores$readiness_grade)        
        rv$result <- list(original = loaded$original, cleaned = std$df, report = report)
        rv$ran    <- TRUE
        
      }, error = function(e) {
        showNotification(paste("Pipeline error:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  # Status
  output$status_msg <- renderUI({
    if (rv$ran)
      div(class = "status-pill status-ready",
          div(class = "dot dot-green"), "Pipeline complete")
    else
      div(class = "status-pill status-waiting",
          div(class = "dot dot-gray"), "Awaiting input")
  })
  
  # Metric values
  output$grade_val <- renderUI({
    if (!rv$ran) return(div(class = "metric-value muted", "—"))
    grade <- rv$result$report$readiness_grade
    cls   <- switch(grade, "A"="green","B"="blue","C"="amber","D"="red","blue")
    div(class = paste("metric-value", cls), grade)
  })
  
  output$score_val <- renderUI({
    if (!rv$ran) return(div(class = "metric-value muted", "—"))
    div(class = "metric-value green",
        paste0(rv$result$report$composite_score, "%"))
  })
  
  output$rows_val <- renderUI({
    if (!rv$ran) return(div(class = "metric-value muted", "—"))
    div(class = "metric-value purple",
        format(nrow(rv$result$cleaned), big.mark = ","))
  })
  
  # ── Tab UIs ──
  waiting <- function(msg) div(class = "waiting-state",
                               div(class = "waiting-icon", "◌"),
                               div(class = "waiting-text", msg)
  )
  
  output$cleaned_ui <- renderUI({
    if (!rv$ran) return(waiting("run pipeline to view cleaned data"))
    DTOutput("cleaned_table")
  })
  output$cleaned_table <- renderDT({
    req(rv$ran)
    datatable(rv$result$cleaned,
              options = list(scrollX = TRUE, pageLength = 10,
                             dom = 'frtip',
                             initComplete = JS("function(s,d,n){$(d.nTable()).css('color','#cbd5e0');}")),
              rownames = FALSE, class = "display")
  })
  
  output$audit_ui <- renderUI({
    if (!rv$ran) return(waiting("run pipeline to view audit trail"))
    DTOutput("audit_table")
  })
  output$audit_table <- renderDT({
    req(rv$ran)
    datatable(rv$result$report$audit_trail,
              options = list(scrollX = TRUE, dom = 'ft', pageLength = 20),
              rownames = FALSE, class = "display")
  })
  
  output$scores_ui <- renderUI({
    if (!rv$ran) return(waiting("run pipeline to view column scores"))
    plotOutput("score_plot", height = "420px")
  })
  output$score_plot <- renderPlot({
    req(rv$ran)
    df <- rv$result$report$column_scores
    ggplot(df, aes(x = reorder(Column, Confidence), y = Confidence, fill = Confidence)) +
      geom_bar(stat = "identity", width = 0.65) +
      geom_text(aes(label = paste0(Confidence, "%")), hjust = -0.15,
                color = "#a0aec0", size = 3.5,
                family = "mono") +
      coord_flip() +
      scale_fill_gradient(low = "#e53e3e", high = "#38a169") +
      scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
      labs(title = "Column Confidence Scores", x = NULL, y = "Score (%)") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#111318", color = NA),
        panel.background = element_rect(fill = "#111318", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#1e2330", linewidth = 0.5),
        panel.grid.minor  = element_blank(),
        plot.title    = element_text(color = "#f7fafc", face = "bold",
                                     size = 14, margin = margin(b = 14)),
        axis.text     = element_text(color = "#718096", size = 11),
        axis.title.x  = element_text(color = "#4a5568", size = 11),
        legend.position = "none"
      )
  }, bg = "#111318")
  
  output$compare_ui <- renderUI({
    if (!rv$ran) return(waiting("run pipeline to view before/after comparison"))
    plotOutput("missing_plot", height = "420px")
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
                        color="#68d391", size=5, family="mono") +
               theme_void() +
               theme(plot.background = element_rect(fill="#111318", color=NA)))
    }
    
    df <- data.frame(
      Column  = rep(had_missing, 2),
      Missing = c(before_missing[had_missing], after_missing[had_missing]),
      Stage   = rep(c("Before", "After"), each = length(had_missing))
    )
    
    ggplot(df, aes(x = reorder(Column, Missing), y = Missing, fill = Stage)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.65) +
      geom_text(aes(label = Missing),
                position = position_dodge(width = 0.65),
                hjust = -0.15, color = "#a0aec0", size = 3.5) +
      coord_flip() +
      scale_fill_manual(values = c("Before" = "#e53e3e", "After" = "#38a169")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Missing Values: Before vs After Cleaning",
           subtitle = paste(length(had_missing), "columns had missing values"),
           x = NULL, y = "Missing Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#111318", color = NA),
        panel.background = element_rect(fill = "#111318", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#1e2330", linewidth = 0.5),
        panel.grid.minor  = element_blank(),
        plot.title    = element_text(color = "#f7fafc", face = "bold", size = 14,
                                     margin = margin(b = 4)),
        plot.subtitle = element_text(color = "#4a5568", size = 11,
                                     margin = margin(b = 14)),
        axis.text     = element_text(color = "#718096", size = 11),
        axis.title.x  = element_text(color = "#4a5568", size = 11),
        legend.title  = element_blank(),
        legend.text   = element_text(color = "#718096", size = 11),
        legend.position = "top"
      )
  }, bg = "#111318")
  
  # Download
  output$download_ui <- renderUI({
    if (!rv$ran) return(NULL)
    downloadButton("download_csv", "⬇  Download Cleaned CSV", class = "btn-download")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("cleaned_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$ran)
      write.csv(rv$result$cleaned, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)