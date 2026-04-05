🧹  Data Cleaning Engine
Automated EDA · R + Shiny · v2.0
An end-to-end data cleaning, validation, and intelligent reporting engine built entirely in R.

Overview
The Data Cleaning Engine accepts a raw CSV as input and returns a cleaned, validated, and fully explained dataset with no manual intervention required. It executes a multi-stage pipeline covering structural normalization, smart imputation, duplicate removal, outlier handling, and categorical standardization.

The engine introduces several novel capabilities absent from standard R packages:
•	Column Confidence Scoring — rates each column's post-cleaning reliability (0–100)
•	Entropy-based Stability Score — Shannon entropy per column, normalized and categorized
•	Dataset Readiness Grade — composite A–D grade from completeness, uniqueness, validity, and consistency
•	Approved / Needs Improvement / Rejected — automated status label on the dataset
•	Two-stage Audit → Clean flow — user sees the audit report before deciding to clean

Quick Start
1. Install R packages
install.packages(c(
  "tidyverse", "janitor", "VIM", "mice", "httr", "jsonlite",
  "shiny", "shinydashboard", "DT", "ggplot2", "naniar",
  "stringr", "lubridate"
))

2. Set up project structure
DataCleaningEngine/
├── app.R
└── pipeline/
    ├── 01_load.R
    ├── 02_impute.R
    ├── 02b_outliers.R
    ├── 03_deduplicate.R
    ├── 04_standardize.R
    ├── 05_validate.R
    ├── 05b_schema.R
    ├── 05c_regex.R
    ├── 05d_entropy.R
    └── 07_report.R

3. Launch the app
setwd("DataCleaningEngine/")
shiny::runApp("app.R")

Tip
No API key is required. All features work fully offline. The Anthropic API key field has been removed in v2.0.

How It Works
The engine runs in two distinct stages, giving the user full control before any data is modified.

Stage 1 — Audit
•	Upload your CSV and click Audit Data
•	The engine runs schema validation, regex checks, and entropy scoring
•	A Quality Score (0–100) and Status label (Approved / Needs Improvement / Rejected) are displayed
•	A 'Fix Data?' Yes/No decision panel appears in the sidebar

Stage 2 — Clean (optional)
•	Click Yes, Fix It to trigger the full cleaning pipeline
•	Missing values are imputed, outliers capped, duplicates removed, and labels standardized
•	A final report and download button appear on completion
•	Click No, Skip to abort without modifying the data

Pipeline Stages

Stage	Name	Description	Status
01	Load & Normalize	Reads CSV, cleans column names, infers data types	✅ Active
02	Schema Validation	Detects Inf values, zero-variance cols, near-empty columns	✅ Active
03	Format Checks	Regex validation for emails, phones, dates, gender fields	✅ Active
04	Entropy Scoring	Shannon entropy computed per column, normalized 0–100	✅ Active
05	Imputation	Grouped median (numeric) and mode (categorical) fill	✅ Active
06	Outlier Handling	IQR-based detection, Winsorization capping	✅ Active
07	Deduplication	Exact duplicate removal + fuzzy signature matching	✅ Active
08	Standardization	Normalizes yes/no, gender variants, boolean strings	✅ Active
09	Final Scoring	Composite 0–100 score, grade A–D, Approved/Rejected status	✅ Active

Scoring Engine
Quality Score (0–100)
The composite score is computed as a weighted sum of four components:

Feature	Details
Completeness (40%)	Average non-null rate across all columns
Deduplication (25%)	Proportion of rows that were unique in the original
Confidence (25%)	Mean column confidence score across all columns
Entropy Bonus (10%)	Average normalized Shannon entropy across numeric/categorical columns

Status Labels
Feature	Details
✅  Approved	Composite score ≥ 75
⚠️  Needs Improvement	Composite score 50–74
❌  Rejected	Composite score < 50

Readiness Grade
Feature	Details
A	Score ≥ 90
B	Score 75–89
C	Score 60–74
D	Score < 60

Entropy Stability Score
Shannon entropy is computed per column using the formula:

H = -Σ p(x) · log₂(p(x))

Numeric columns are first binned into 10 equal-width buckets. The raw entropy is then normalized against the maximum possible entropy for that column (log₂ of the number of unique values), producing a 0–100 score.

Feature	Details
Highly Distributed (80–100)	Values are evenly spread — low skew
Moderate (50–79)	Reasonable distribution with some concentration
Skewed (20–49)	Most values cluster around a small set
Highly Concentrated (0–19)	Near-constant column — low informational value

Dashboard Tabs
Feature	Details
📊  Data Preview	Interactive table of the dataset (pre- or post-clean)
📋  Audit Trail	Step-by-step log of every transformation performed
📈  Column Scores	Bar chart of confidence scores per column (red→green)
⚡  Entropy Scores	Entropy bar chart + stability table per column
🔍  Before vs After	Side-by-side missing value counts before and after cleaning

File Reference
Feature	Details
app.R	Main Shiny UI and server — entry point
01_load.R	CSV ingestion, column name normalization, type inference
02_impute.R	Smart grouped median / mode imputation
02b_outliers.R	IQR outlier detection and Winsorization
03_deduplicate.R	Exact + fuzzy duplicate removal
04_standardize.R	Categorical label normalization
05_validate.R	Column confidence scoring and composite grading
05b_schema.R	Schema validation (Inf values, zero-variance, near-empty)
05c_regex.R	Regex format checks for emails, phones, dates, etc.
05d_entropy.R	Shannon entropy computation per column
07_report.R	Final audit trail and report assembly

Notes
Fuzzy Deduplication
Fuzzy duplicates are detected by generating a normalized row signature: all character columns are lowercased, stripped of non-alphanumeric characters, sorted, and concatenated. Rows with identical signatures are treated as fuzzy duplicates.

Outlier Handling
Outliers are capped rather than deleted (Winsorization). Values below Q1 − 1.5×IQR are raised to the lower fence; values above Q3 + 1.5×IQR are lowered to the upper fence. This preserves all rows while neutralizing extreme values.

Imputation Strategy
Numeric columns are filled with the column median. Categorical columns are filled with the mode. Columns with more than 60% missing values are flagged in the audit trail and left unimputed.

Data Cleaning Engine  ·  v2.0  ·  Built with R + Shiny
