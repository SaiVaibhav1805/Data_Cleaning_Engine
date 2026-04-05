build_audit_trail <- function(audit_log, scores, grade) {
  
  imp_summary <- if (length(audit_log$imputation) > 0)
    paste(unlist(audit_log$imputation), collapse = " | ")
  else "No missing values found"
  
  dedup_summary <- if (!is.null(audit_log$deduplication))
    audit_log$deduplication
  else "No duplicates found"
  
  std_summary <- if (!is.null(audit_log$standardization))
    audit_log$standardization
  else "No standardization needed"
  
  outlier_summary <- if (!is.null(audit_log$outliers))
    if (is.list(audit_log$outliers))
      paste(unlist(audit_log$outliers), collapse = " | ")
  else
    audit_log$outliers
  else "No outlier handling performed"
  
  trail <- data.frame(
    Stage = c("Structural Normalization", "Imputation",
              "Outlier Handling", "Deduplication", "Standardization"),
    Summary = c(
      "Column names cleaned, types auto-detected",
      imp_summary,
      outlier_summary,
      dedup_summary,
      std_summary
    ),
    stringsAsFactors = FALSE
  )
  
  col_score_df <- data.frame(
    Column     = names(scores$column_scores),
    Confidence = sapply(scores$column_scores, function(x) x$confidence),
    stringsAsFactors = FALSE
  )
  
  list(
    audit_trail     = trail,
    column_scores   = col_score_df,
    readiness_grade = scores$readiness_grade,
    composite_score = scores$composite_score
  )
}