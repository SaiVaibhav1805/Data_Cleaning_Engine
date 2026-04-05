validate_schema <- function(df, audit_log) {
  
  schema_issues <- list()
  
  for (col in names(df)) {
    issues <- c()
    
    # Check for mixed types in numeric columns
    if (is.numeric(df[[col]])) {
      if (all(is.na(df[[col]]))) {
        issues <- c(issues, "all values are NA")
      }
      if (any(is.infinite(df[[col]]), na.rm = TRUE)) {
        issues <- c(issues, "contains Inf values")
        df[[col]][is.infinite(df[[col]])] <- NA
      }
    }
    
    # Check for constant columns (zero variance)
    if (length(unique(na.omit(df[[col]]))) == 1) {
      issues <- c(issues, "constant column (zero variance)")
    }
    
    # Check for near-empty columns (>80% NA)
    pct_na <- mean(is.na(df[[col]]))
    if (pct_na > 0.8) {
      issues <- c(issues, paste0(round(pct_na * 100), "% missing — consider dropping"))
    }
    
    # Check numeric columns for negative values where unexpected
    if (is.numeric(df[[col]])) {
      neg_count <- sum(df[[col]] < 0, na.rm = TRUE)
      if (neg_count > 0 && grepl("age|count|price|amount|qty|quantity|salary", 
                                 col, ignore.case = TRUE)) {
        issues <- c(issues, paste0(neg_count, " unexpected negative values"))
        df[[col]][df[[col]] < 0] <- NA
      }
    }
    
    if (length(issues) > 0) {
      schema_issues[[col]] <- paste0(col, ": ", paste(issues, collapse = "; "))
    }
  }
  
  if (length(schema_issues) == 0) {
    audit_log$schema <- "No schema violations detected"
  } else {
    audit_log$schema <- schema_issues
  }
  
  list(df = df, audit = audit_log)
}