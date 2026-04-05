smart_impute <- function(df, audit_log) {
  library(dplyr)
  library(mice)
  
  imputation_notes <- list()
  
  for (col in names(df)) {
    missing_count <- sum(is.na(df[[col]]))
    if (missing_count == 0) next
    
    pct_missing <- missing_count / nrow(df)
    
    if (pct_missing > 0.6) {
      # Too many missing — flag column, don't impute
      imputation_notes[[col]] <- paste0(col, ": >60% missing, flagged for removal")
      next
    }
    
    if (is.numeric(df[[col]])) {
      # Group-aware median imputation
      # Try to find a related categorical column to group by
      cat_cols <- names(df)[sapply(df, is.character)]
      if (length(cat_cols) > 0) {
        group_col <- cat_cols[1]
        df <- df %>%
          group_by(.data[[group_col]]) %>%
          mutate(!!col := ifelse(
            is.na(.data[[col]]),
            median(.data[[col]], na.rm = TRUE),
            .data[[col]]
          )) %>%
          ungroup()
      } else {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      }
      imputation_notes[[col]] <- paste0(col, ": ", missing_count, " NAs filled with grouped median")
      
    } else if (is.character(df[[col]])) {
      # Mode imputation for categorical
      mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_val
      imputation_notes[[col]] <- paste0(col, ": ", missing_count, " NAs filled with mode ('", mode_val, "')")
    }
  }
  
  audit_log$imputation <- imputation_notes
  list(df = df, audit = audit_log)
}