validate_and_score <- function(original_df, cleaned_df) {
  library(dplyr)
  
  scores <- list()
  
  for (col in names(cleaned_df)) {
    completeness <- 1 - mean(is.na(cleaned_df[[col]]))
    uniqueness   <- n_distinct(cleaned_df[[col]], na.rm = TRUE) / max(nrow(cleaned_df), 1)
    validity     <- mean(cleaned_df[[col]] != "" | is.na(cleaned_df[[col]]), na.rm = TRUE)
    
    confidence <- round((completeness * 0.5 + min(uniqueness, 1) * 0.25 + validity * 0.25) * 100, 1)
    
    # Use named LIST instead of vector to avoid jsonlite warning
    scores[[col]] <- list(
      completeness = as.numeric(completeness),
      uniqueness   = as.numeric(uniqueness),
      confidence   = as.numeric(confidence)
    )
  }
  
  # Dataset Readiness Grade
  avg_completeness <- mean(sapply(scores, function(x) x$completeness))
  dup_ratio        <- 1 - (nrow(cleaned_df) / nrow(original_df))
  avg_confidence   <- mean(sapply(scores, function(x) x$confidence))
  
  composite <- (avg_completeness * 0.4 + (1 - dup_ratio) * 0.3 + avg_confidence / 100 * 0.3) * 100
  
  grade <- case_when(
    composite >= 90 ~ "A",
    composite >= 75 ~ "B",
    composite >= 60 ~ "C",
    TRUE ~ "D"
  )
  
  list(
    column_scores   = scores,
    readiness_grade = grade,
    composite_score = round(composite, 1)
  )
}