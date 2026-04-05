validate_and_score <- function(original_df, cleaned_df, entropy_scores = NULL) {
  
  scores <- list()
  
  for (col in names(cleaned_df)) {
    completeness <- 1 - mean(is.na(cleaned_df[[col]]))
    uniqueness   <- length(unique(na.omit(cleaned_df[[col]]))) /
      max(nrow(cleaned_df), 1)
    validity     <- mean(cleaned_df[[col]] != "" | is.na(cleaned_df[[col]]),
                         na.rm = TRUE)
    confidence   <- round(
      (completeness * 0.5 + min(uniqueness, 1) * 0.25 + validity * 0.25) * 100, 1
    )
    
    scores[[col]] <- list(
      completeness = as.numeric(completeness),
      uniqueness   = as.numeric(uniqueness),
      confidence   = as.numeric(confidence)
    )
  }
  
  # Component scores (all 0-100)
  avg_completeness <- mean(sapply(scores, function(x) x$completeness)) * 100
  dup_ratio        <- (1 - nrow(cleaned_df) / max(nrow(original_df), 1)) * 100
  avg_confidence   <- mean(sapply(scores, function(x) x$confidence))
  
  # Entropy bonus (0-10 bonus points)
  entropy_bonus <- 0
  if (!is.null(entropy_scores) && length(entropy_scores) > 0) {
    avg_entropy   <- mean(sapply(entropy_scores, function(x) x$norm_entropy))
    entropy_bonus <- round(avg_entropy / 10, 1)
  }
  
  # Weighted composite 0-100
  composite <- round(
    avg_completeness * 0.40 +
      (100 - dup_ratio) * 0.25 +
      avg_confidence   * 0.25 +
      entropy_bonus    * 0.10,
    1
  )
  composite <- min(composite, 100)
  
  # Readiness grade
  grade <- if (composite >= 90) "A" else
    if (composite >= 75) "B" else
      if (composite >= 60) "C" else "D"
  
  # Status label
  status <- if (composite >= 75) "Approved" else
    if (composite >= 50) "Needs Improvement" else
      "Rejected"
  
  # Status color
  status_color <- if (composite >= 75) "#68d391" else
    if (composite >= 50) "#f6ad55" else
      "#fc8181"
  
  list(
    column_scores   = scores,
    readiness_grade = grade,
    composite_score = composite,
    status          = status,
    status_color    = status_color,
    component_scores = list(
      completeness  = round(avg_completeness, 1),
      deduplication = round(100 - dup_ratio, 1),
      confidence    = round(avg_confidence, 1),
      entropy_bonus = entropy_bonus
    )
  )
}