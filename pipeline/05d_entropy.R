compute_entropy_scores <- function(df) {
  
  entropy_scores <- list()
  
  for (col in names(df)) {
    vals <- na.omit(df[[col]])
    if (length(vals) == 0) next
    
    if (is.numeric(vals)) {
      # Bin numeric into 10 buckets then compute entropy
      breaks <- tryCatch(
        cut(vals, breaks = 10, labels = FALSE),
        error = function(e) rep(1, length(vals))
      )
      freq <- table(breaks)
    } else {
      freq <- table(vals)
    }
    
    # Shannon entropy
    probs   <- as.numeric(freq) / sum(freq)
    probs   <- probs[probs > 0]
    entropy <- -sum(probs * log2(probs))
    
    # Normalize to 0-100
    # Max entropy = log2(n_unique)
    n_unique    <- length(freq)
    max_entropy <- if (n_unique > 1) log2(n_unique) else 1
    norm_entropy <- round((entropy / max_entropy) * 100, 1)
    
    # Stability interpretation
    stability <- if (norm_entropy >= 80) "Highly Distributed" else
      if (norm_entropy >= 50) "Moderate"           else
        if (norm_entropy >= 20) "Skewed"             else
          "Highly Concentrated"
    
    entropy_scores[[col]] <- list(
      entropy      = round(entropy, 3),
      norm_entropy = norm_entropy,
      stability    = stability
    )
  }
  
  entropy_scores
}