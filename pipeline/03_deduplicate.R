remove_duplicates <- function(df, audit_log) {
  library(stringr)
  
  before <- nrow(df)
  
  # Step 1: Exact duplicates
  df <- unique(df)
  exact_removed <- before - nrow(df)
  
  # Step 2: Fuzzy duplicates on character columns
  char_cols <- names(df)[sapply(df, is.character)]
  fuzzy_removed <- 0
  
  if (length(char_cols) > 0) {
    # Create a normalized signature for each row
    signature <- apply(df[, char_cols, drop = FALSE], 1, function(row) {
      row <- tolower(trimws(row))
      row <- gsub("[^a-z0-9]", "", row)
      paste(sort(row), collapse = "_")
    })
    
    # Find duplicate signatures
    dup_sigs <- duplicated(signature)
    fuzzy_removed <- sum(dup_sigs)
    df <- df[!dup_sigs, ]
  }
  
  after <- nrow(df)
  
  audit_log$deduplication <- paste0(
    exact_removed, " exact duplicates removed, ",
    fuzzy_removed, " fuzzy duplicates removed. ",
    before, " -> ", after, " rows"
  )
  
  list(df = df, audit = audit_log)
}