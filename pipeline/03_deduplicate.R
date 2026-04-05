remove_duplicates <- function(df, audit_log) {
  library(dplyr)
  
  before <- nrow(df)
  df <- df %>% distinct()
  after <- nrow(df)
  
  removed <- before - after
  audit_log$deduplication <- paste0(removed, " duplicate rows removed (", before, " → ", after, " rows)")
  
  list(df = df, audit = audit_log)
}