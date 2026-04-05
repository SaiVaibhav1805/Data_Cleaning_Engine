standardize_categoricals <- function(df, audit_log) {
  library(dplyr)
  library(stringr)
  
  notes <- list()
  
  df <- df %>% mutate(across(where(is.character), ~ {
    original <- .x
    cleaned <- str_to_lower(str_trim(.x))  # lowercase + trim
    
    # Common synonyms mapping
    cleaned <- case_when(
      cleaned %in% c("yes", "y", "true", "1", "t") ~ "yes",
      cleaned %in% c("no", "n", "false", "0", "f") ~ "no",
      cleaned %in% c("male", "m") ~ "male",
      cleaned %in% c("female", "f") ~ "female",
      TRUE ~ cleaned
    )
    cleaned
  }))
  
  audit_log$standardization <- "Categorical labels normalized: yes/no, gender variants, boolean strings"
  list(df = df, audit = audit_log)
}