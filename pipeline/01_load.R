load_and_normalize <- function(filepath) {
  library(readr)
  library(janitor)
  library(stringr)
  library(lubridate)
  
  raw_df <- read_csv(filepath, show_col_types = FALSE)
  
  # Clean column names on BOTH so they match
  original_df <- clean_names(raw_df)
  df <- original_df
  
  # Trim whitespace
  char_cols <- names(df)[sapply(df, is.character)]
  for (col in char_cols) {
    df[[col]] <- stringr::str_trim(df[[col]])
  }
  
  # Auto-parse dates
  for (col in char_cols) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(df[[col]], orders = c("ymd", "dmy", "mdy"))
    )
    if (sum(!is.na(parsed)) > 0.5 * length(df[[col]])) {
      df[[col]] <- as.Date(parsed)
    }
  }
  
  # Auto-convert numeric strings
  char_cols2 <- names(df)[sapply(df, is.character)]
  for (col in char_cols2) {
    num <- suppressWarnings(as.numeric(df[[col]]))
    if (sum(!is.na(num)) > 0.8 * sum(!is.na(df[[col]]))) {
      df[[col]] <- num
    }
  }
  
  list(original = original_df, cleaned = df)
}