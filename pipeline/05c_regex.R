validate_formats <- function(df, audit_log) {
  
  format_issues <- list()
  
  # Regex patterns for common formats
  patterns <- list(
    email  = list(
      regex   = "^[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}$",
      keywords = c("email", "mail")
    ),
    phone  = list(
      regex   = "^[\\+]?[0-9\\s\\-\\(\\)]{7,15}$",
      keywords = c("phone", "mobile", "contact", "tel")
    ),
    zip    = list(
      regex   = "^[0-9]{4,6}$",
      keywords = c("zip", "postal", "pincode", "pin")
    ),
    date   = list(
      regex   = "^\\d{4}-\\d{2}-\\d{2}$|^\\d{2}/\\d{2}/\\d{4}$",
      keywords = c("date", "dob", "birth", "created", "updated")
    ),
    gender = list(
      regex   = "^(male|female|other|m|f)$",
      keywords = c("gender", "sex")
    )
  )
  
  for (col in names(df)) {
    if (!is.character(df[[col]])) next
    
    col_lower <- tolower(col)
    matched_format <- NULL
    
    # Match column name to a known format
    for (fmt_name in names(patterns)) {
      if (any(sapply(patterns[[fmt_name]]$keywords, 
                     function(k) grepl(k, col_lower)))) {
        matched_format <- fmt_name
        break
      }
    }
    
    if (is.null(matched_format)) next
    
    pattern   <- patterns[[matched_format]]$regex
    vals      <- na.omit(df[[col]])
    valid     <- grepl(pattern, vals, ignore.case = TRUE)
    n_invalid <- sum(!valid)
    
    if (n_invalid > 0) {
      format_issues[[col]] <- paste0(
        col, " (", matched_format, "): ",
        n_invalid, " invalid format(s) detected"
      )
    }
  }
  
  if (length(format_issues) == 0) {
    audit_log$formats <- "All detected format columns passed regex checks"
  } else {
    audit_log$formats <- format_issues
  }
  
  list(df = df, audit = audit_log)
}