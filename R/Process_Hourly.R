#' Process mean, sd, median and mad of a dataframe. 
#' 
#' @param df Dataframe to summarize. 
#' @param group_var Vector of column names on which to group for summarizing.
#' @param value_var Column name of the column to summarize.
#' 
process_hourly <- function(df, group_var = NULL, value_var) {
  
  if(!("DateTime" %in% colnames(df))){
    stop("No DateTime column found in input df.")
  }
  
  df %>%
    mutate(
      Date = as.Date(DateTime),
      Hour = hour(DateTime)
    ) %>%
    group_by(!!!syms(group_var), Date, Hour) %>%
    summarise(
      Mean   = mean({{ value_var }}, na.rm = TRUE),
      SD     = sd({{ value_var }}, na.rm = TRUE),
      Median = median({{ value_var }}, na.rm = TRUE),
      MAD    = mad({{ value_var }}, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      DateTime := as.POSIXct(
        paste0(Date, " ", sprintf("%02d:00:00", Hour)),
        format = "%Y-%m-%d %H:%M:%S"
      )
    )
}