#' Load .csv file and format DateTime column in appropriate POSIXct format.
#' 
#' @param file The name of the .csv file to load.
#' @param anytime Boolean ; set to TRUE for correction in case DateTime is missing time at midnight.
load_csv_custom <- function(file, anytime = F){
  
  
  df <- read.csv(file)
  
  if(anytime){
    df <- df %>% mutate(DateTime = anytime(DateTime))
  }
  
  df <- df %>% 
    mutate(
      DateTime = ifelse(
        grepl(" ", DateTime),
        as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        as.POSIXct(paste(DateTime, "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      ) 
    ) %>% 
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  # %>% mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  return(df)
}