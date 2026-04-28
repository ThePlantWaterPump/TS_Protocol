#' Load Weight data from .csv files.
#' @param path Path to folder.
#' @param skip Number of lines to skip at reading. Default is 0.
#' @param DT.format DateTime format for reading into POSIXct formating. Default if "%d/%m/%Y %H:%M:%S".
#' 
load_Weigth <- function(path, skip = 0, DT.format = "%d/%m/%Y %H:%M:%S"){
  
  lf <- list.files(path, pattern = ".csv")
  
  if(is.null(lf)){
    stop(path, " is empty.")
  }
  
  W_data <- tibble()
  
  # Collect all
  for(f in lf){
    
    cat("Loading ",f, "\n")
    scale_id <- str_split(f, pattern = ".csv")[[1]][1]
    
    temp <- read.csv(paste0(path, f), sep = ",", header = T)
    
    W_temp <- tibble(Date     = temp$Date,
                     Time     = temp$Time,
                     Weight   = as.numeric(temp$Data),
                     Scale    = scale_id)
    
    W_data <- rbind(W_data, W_temp)
  }
  
  # COnvert datetime to posixc
  W_data <- W_data %>% 
    mutate(DateTime = as.POSIXct(paste0(Date, " ", Time), 
                                 format = DT.format))
  
  if(any(sapply(W_data$DateTime, is.na))){
    warning("\n There are NULL values in DateTime. Check DateTime format.")
  }
  
  return(W_data)
}