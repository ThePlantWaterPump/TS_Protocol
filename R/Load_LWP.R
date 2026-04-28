#' Extract LWP data from all .CSV files in the given folder.
#' 
#' @param path Folder path in wich we want to compile every csv file.
#' @param sep Separator of columns. By default ","
#' @param columns Column number to keep. By default c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12).
#' @param colnames Column names to apply. By default c("Date", "Time","Chambertemp", "dT", "WetBulb", "LWP", "Intercept", "Slope", "Correction_dT", "Correctionfactor", "BatteryLevel")
#' 
################################################################################
load_LWP <- function(path, 
                     sep = ",", 
                     columns = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12),
                     colnames = c("Date", "Time","Chambertemp", "dT",
                                  "WetBulb", "LWP", "Intercept", "Slope", 
                                  "Correction_dT", "Correctionfactor", "BatteryLevel")
){
  
  ls <- list.files(path, pattern = ".CSV")
  
  LWP_data <- tibble()
  for(i in ls){
    cat("Loading ", i, "\n")
    sensor.name       <- strsplit(i, split = ".CSV")[[1]]   # Extract sensor name from filename
    LWP_temp          <- load_psy_file(file = paste0(path, i), 
                                       columns = columns,
                                       colnames = colnames)
    # LWP_temp$Plant_ID <- Plants_ID$Plant[Plants_ID$PSY == sensor.name] 
    LWP_temp$PSY      <- sensor.name
    LWP_data          <- rbind(LWP_data, LWP_temp)
  }
  return(LWP_data)
}

################################################################################

load_psy_file <- function(file, 
                          columns, 
                          colnames){
  data_i <- read.csv(file = file, sep = "\n", header = F, stringsAsFactors = F, fileEncoding = 'ISO-8859-1')
  
  data_clean <- tibble()
  for(i in seq(1, nrow(data_i))){
    
    # Check with comma
    row_temp <- unlist(strsplit(data_i[i,], split = ","))
    
    # Check if format is with semicolumns -> replace , by . and ; by ,
    if(grepl("^\\d{2}/\\d{2}/\\d{4};\\d{2}:\\d{2}:\\d{2};\\d{2}$", row_temp[1])){
      row_temp2 <- data_i[i,] 
      row_temp2 <- gsub(",", ".", row_temp2, fixed = TRUE)
      row_temp2 <- gsub(";", ",", row_temp2, fixed = TRUE)
      row_temp2 <- unlist(strsplit(row_temp2, split = ","))
    }else if(grepl("^\\d{2}/\\d{2}/\\d{4}$", row_temp[1])){
      row_temp2 <- row_temp
    }else{
      # cat(row_temp)
      # stop("Wrong data format ! |", row_temp)
      next
    }
    
    data_clean <- rbind(data_clean, row_temp2[columns])
    
  }
  
  colnames(data_clean) <- c(colnames)  # Rename columns
  
  data_clean$LWP <- as.numeric(data_clean$LWP)
  
  # Define DateTime in POSIXct format
  data_clean <- data_clean %>% mutate(DateTime = as.POSIXct(paste0(Date, " ", Time), 
                                                            format="%d/%m/%Y %H:%M:%S")
  )
  
  return(data_clean)
}