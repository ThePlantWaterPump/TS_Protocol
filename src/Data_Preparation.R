# ============================================================================ #

load_psy_file <- function(file){
  #' Simple function to extract data from raw CSV files for LWP
  #' @param file the path and the file name
  
  data_i <- read.csv(file = file, sep = "\n", header = F, stringsAsFactors = F, fileEncoding = 'ISO-8859-1')
  
  data_clean <- tibble()
  for(i in seq(1, nrow(data_i))){
    # print(i)
    row_temp <- unlist(strsplit(data_i[i,], split = ","))
    
    # Check that row is in the correct format
    if(grepl("^\\d{2}/\\d{2}/\\d{4}$", row_temp[1])){
      data_clean <- rbind(data_clean, row_temp[c(1, 2, 6)])
    }else(next)
    
  }
  
  colnames(data_clean) <- c("Date","Time", "LWP")  # Rename columns
  
  data_clean$LWP <- as.numeric(data_clean$LWP)
  
  # Define DateTime in POSIXct format
  data_clean <- data_clean %>% mutate(DateTime = as.POSIXct(paste0(Date, " ", Time), 
                                                            format="%d/%m/%Y %H:%M:%S")
                                      )
  
  return(data_clean)
}

# ============================================================================ #

load_LWP <- function(path){
  #' Loop function to apply load_psy_file on every csv file of the path
  #' @param path Folder path in wich we want to compile every csv file.
  #' 
  
  ls <- list.files(path)
  
  LWP_data <- tibble()
  for(i in ls){
    cat("Loading ", i, "\n")
    sensor.name       <- strsplit(i, split = ".CSV")[[1]]   # Extract sensor name from filename
    LWP_temp          <- load_psy_file(file = paste0(path_LWP, i))
    # LWP_temp$Plant_ID <- Plants_ID$Plant[Plants_ID$PSY == sensor.name] 
    LWP_temp$PSY      <- sensor.name
    LWP_data          <- rbind(LWP_data, LWP_temp)
  }
  return(LWP_data)
}

# ============================================================================ #

load_SWP <- function(path_SWP, sep = "\t", skip = 0){
  
  ls <- list.files(path_SWP, pattern = ".dat")
  
  SWP_raw <- tibble()
  
  for(file in ls){
    
    cat("Loading ", file, "\n")
    
    # Find column names by reading the first row of the DAT file : 
    preview <- read.table(paste0(path_SWP, file), sep = sep, nrows = 5, skip = skip)
    # preview <- readLines(paste0(path_SWP, file), n = 5)  # Read first few lines
    
    header_line  <- which(preview$V1 == "TIMESTAMP")  # Find the first non-empty row
    column_names <- unlist(preview[header_line,])  # Split by tab
    
    # Read file
    data <- read.table(file = paste0(path_SWP, file), 
                       sep = sep, header = T, skip = skip + 3, stringsAsFactors = FALSE)
    
    # Change column names
    colnames(data) <- column_names
    
    # Loop through the columns. If the column name contains "Tension", extract the Teros_ID from it.
    SWP_temp <- tibble()
    coln <- colnames(data)
    for(col_i in coln){
      
      if(is.na(strsplit(col_i, split = "Tension")[[1]][1])){
        next
      }
      
      else if(strsplit(col_i, split = "Tension")[[1]][1] == ""){
        
        # Extract Teros_ID from Tension column name
        Teros_ID <- strsplit(col_i, split = "Tension")[[1]][2]
        # print(Teros_ID)
        
        # Create a temporary dataframe
        temp_data <- tibble(DateTime = data$TIMESTAMP,
                            Teros_ID = Teros_ID,
                            SWP      = as.numeric(data[col_i][[1]]))
        
        # Rbind the temp df with the full df
        SWP_temp <- rbind(SWP_temp, temp_data)
      }
    }
    
    SWP_raw <- rbind(SWP_raw, SWP_temp)
  }
  
  return(SWP_raw)
  
}

# ============================================================================ #

load_Weigth <- function(path, skip = 0, DT.format = "%d/%m/%Y %H:%M:%S"){
  
  lf <- list.files(path, pattern = ".csv")
  
  if(is.null(lf)){
    stop(path, " is empty.")
  }
  
  W_data <- tibble()
  
  # Collect all
  for(f in lf){
    
    # print(f)
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

# ============================================================================ #

compute_Tr <- function(W, th, DT.format = "%d-%m-%y %H:%M:%S"){
  
  Tr_raw <- W[order(W$DateTime),]
  
  Tr_data <- Tr_raw %>% 
    mutate(Hour = hour(DateTime)) %>%
    group_by(Plant_ID, Date, Hour) %>%
    summarise(Med_W = median(Weight)) %>% 
    mutate(Time = sprintf("%02d:00:00",Hour)) %>% 
    mutate(DateTime = as.POSIXct(paste0(Date, " ", Time),
                                 format = DT.format)) %>%
    group_by(Plant_ID) %>% 
    mutate(dW = lag(Med_W) - Med_W ,
           dt = DateTime - lag(DateTime)) %>% 
    filter(abs(dW) < th) %>% 
    mutate(Tr = dW/as.numeric(dt)) # g/h
  
  if(any(sapply(Tr_data$DateTime, is.na))){
    warning("\n There are NULL values in DateTime. Check DateTime format.")
  }
  
  return(Tr_data)
  
}

# ============================================================================ #