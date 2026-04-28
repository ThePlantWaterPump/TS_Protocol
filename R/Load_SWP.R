#' Extract weighting scale data from .data files in input folder.
#' 
#' @param path_SWP Path to folder containing .dat files.
#' @param sep Separator, by default "\t"
#' @param skip Lines to skip when reading, by default 0
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