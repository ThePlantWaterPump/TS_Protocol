#' Compute transpiration rate from weight. Weights values are summarized as median per hour.
#' 
#' @param W Weight data. Should contain columns : "DateTime", "Plant_ID", "Weight".
#' @param th Threshold of dW to filter. 
#' @param DT.format DateTime column format for reformating of DateTime column when grouping per Date and Hour. By default "%d-%m-%y %H:%M:%S".

compute_Tr <- function(W, th, DT.format = "%d-%m-%y %H:%M:%S"){
  
  
  # Quality check
  for(c in c("DateTime", "Plant_ID", "Weight")){
    if(!(c %in% colnames(W))){
      stop(paste0("Column '", c, "' is needed and is not in provided dataset."))
    }
  }
  
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
    mutate(Tr = dW/as.numeric(dt))%>%   # g/h
    filter(Tr >= 0)  # Remove negative Tr values
  
  if(any(sapply(Tr_data$DateTime, is.na))){
    warning("\n There are NULL values in DateTime. Check DateTime format.")
  }
  
  return(Tr_data)
}