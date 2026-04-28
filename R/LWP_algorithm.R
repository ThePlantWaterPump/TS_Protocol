#' Apply LWP cleaning algorithm. 
#' 
#' @param LWP_data LWP data to clean. Should contain columns "Plant_ID", "DateTime", and var_name.
#' @param var_name Name of the column to clean. By default "LWP".
#' @param Plants_ID Optional : Metadata of plant_id.
#' @param check.na Set to TRUE to clean for NA.
#' @param check.zero Set to TRUE to clean for zeros.
#' @param check.cycles Set to TRUE to check that day/night cycle are respected.
#' @param check.levels Set to TRUE to check that the buffer level is okey.
#' @param nan.th Threshold for tolerance of nan values per buffer. 
#' @param zero.th Threshold for tolerance of zero values per buffer.
#' @param cycle.alpha Factor of discrimination between day and night. (e.g. mean day values should be < cycle.alpha * mean night values)
#' @param slope.th Threshold for slope tolerance per buffer.
#' @param level.alpha Threshold for tolerance of mean level of the buffer compared to all buffers. 
#' 
clean.LWP <- function(LWP_data, 
                      var_name     = "LWP",
                      Plants_ID    = NULL, 
                      check.na     = T, 
                      check.zero   = T, 
                      check.cycles = T, 
                      check.slope  = T, 
                      check.level  = T, 
                      nan.th       = 0.5, 
                      zero.th      = 0.5, 
                      cycle.alpha  = 1.15, 
                      slope.th     = -0.1, 
                      level.alpha  = 10){
  
  Buffer_list <- create_buffers(LWP_data     = LWP_data, 
                                var_name = var_name,
                                Plants_ID    = Plants_ID,
                                check.na     = check.na,
                                check.zero   = check.zero, 
                                check.cycles = check.cycles, 
                                check.slope  = check.slope, 
                                check.level  = check.level, 
                                nan.th       = nan.th, 
                                zero.th      = zero.th, 
                                cycle.alpha  = cycle.alpha, 
                                slope.th     = slope.th, 
                                level.alpha  = level.alpha) 
  
  data_clean <- merge_buffer(Buffer_list)
  
  return(data_clean)
  
}

################################################################################

create_buffers <- function(LWP_data,
                           Plants_ID = NULL,
                           var_name = "LWP",
                           check.na = T,
                           check.zero = T,
                           check.cycles = T,
                           check.slope = T,
                           check.level = T,
                           nan.th = 0.5,
                           zero.th = 0.5,
                           cycle.alpha = 1.15,
                           slope.th = -0.1, level.alpha = 10){
  
  data <- LWP_data
  
  # Quality checks
  if(is.null(data$Date)){
    stop("Input data miss Date column.")
  }
  
  if(is.null(data$Time)){
    stop("Input data miss Time column.")
  }
  
  if(is.null(data[[var_name]])){
    stop(paste("Input data miss", var_name, "column."))
  }
  
  if(is.null(data$Plant_ID)){
    stop("Input data miss Plant_ID column.")
  }
  
  if(is.null(data$DateTime)){
    stop("Input data miss DateTime column.")
  }
  
  if(is.null(Plants_ID)){
    Plants_ID <- unique(data$Plant_ID)
  }
  
  Buffer_list <- list()
  Buffer_id <- 0
  
  for(plant_i in unique(data$Plant_ID)){
    temp_buff <- NULL
    temp_data <- data %>% filter(Plant_ID == plant_i)
    for(date_i in unique(temp_data$Date)){
      
      # Create Buffer
      Buffer <- temp_data %>% filter(Plant_ID == plant_i & Date == date_i)
      
      # ========================================================================
      # Check Nan
      if(check.na){
        if(check_nan(Buffer, var_name, threshold = nan.th) == F){
          next
        }
      }
      
      # Check zeros
      if(check.zero){
        if(check_zero(Buffer, var_name, threshold = zero.th) == F){
          next
        }
      }
      
      if(nrow(Buffer) > 1){ # Because cycles and slope only works for Buffer that contains multiple values
        # Check cycles
        if(check.cycles){
          if(check_cycle(Buffer, var_name, alpha = cycle.alpha) == F){
            next
          }
          
        }
        
        # Check slope
        if(check.slope){
          if(check_slope(Buffer, var_name, threshold = slope.th) == F){
            next
          }
        }
      }
      
      # ========================================================================
      # Save
      Buffer_list[[length(Buffer_list)+1]] <- list(Plant_ID = plant_i,
                                                   Date = date_i,
                                                   Buffer_id = Buffer_id,
                                                   Buffer = Buffer)
      Buffer_id <- Buffer_id + 1
    }
  }
  
  # Check level
  if(check.level){
    Buffer_list <- check_level2(Buffer_list, var_name, alpha = level.alpha)
  }
  
  return(Buffer_list)
}

################################################################################
check_nan <- function(Buffer, var_name, threshold = 0.5){
  check <- T
  if(any(is.na(Buffer[[var_name]])) && (sum(is.na(Buffer[[var_name]]))/length(Buffer[[var_name]])) > threshold){
    check <- F
  }
  
  return(check)
}

################################################################################
check_zero <- function(Buffer, var_name, threshold = 0.5){
  check <- T
  if(any(Buffer[[var_name]] == 0) && sum(Buffer[[var_name]] == 0, na.rm = TRUE)/length(Buffer[[var_name]]) > threshold){
    check <- F
  }
  
  return(check)
}

################################################################################
check_cycle <- function(Buffer, var_name, alpha){
  check      <- T
  mean_day   <- mean(Buffer[[var_name]][Buffer$Time > "06:00:00" & Buffer$Time <= "18:00:00"])
  mean_night <- mean(Buffer[[var_name]][Buffer$Time <= "06:00:00" | Buffer$Time > "18:00:00"])
  
  if(is.na(mean_day)){
    check <- F
  }else if(is.na(mean_night)){
    check <- F
  }else if(mean_day > alpha*mean_night){
    check <- F
  }
  
  return(check)
}

################################################################################
check_slope <- function(Buffer, var_name, threshold = 10){
  check <- T
  lm <- lm(data = Buffer, formula = as.formula(paste(var_name, "~ DateTime")))
  slope <- lm$coefficients[2]
  if(abs(slope) > threshold){
    check <- F
  }
  return(check)
}

################################################################################
check_level2 <- function(Buffer_list, var_name, alpha = 10){
  Buffer_list2 <- list()
  stats <- tibble()
  Buffer_data <- merge_buffer(Buffer_list)
  Plants_ID <- unique(Buffer_data$Plant_ID)
  
  # Extract stats per plant
  for(plant_i in Plants_ID){
    temp_df <- na.omit(Buffer_data %>% filter(Plant_ID == plant_i))
    stats <- rbind(stats, tibble(Plant_ID = plant_i,
                                 mean_var = mean(temp_df[[var_name]]),
                                 median_var = median(temp_df[[var_name]]),
                                 sd_var = sd(temp_df[[var_name]]),
                                 mad_var = mad(temp_df[[var_name]])))
  }
  
  # Go through Buffers
  for(buffer_i in Buffer_list){
    
    check    <- T
    plant_i  <- buffer_i$Plant_ID
    temp_mean <- stats$mean_var[stats$Plant_ID == plant_i]
    temp_sd <- stats$sd_var[stats$Plant_ID == plant_i]
    
    mvalue   <- mean(na.omit(buffer_i$Buffer[[var_name]]))  # Mean value of the buffer i
    
    if(is.na(mvalue)){
      stop("Mean value is NA.")
    }
    
    if(!is.na(mvalue) && !is.na(temp_mean) && !is.na(temp_sd)){
      if(mvalue < temp_mean - alpha*temp_sd | mvalue > temp_mean + alpha*temp_sd){
        check <- F
      }
    } else {
      check <- F
    }
    
    if(check){
      Buffer_list2[[length(Buffer_list2) + 1]] <- buffer_i
    }
  }
  
  return(Buffer_list2)
}

################################################################################

merge_buffer <- function(Buffer_list){
  Buffer_data <- tibble()
  for(i in Buffer_list){
    temp_buff           <- i$Buffer
    temp_buff$Buffer_id <- i$Buffer_id
    Buffer_data         <- rbind(Buffer_data, temp_buff)
  }
  return(Buffer_data)
}