create_buffers <- function(LWP_data, 
                           Plants_ID = NULL, 
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
  
  if(is.null(data$LWP)){
    stop("Input data miss LWP column.")
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
  
  for(plant_i in unique(Plants_ID)){
    temp_buff <- NULL
    temp_data <- data %>% filter(Plant_ID == plant_i)
    for(date_i in unique(temp_data$Date)){
      
      # print(paste0(plant_i, " : ", date_i))
      
      # Create Buffer
      Buffer <- temp_data %>% filter(Plant_ID == plant_i & Date == date_i)
      
      # message("FLAG")
      
      # ========================================================================
      # Check Nan
      if(check.na){
        if(check_nan(Buffer, threshold = nan.th) == F){
          next
        }  
      }
      
      # Check zeros
      if(check.zero){
        if(check_zero(Buffer, threshold = zero.th) == F){
          next
        }  
      }
      
      if(nrow(Buffer) > 1){ # Because cycles and slope only works for Buffer that contains multiple values
        # Check cycles
        if(check.cycles){
          if(check_cycle(Buffer, alpha = cycle.alpha) == F){
            next
          } 
          
        }
        
        # Check slope
        if(check.slope){
          if(check_slope(Buffer, threshold = slope.th) == F){
            next
          }  
        }  
      }
      
      
      
      # ========================================================================  
      # Save 
      # print("Saving Buffer")
      Buffer_list[[length(Buffer_list)+1]] <- list(Plant_ID = plant_i, 
                                                   Date = date_i,
                                                   Buffer_id = Buffer_id,
                                                   Buffer = Buffer)
      Buffer_id <- Buffer_id + 1
    }
    
    # message("FLAG")
  }  
  
  # Check level
  if(check.level){
    # message("FLAG")
    Buffer_list <- check_level2(Buffer_list, alpha = level.alpha)
    
  }
  
  return(Buffer_list)
}
#===============================================================================
#===============================================================================
check_nan <- function(Buffer, threshold = 0.5){
  check <- T
  if(any(is.na(Buffer$LWP)) && (sum(is.na(Buffer$LWP))/length(Buffer$LWP)) > threshold){
    check <- F
  }
  
  return(check)
}
#===============================================================================
#===============================================================================
check_zero <- function(Buffer, threshold = 0.5){
  check <- T
  if(any(Buffer$LWP == 0) && sum(Buffer$LWP == 0, na.rm = TRUE)/length(Buffer$LWP) > threshold){
    check <- F
  }
  
  return(check)
}
#===============================================================================
#===============================================================================
check_cycle <- function(Buffer, alpha){
  
  check      <- T
  mean_day   <- mean(Buffer$LWP[Buffer$Time > "06:00:00" & Buffer$Time <= "18:00:00"])
  mean_night <- mean(Buffer$LWP[Buffer$Time <= "06:00:00" | Buffer$Time > "18:00:00"])
  # print(paste0("mean_day : ", mean_day, " | mean_night : ", mean_night))
  
  if(is.na(mean_day)){
    check <- F
  }else if(is.na(mean_night)){
    check <- F
  }else if(mean_day > alpha*mean_night){
    check <- F
  }
  
  return(check)
}
#===============================================================================
#===============================================================================
check_slope <- function(Buffer, threshold = 10){
  check <- T
  lm <- lm(data = Buffer, formula = LWP ~ DateTime)
  slope <- lm$coefficients[2]
  # print(paste0("Slope : ", slope))
  if(abs(slope) > threshold){
    check <- F
  }
  return(check)
}
#===============================================================================
#===============================================================================
check_level <- function(Buffer, temp_buff, alpha = 3){
  check <- T
  mean_0 <- mean(temp_buff$LWP)
  sd_0 <- sd(temp_buff$LWP)
  mean_1 <- mean(Buffer$LWP)
  sd_1 <- sd(Buffer$LWP)
  
  # print(paste0("Slope : ", slope))
  if(mean_1 < mean_0 - alpha*sd_0 | mean_1 > mean_0 + alpha*sd_0){
    check <- F
  }
  return(check)
}

#===============================================================================
#===============================================================================

check_level2 <- function(Buffer_list, alpha = 10){
  Buffer_list2 <- list()
  stats <- tibble()
  Buffer_data <- merge_buffer(Buffer_list)
  Plants_ID <- unique(Buffer_data$Plant_ID)
  
  # Extract stats per plant
  for(plant_i in Plants_ID){
    temp_df <- na.omit(Buffer_data %>% filter(Plant_ID == plant_i))
    stats <- rbind(stats, tibble(Plant_ID = plant_i,
                                 mean_LWP = mean(temp_df$LWP),
                                 median_LWP = median(temp_df$LWP),
                                 sd_LWP = sd(temp_df$LWP),
                                 mad_LWP = mad(temp_df$LWP)))
  }
  
  # Go through Buffers
  for(buffer_i in Buffer_list){
    
    check    <- T
    plant_i  <- buffer_i$Plant_ID
    temp_mean <- stats$mean_LWP[stats$Plant_ID == plant_i]
    temp_sd <- stats$sd_LWP[stats$Plant_ID == plant_i]
    
    mvalue   <- mean(na.omit(buffer_i$Buffer$LWP))  # Mean value of the buffer i
    
    if(is.na(mvalue)){
      # mvalue <- mean(buffer_i$LWP)
      stop("Mean value is NA.")
    }
    
    if(mvalue < temp_mean - alpha*temp_sd | mvalue > temp_mean + alpha*temp_sd){
      check <- F
    }
    
    if(check){
      Buffer_list2[[length(Buffer_list2) + 1]] <- buffer_i
    }
  }
  
  return(Buffer_list2)
}

#===============================================================================
#===============================================================================
merge_buffer <- function(Buffer_list){
  Buffer_data <- tibble()
  for(i in Buffer_list){
    temp_buff           <- i$Buffer
    temp_buff$Buffer_id <- i$Buffer_id
    Buffer_data         <- rbind(Buffer_data, temp_buff)
  }
  return(Buffer_data)
}

#===============================================================================
#===============================================================================

clean.LWP <- function(LWP_data, 
                      Plants_ID = NULL, 
                      check.na = T, 
                      check.zero = T, 
                      check.cycles = T, 
                      check.slope = T, 
                      check.level = T, 
                      nan.th = 0.5, 
                      zero.th = 0.5, 
                      cycle.alpha = 1.15, 
                      slope.th = -0.1, level.alpha = 10){
  
  Buffer_list <- create_buffers(LWP_data     = LWP_data, 
                                Plants_ID       = Plants_ID,
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