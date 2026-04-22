clean_weight <- function(W_raw, 
                         refill_th = 1){
  
  data2plot <- W_raw %>% left_join(Plants_ID[c("Plant_ID", "Treatment")])
  
  p <- plot_timeseries(data = data2plot
                  ,x = "DateTime", y = "Weight"
                  ,color = "Treatment", show.lines = F) +
    geom_line(aes(x = DateTime, y = Weight, color = Treatment, group = Plant_ID)) +
    ggtitle("Raw Weight")
  plot(p)
  # ============================================================================ #
  # Compute dW and detect refill
  
  W_temp <- W_raw %>%
    arrange(Plant_ID, DateTime) %>%
    group_by(Plant_ID) %>%
    mutate(
      dW     = Weight - lag(Weight),
      Refill = ifelse(abs(dW) > refill_th, TRUE, FALSE)
    )
  # ============================================================================ #
  # Plot W_temp with detected refill
  p <- plot_timeseries(data = W_temp
                       ,x = "DateTime", y = "Weight"
                       ,color = "Plant_ID"
                       ,show.lines = F) +
    
    geom_line(aes(x = DateTime, y = Weight, group = Plant_ID)) +
    geom_point(data = filter(W_temp, Refill == T),
               aes(x = DateTime, y = Weight),
               color = "blue", size = 3, shape = 18) +
    ggtitle("Refilling detection")
  
  
  plot(p)
  # ggsave(filename = paste0(plot_path, "W_refill_detection.png"), plot = p, 
  #        device = "png", width = 12, height = 6)
  # ============================================================================ #
  # Compute dW, if there is Refill -> store jumps, then correct weight at jumps
  W_clean <- W_temp %>%
    arrange(Plant_ID, DateTime) %>%
    group_by(Plant_ID) %>%
    mutate(
      dW                = Weight - lag(Weight),
      Refill            = ifelse(abs(dW) > refill_th, TRUE, FALSE), # Detect refill
      jump              = ifelse(Refill, dW, 0),        # Store jumps due to refill
      total_jump        = cumsum(replace_na(jump, 0)),  # Sum jumps
      Weight_corrected  = Weight - total_jump,          # Weight correction
      # transpiration_cum = Weight_corrected[1] - Weight_corrected  # Cummulated loss
    ) %>%
    ungroup() %>% 
    select(DateTime, Plant_ID, Weight_corrected) %>% 
    rename(Weight = Weight_corrected)
  # ============================================================================ #
  # PLOT CLEAN WEIGHT
  data2plot <- left_join(W_clean, Plants_ID[c("Plant_ID", "Treatment")])
  p <- plot_timeseries(data = data2plot
                       ,x = "DateTime", y = "Weight"
                       ,color = "Treatment"
                       ,show.lines = F) +
    geom_line(aes(x = DateTime, y = Weight, 
                  group = Plant_ID, color = Treatment)) +
    ggtitle("Clean Weight")
  plot(p)
  # ggsave(filename = paste0(plot_path, "W_clean.png"), plot = p,
  #        device = "png", width = 12, height = 6)
  # ============================================================================ #
  # Write to disk
  # write.csv(x = W_clean, file = paste0(data_path_clean, "W_clean.csv"), row.names = F)
  return(W_clean)
}

# ============================================================================ #

clean.LWP <- function(LWP_data, 
                      var_name="LWP",
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
# ============================================================================ #