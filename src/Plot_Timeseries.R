# Custom function to plot timeseries with day-night visualization.
plot_timeseries <- function(data, 
                            x, 
                            y,
                            color         = "Treatment",
                            fill          = "Treatment",
                            group         = NULL,
                            color_palette = NULL,
                            mean          = NULL, sd = NULL,
                            x_lab         = "x",
                            y_lab         = "y",
                            facet         = NULL,
                            ncol          = 1,
                            light.hours   = c("11:00:00", "23:00:00"),
                            light.rect    = TRUE,
                            show.lines    = FALSE
) {
  #'@param data The input dataframe, containing at least a DateTime column and a y column
  #'@param x The name of the column to be used as x variable. The values in it should be in POSIXct format.
  #'@param y The name of the column to be used as y. Values should be numeric.
  #'@param color The name of the column to be used as color factor. Values should be as factor.
  #'@param color_palette A vector containing colors per level of color.
  #'@param mean If provided, show lines
  #'@param sd If provided, show ribbon with ymin = mean-sd and ymax = mean-sd
  #'@param facet If provvided, will facet the graph with the given column. 
  #'@param ncol If facet is provided, will fix the number of columns of the facet. 
  #'@param light.hours Vector of length 2 containing the start and end time (in %H:%M:%S format) of the lighting period.
  #'@param light.rect Boolean. If set to FALSE, will hide the day-night visualization.
  #'@param show.lines Boolean. If set to TRUE, will automatically plot data from "x" and "y" column names.
  
  p <- ggplot(data) 
  
  if(light.rect){
    p <- p +
      geom_rect(
        data = unique(data.frame(
          xmin = as.POSIXct(paste0(format(data[[x]], "%Y-%m-%d"), " ", light.hours[1]),
                            format = "%Y-%m-%d %H:%M:%S"),
          xmax = as.POSIXct(paste0(format(data[[x]], "%Y-%m-%d"), " ", light.hours[2]),
                            format = "%Y-%m-%d %H:%M:%S"),
          ymin = -Inf,
          ymax = Inf
        )),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "white", alpha = 1, inherit.aes = FALSE
      ) 
  }
  
  # Lines
  if (show.lines) {
    aes_args <- aes(x = !!sym(x), y = !!sym(y))
    
    if (!is.null(color)) {
      aes_args <- modifyList(aes_args, aes(color = !!sym(color)))
    }
    if (!is.null(group)) {
      aes_args <- modifyList(aes_args, aes(group = !!sym(group)))
    }
    
    p <- p + geom_line(aes_args, linewidth = 1)
  }
  
  # Facet
  if(!is.null(facet)){
    p <- p + facet_wrap(as.formula(paste("~", facet)), ncol = ncol) 
  }
  
  # Colors
  if (!is.null(color_palette)) {
    p <- p + scale_color_manual(values = color_palette) +
      scale_fill_manual(values = color_palette)
  }
  
  # Theme
  p <- p +
    ylab(y_lab) +
    theme_few() +
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5)) 
  
  if(x == "DateTime"){
    p <- p + scale_x_datetime(date_breaks = "1 day",
                              date_labels = "%d/%m",
                              name = x_lab) 
  } else if(x == "Hour"){
    p <- p + scale_x_continuous(breaks = seq(0, max(data[[x]], na.rm = TRUE)+2, by = 24))
  }
  
  if(light.rect){
    p <- p + theme(panel.background = element_rect(fill = '#dee5e8', color = '#dee5e8'))
  }
  
  return(p)
}