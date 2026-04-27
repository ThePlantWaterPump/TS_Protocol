plot_destructive <- function(data, 
                             x, 
                             y,
                             fill          = x,
                             shape         = NULL,
                             color_palette = NULL,
                             x_lab         = "x",
                             y_lab         = "y",
                             facet         = NULL,
                             facet_grid    = NULL, 
                             title         = NULL,
                             show.points   = TRUE,
                             ncol  = 1) {
  
  library(rlang)
  
  # Base plot
  p <- ggplot(data, 
              aes(
                x = !!sym(x), 
                y = !!sym(y), 
                fill = !!sym(fill)
              )
  ) +
    geom_boxplot(
      width = 0.5, 
      lwd = 0.4, 
      alpha = 0.7, 
      outlier.shape = NA, 
      color = "black"
    )
  
  # Points (robuste avec ou sans shape)
  if (show.points) {
    
    if (!is.null(shape)) {
      p <- p + geom_point(
        aes(
          shape = !!sym(shape)
        ),
        position = ggbeeswarm::position_quasirandom(width = 0.2),
        alpha = 1,
        size = 1.5
      )
    } else {
      p <- p + geom_point(
        position = ggbeeswarm::position_quasirandom(width = 0.2),
        alpha = 1,
        size = 1.5,
        color = "grey2"
      )
    }
    
  }
  
  # Facet wrap
  if (!is.null(facet)) {
    p <- p + facet_wrap(as.formula(paste("~", facet)), ncol = ncol)
  }
  
  # Facet grid
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  
  # Title
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  # Color palette
  if (!is.null(color_palette)) {
    p <- p + 
      scale_color_manual(values = color_palette) +
      scale_fill_manual(values = color_palette)
  }
  
  # Theme
  p <- p +
    theme_few() +
    ylab(y_lab) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  return(p)
}