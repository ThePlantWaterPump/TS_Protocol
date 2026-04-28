#' Destructive Plot with Boxplots and Points
#'
#' This function creates a destructive plot combining boxplots and points,
#' allowing for detailed visualization of data distributions and individual data points.
#' It is particularly useful for comparing groups and identifying outliers.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x The variable to map on the x-axis. Should be a column name in `data`.
#' @param y The variable to map on the y-axis. Should be a column name in `data`.
#' @param fill The variable to use for filling the boxplots. Defaults to `x`.
#' @param shape The variable to map to the shape of the points. If `NULL`, points are not shaped.
#' @param color_palette A vector of colors to use for the plot. If `NULL`, the default color palette is used.
#' @param x_lab The label for the x-axis. Defaults to "x".
#' @param y_lab The label for the y-axis. Defaults to "y".
#' @param facet The variable to facet the plot by. If `NULL`, no faceting is applied.
#' @param facet_grid A formula for faceting the plot into a grid. If `NULL`, no grid faceting is applied.
#' @param title The title of the plot. If `NULL`, no title is displayed.
#' @param show.points A logical value indicating whether to display individual points. Defaults to `TRUE`.
#' @param ncol The number of columns for faceting. Defaults to 1.
#'
#' @return A `ggplot` object representing the destructive plot.
#'
#' @examples
#' # Example usage:
#' data(mtcars)
#' plot_destructive(mtcars, x = "cyl", y = "mpg", fill = "cyl", shape = "gear")
#'
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point facet_wrap facet_grid ggtitle
#' @importFrom ggplot2 scale_color_manual scale_fill_manual theme element_blank
#' @importFrom ggbeeswarm position_quasirandom
#' @importFrom ggthemes theme_few
#'
#' @export
#' 
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