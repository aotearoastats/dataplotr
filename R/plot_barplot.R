# @include set_bar_theme

#' Draws barplots.
#'
#' @param data_frame A data frame
#'
#' @param x_var A factor
#' @param y_var A numerical variable
#'
#' @param main_title A string, optional main title of the barplot
#' @param bar_width A floating number, optional bar width
#'
#' @param x_title A string, optional title of the x-axis
#' @param x_limits A vector of length 2, optional scale limits, e.g.,
#'   c(4,10)
#' @param x_breaks A vector, optional scale breaks, e.g., c(4, 6, 9)
#' @param x_ticks Logical, FALSE switches off tick marks on the x-axis
#'
#' @param y_title A string, optional title of the y-axis
#' @param y_limits A vector of length 2, optional scale limits, e.g., c(0,
#'   1)
#' @param y_breaks A vector, optional breaks, e.g., c(0.0, 0.5, 1.0)
#'
#' @param y_facet A formular, an optional grid for the y-variable
#'   controlled by y_facet e.g., "SCHOOL ~ ." to have the y-variable sliced
#'   horizontally by a school name variable, or ". ~ SCHOOL" to have the
#'   y-variable sliced vertically by a school name variable. Defaults to
#'   FALSE.
#' @param label_vars An optional label for the bars , e.g, "percent(FREQ)"
#' @param percentage logical, if TRUE assumes that y_var is a percentage
#'
#' @param fill_vars A factor, an optional variable by which to slice the
#'   x-variable, used for filling the bars
#' @param fill_palette A vector of length equal to the number of categories
#'   of fill_vars, an optional colour palette, defaults to fill_brewer
#' @param colour_vars A factor, an optional variable by which to slice the
#'   x-variable, usually equal to fill_vars, used for colouring the outside
#'   edge of the bars
#' @param colour_palette A vector of length equal to the number of
#'   categories of colour_vars, an optional colour palette, usually equal to
#'   fill_palette, but can be used to highlight the bar borders, defaults to
#'   colour_breweR
#'
#' @param pos A position, an optional placement rule for the bars, defaults
#'   to "dodge"
#' @param barplot_alpha A floating number between 0 and 1, an optional
#'   degree of transparency for the bars, default at 1
#'
#' @param colour_title legend guide for scale_colour, setting a title
#'   activates the legend, requires colour_vars to be set, defaults to
#'   FALSE.
#' @param fill_title legend guide for scale_fill, setting a title activates
#'   the legend, requires fill_vars to be set, default at FALSE.
#'
#' @param legend_labels a vector of labels for the legend, e.g,
#'   c("M\\u0101ori", "Pakeh\\u0101")
#' @param legend_rev determines whether the order of the legend labels
#'   should be reversed
#' @param flip Flips x-axis and y-axis if TRUE, defaults to FALSE
#'
#' @return a bar plot object
#' @examples
#'
#' \dontrun{
#' library(tidyverse)
#' iris.plot <- iris %>% group_by(Species) %>% summarize(Sepal.Length=mean(Sepal.Length))
#'
#'   #Simple bar plot
#'   p <- plot_barplot(iris.plot,
#'                     x_var = "Species",
#'                     y_var = "Sepal.Length")
#'   print(p)
#'
#'   #Same bar plot with fill by Species and border colour by Species reversed
#'   p <- plot_barplot(iris.plot,
#'                     main_title="Sepal.Length by Species on the Iris Data Set",
#'                     x_var = "Species",
#'                     y_var = "Sepal.Length",
#'                     fill_title = "Species",
#'                     fill_vars = "Species",
#'                     colour_vars = "rev(Species)")
#'   print(p)
#'
#'   #Same bar plot with a facet for Sepal.Width and a legend with a title
#'   p <- plot_barplot(iris,
#'                     x_var = "Species",
#'                     y_var = "Sepal.Length",
#'                     fill_vars = "Species",
#'                     y_facet = "Sepal.Width ~ .",
#'                     fill_title = "Species",
#'                     flip = TRUE)
#'   print(p)}
#'
#' @section Bugs:
#' Guide colour and guide fill currently take the same arguments and can
#' not be controlled individually.
#'
#' @export
plot_barplot <- function(...) {
  set_bar_theme()
  elements <- list(bar,
                   bar_labels,
                   scales,
                   colour,
                   legend,
                   facet,
                   flip_axes,
                   x_ticks,
                   title
                   )
  ## 30/4/18 AK
  ## facet interacts with title
  ## If you run into the following error (after checking the data) it may be a problem
  ## with the relative placements of facet and title and possbily flip_axes. Reproduce
  ## error as follows: move facet after title and try to create a plot with a facet and
  ## a title. It will fail with the following error:
  ##
  ## Error in combine_vars(data, params$plot_env, cols, drop = params$drop)
  ## At least one layer must contain all variables used for facetting
  d <- dataplot(..., element_function=elements)
  return(d)
}
