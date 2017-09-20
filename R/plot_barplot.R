plot_barplot <- function(data_frame, x_var, y_var, main_title = NULL, bar_width = NULL, x_title = ggplot2::waiver(), x_ticks=TRUE, y_title = ggplot2::waiver(), x_limits = NULL, y_limits = NULL, x_breaks = ggplot2::waiver(), y_breaks = ggplot2::waiver(), y_facet = FALSE, label_vars = FALSE, percentage = FALSE, colour_vars = NULL, fill_vars = NULL, colour_palette = NULL, fill_palette = NULL, pos = "dodge", barplot_alpha = 1, colour_title = FALSE, fill_title = FALSE, legend_labels = ggplot2::waiver(), legend_rev = FALSE, flip = FALSE, text_col = NULL, basesize = NULL, legend_col = NULL, legend_pos = NULL, keysize = NULL){
  #' Draws barplots.
  #' 
  #' @param data_frame: A data frame
  #'
  #' @param x_var: A factor
  #' @param y_var: A numerical variable
  #'
  #' @param main_title: A string, optional main title of the barplot
  #' @param bar_width: A floating number, optional bar width
  #'   
  #' @param x_title: A string, optional title of the x-axis
  #' @param x_limits: A vector of length 2, optional scale limits, e.g., c(4,10)
  #' @param x_breaks: A vector, optional scale breaks, e.g., c(4, 6, 9)
  #' @param x_ticks: Logical, FALSE switches off tick marks on the x-axis
  #'   
  #' @param y_title: A string, optional title of the y-axis
  #' @param y_limits: A vector of length 2, optional scale limits, e.g., c(0, 1)
  #' @param y_breaks: A vector, optional breaks, e.g., c(0.0, 0.5, 1.0)
  #'   
  #' @param y_facet: A formular, an optional grid for the y-variable controlled
  #'   by y_facet e.g., "SCHOOL ~ ." to have the y-variable sliced horizontally 
  #'   by a school name variable, or ". ~ SCHOOL" to have the y-variable sliced 
  #'   vertically by a school name variable. Defaults to FALSE.
  #' @param label_vars: An optional label for the bars , e.g, "percent(FREQ)"
  #' @param percentage: logical, if TRUE assumes that y_var is a percentage
  #'
  #' @param fill_vars: A factor, an optional variable by which to slice the
  #'   x-variable, used for filling the bars
  #' @param fill_palette: A vector of length equal to the number of categories
  #'   of fill_vars, an optional colour palette, defaults to fill_brewer
  #' @param colour_vars: A factor, an optional variable by which to slice the
  #'   x-variable, usually equal to fill_vars, used for colouring the outside
  #'   edge of the bars
  #' @param colour_palette: A vector of length equal to the number of categories
  #'   of colour_vars, an optional colour palette, usually equal to fill_palette,
  #'   but can be used to highlight the bar borders, defaults to colour_brewer
  #'   
  #' @param pos: A position, an optional placement rule for the bars, defaults to "dodge"
  #' @param barplot_alpha: A floating number between 0 and 1, an optional degree of transparency for the bars, default at 1
  #'   
  #' @param colour_title: legend guide for scale_colour, setting a title 
  #'   activates the legend, requires colour_vars to be set, defaults to FALSE.
  #' @param fill_title: legend guide for scale_fill, setting a title activates 
  #'   the legend, requires fill_vars to be set, default at FALSE.
  #'
  #' @param legend_labels: a vector of labels for the legend, e.g, 
  #'   c("M\\u0101ori", "Pakeh\\u0101")
  #' @param legend_rev: determines whether the order of the legend labels should
  #'   be reversed
  #' @param flip: Flips x-axis and y-axis if TRUE, defaults to FALSE
  #'   
  #' @return a bar plot object
  #' @examples
  #' 
  #' library(tidyverse)
  #' iris.plot <- iris %>% group_by(Species) %>% summarize(Sepal.Length=mean(Sepal.Length))
  #'
  #'   #Simple bar plot
  #'   p <- plot_barplot(iris.plot,
  #'                     x_var = "Species",
  #'                     y_var = "Sepal.Length")
  #'   plot(p)
  #' 
  #'   #Same bar plot with fill by Species and border colour by Species reversed
  #'   p <- plot_barplot(iris.plot,
  #'                     main_title="Sepal.Length by Species on the Iris Data Set",
  #'                     x_var = "Species",
  #'                     y_var = "Sepal.Length",
  #'                     fill_title = "Species",
  #'                     fill_vars = "Species",
  #'                     colour_vars = "rev(Species)")
  #'   plot(p)
  #' 
  #'   #Same bar plot with a facet for Sepal.Width and a legend with a title
  #'   p <- plot_barplot(iris,
  #'                     x_var = "Species",
  #'                     y_var = "Sepal.Length",
  #'                     fill_vars = "Species",
  #'                     y_facet = "Sepal.Width ~ .",
  #'                     fill_title = "Species",
  #'                     flip = TRUE)
  #'   plot(p)
  #' 
  #' @section Bugs: Guide colour and guide fill currently take the same 
  #'   arguments and can not be controlled individually.
  #'
  #' @export

  set_bar_theme()

  p <- ggplot2::ggplot(data = data_frame)

  p <- p + ggplot2::aes_string(x = x_var,
                               y = y_var,
                               fill = fill_vars,
                               colour = colour_vars,
                               label = label_vars)

  p <- p + ggplot2::geom_bar(position=pos,
                             alpha = barplot_alpha,
                             stat = "identity",
                             width = bar_width,
                             size=1) ## controls border thickness

  p <- p + ggplot2::scale_x_discrete(x_title,
                                     drop = FALSE,
                                     limits = rev(x_limits),
                                     breaks = x_breaks)


  ## percentage for y scale
  if(percentage == TRUE) {
    p <- p + ggplot2::scale_y_continuous(y_title,
                                         labels = scales::percent,
                                         limits = y_limits,
                                         breaks = y_breaks)
  } else {
    p <- p + ggplot2::scale_y_continuous(y_title,
                                         limits = y_limits,
                                         breaks = y_breaks)
  }

  ## colour either manual or brewer
  if(!(is.null(colour_palette)) & !(is.null(fill_palette))){
    p <- p + ggplot2::scale_colour_manual(values = colour_palette,
                                          labels = legend_labels)
    p <- p + ggplot2::scale_fill_manual(values = fill_palette,
                                        labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_colour_brewer(labels = legend_labels,
                                          direction = -1)
    p <- p + ggplot2::scale_fill_brewer(labels = legend_labels,
                                        direction = -1)
  }

  ## facet
  if(!(y_facet == FALSE)) {
    p <- p + ggplot2::facet_grid(y_facet, drop = TRUE, scales="free_x")
  }

  ## legend
  p <- p + ggplot2::guides(colour = "none",
                           fill = "none")

  if(!(colour_title == FALSE)) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = colour_title, reverse = legend_rev))
	}

  if(!(fill_title == FALSE)){
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = fill_title, reverse = legend_rev))
  }

  ## flip axes
  if(flip) {
    p <- p + ggplot2::coord_flip()
  }

  ## bar labels
  if(!(label_vars == FALSE)){
    p <- p + ggplot2::geom_label(ggplot2::aes_string(y = paste(y_var, " - 0.00"), fill = rev(fill_vars)),
                                 colour = ggplot2::theme_get()[["bar.label.color"]],
                                 size = ggplot2::theme_get()[["bar.label.size"]],
                                 show.legend = FALSE,
                                 position = pos)
  }

  ## x_ticks switches off labels and ticks on the x-axis
  if(!x_ticks){
    p <- p + ggplot2::theme(axis.text.x=element_blank(),
                            axis.ticks.x=element_blank())
  }


  ## title
  if(!is.null(main_title)){
    cow <- cowplot::ggdraw()
    cow <- cow + cowplot::draw_label(main_title, size=ggplot2::theme_get()[["main.title.size"]])
    p <- cowplot::plot_grid(cow, p, ncol=1, rel_heights=c(0.1, 1))
  }

  return(p)
}

