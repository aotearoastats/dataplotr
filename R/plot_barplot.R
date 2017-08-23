plot_barplot <- function(data_frame,
                         main_title = NULL,
                         bar_width = NULL,
                         x_title = ggplot2::waiver(),
                         y_title = ggplot2::waiver(),
                         x_var,
                         y_var,
                         x_limits = NULL,
                         y_limits = NULL,
                         x_breaks = ggplot2::waiver(),
                         y_breaks = ggplot2::waiver(),
                         y_facet = FALSE,
                         label_vars = FALSE,
                         percentage = FALSE,
                         color_vars = NULL, fill_vars = NULL,
                         colour_palette = NULL, fill_palette = NULL,
                         pos = "dodge",
                         barplot_alpha = 1,
                         colour_title = FALSE,
                         fill_title = FALSE,
                         legend_labels = ggplot2::waiver(),
                         legend_pos = c(1,1),
                         legend_rev = FALSE,
                         legend_col = "#E5E2E0",
                         keysize = 0.7,
                         text_col = black_seq[8],
                         basesize = 12,
                         flip = FALSE){
  #' Draws barplots.
  #'
  #' @param data_frame: A data frame
  #' @param main_title: Title of the barplot, defaults to NULL.
  #' @param bin_width: Width of the bars, defaults to NULL.
  #' @param text_col: Text colour, defaults to black_seq[8]
  #' @param basesize: Text basesize, defaults to 12
  #' @param flip: Flips x-axis and y-axis if TRUE, defaults to FALSE
  #'
  #' @param x_title: X-axis title, string in quotations, e.g., "Year Level"
  #' @param x_var: X-axis variable, has to be a factor
  #' @param x_limits: Scale limits, e.g., c(4,10) for a year level variable
  #' @param x_breaks: Breaks, e.g., c(4, 6, 9) for a year level variable
  #'
  #' @param y_title: Y-axis title, string in quotations, e.g., "Percentage of 
  #'   Blocks"
  #' @param y_var: Y-axis variable
  #' @param y_limits: Scale limits, e.g., c(0, 1) for a percentage variable
  #' @param y_breaks: Breaks, e.g., c(0, 0.5, 1) for a percentage variable
  #'
  #' @param y_facet: A grid for the y-variable controlled via a formula in 
  #'   quotations e.g., "SCHOOL ~ ." to have the y-variable sliced horizontally 
  #'   by a school name variable, or ". ~ SCHOOL" to have the y-variable sliced 
  #'   vertically by a school name variable. Defaults to FALSE.
  #' @param label_vars: A variable for bar labels, e.g, 
  #'   label_vars="percent(FREQ)"
  #' @param percentage: whether the quantitative variable should have percentage
  #'   labels
  #'   
  #' @param color_vars: variable by which to cross the x-variable, used for 
  #'   colouring the bars, has to be factor
  #' @param fill_vars: variable by which to cross the x-variable, used for 
  #'   filling the bars, has to be factor
  #' @param colour_palette: palette values for scale_colour, defaults to 
  #'   colour_brewer
  #' @param fill_palette: palette values for scale_fill, defaults to fill_brewer
  #'   
  #' @param pos: position of boxplot, default at "dodge"
  #' @param barplot_alpha: degree of transparency of barplot, default at 1
  #'   
  #'   
  #'   legend arguments
  #' @param colour_title: legend guide for scale_colour, setting a title 
  #'   activates the legend, defaults to FALSE.
  #' @param fill_title: legend guide for scale_fill, setting a title activates 
  #'   the legend, default at FALSE.
  #' @param legend_labels: a vector of labels for the legend, e.g, 
  #'   c("M\\u0101ori", "Pakeh\\u0101")
  #' @param legend_pos: legend positions, takes a string ("top", "bottom", 
  #'   "right", "left"), or a vector of two numbers between 0 and 1, indicating 
  #'   the relative x and y positions.
  #' @param legend_rev: determines whether the order of the legend labels should
  #'   be reversed
  #' @param legend_col: legend background colour, default at "#E5E2E0" keysize: 
  #'   size of the legend
  #'   
  #' @return a bar plot object
  #' @examples
  #' 
  #'   #Simple bar plot
  #'   p <- plot_barplot(iris,
  #'                     x_var = "Species",
  #'                     y_var = "Sepal.Length")
  #'   plot(p)
  #' 
  #'   #Same bar plot with colour by Species
  #'   p <- plot_barplot(iris,
  #'                     x_var = "Species",
  #'                     y_var = "Sepal.Length",
  #'                     fill_title = "Species",
  #'                     fill_vars = "Species",
  #'                     color_vars = "Species")
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
  #'   Labels don't look good with position = "stack"
  #' @export

  p <- ggplot2::ggplot(data = data_frame)

  p <- p + ggplot2::aes_string(x = x_var,
                               y = y_var,
                               fill = fill_vars,
                               colour = color_vars,
                               label = label_vars)

  p <- p + ggplot2::geom_bar(position=pos,
                             alpha = barplot_alpha,
                             stat = "identity",
                             width = bar_width)

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
  if(exists(colour_palette) & exists(fill_palette)){
    p <- p + scale_colour_manual(values = colour_palette,
                                 labels = legend_labels)
    p <- p + scale_fill_manual(values = fill_palette,
                               labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_colour_brewer(labels = legend_labels,
                                          direction = -1)
    p <- p + ggplot2::scale_fill_brewer(labels = legend_labels,
                                        direction = -1)
  }

  ## facet
  if(!(y_facet == FALSE)) {
    p <- p + ggplot2::facet_grid(y_facet, drop = FALSE)
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

  ## ## bar labels
  if(!(label_vars == FALSE)){
    p <- p + ggplot2::geom_label(ggplot2::aes_string(y = paste(y_var, " - 0.00"), fill = rev(fill_vars)),
                                 colour = text_col,
                                 show.legend = FALSE,
                                 position = pos,
                                 size = basesize - 9)
  }

  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = basesize - 2, colour = text_col),
                          legend.title = ggplot2::element_text(size = basesize, colour = text_col),
                          legend.key.size = ggplot2::unit(keysize, "cm"),
                          legend.key = ggplot2::element_blank(),
                          legend.background = ggplot2::element_rect(colour = legend_col),
                          ## legend.justification = c(1, 1),
                          legend.position = legend_pos,
                          axis.text.x = ggplot2::element_text(size = basesize, colour = text_col, lineheight = 1.2, vjust = 1),
                          axis.text.y = ggplot2::element_text(size = basesize-2, colour = text_col, lineheight = 1.2, vjust = 0.5),
                          axis.title.x = ggplot2::element_text(vjust = 3.1804, colour = text_col, size = basesize + 2, margin=ggplot2::margin(25,0,0,0)),
                          axis.title.y = ggplot2::element_text(vjust = 0.6316, colour = text_col, size = basesize + 2),
                          axis.ticks.length = ggplot2::unit(.20888, "cm"),
                          plot.title = ggplot2::element_text(vjust = 3.8746, hjust = 0, colour = text_col, size = basesize + 8),
                          plot.margin = ggplot2::unit(c(.8835, .582083, 0.0748, .9378), "cm"),
                          strip.background = ggplot2::element_rect(fill = "#ececec", colour = black_seq[8]), # facet background
                          strip.text.y = ggplot2::element_text(size = 8, angle = 0)) # for facet text

  ## title
  if(exists("main_title")){
    ## p <- p + ggplot2::ggtitle(main_title)
    title <- cowplot::ggdraw() + cowplot::draw_label(main_title) ##, fontface='bold')
    p <- cowplot::plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
  }
  return(p)
}
