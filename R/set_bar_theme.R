set_bar_theme <- function(text_col = black_seq[8], basesize = 12, legend_col = "#E5E2E0", legend_pos = "bottom", keysize = 0.7) {
  #' Sets a theme for a barplot
  #' @param text_col Text colour, defaults to black_seq[8]
  #' @param basesize Text basesize, defaults to 12
  #' @param legend_col legend background colour, default at "#E5E2E0"
  #' @param legend_pos legend positions, takes a string ("top", "bottom", 
  #'   "right", "left"), or a vector of two numbers between 0 and 1, indicating 
  #'   the relative x and y positions.
  #' @param keysize size of the legend
  #' @export
  bar_theme <- ggplot2::theme_bw() +
    ggplot2::theme(legend.text = ggplot2::element_text(size = basesize - 2, colour = text_col),
                   legend.title = ggplot2::element_text(size = basesize, colour = text_col),
                   legend.key.size = ggplot2::unit(keysize, "cm"),
                   legend.key = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(colour = legend_col),
                   legend.position = legend_pos,
                   axis.title.x = ggplot2::element_text(vjust = 3.1804, colour = text_col, size = basesize + 2, margin=ggplot2::margin(25,0,0,0)),
                   axis.text.x = ggplot2::element_text(size = basesize, lineheight = 1.2, vjust = 1),
                   axis.title.y = ggplot2::element_text(vjust = 0.6316, colour = text_col, size = basesize + 2),
                   axis.text.y = ggplot2::element_text(size = basesize-2, colour = text_col, lineheight = 1.2, vjust = 0.5),
                   axis.ticks.length = ggplot2::unit(.10888, "cm"),
                   ## plot.title = ggplot2::element_text(vjust = 3.8746, hjust = 0, colour = text_col, size = basesize + 8),
                   plot.margin = ggplot2::unit(c(.8835, 0, 0.0748, 0), "cm"),
                   panel.border = ggplot2::element_rect(color = "grey", fill = NA, size = 1), ## for facet borders
                   strip.background = ggplot2::element_rect(fill = "#ececec", colour = black_seq[8]), # facet background
                   strip.text.x = ggplot2::element_text(size = basesize), # for facet text
                   strip.text.y = ggplot2::element_text(size = basesize, angle = 0)) # for facet text

  bar_theme["main.title.size"] <- basesize+5
  bar_theme["bar.label.size"] <- basesize-9
  bar_theme["bar.label.color"] <- text_col

  ggplot2::theme_set(bar_theme)
}
