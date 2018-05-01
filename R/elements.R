#' Add a bar element
bar <- function(p){
  p <- p + ggplot2::geom_bar(
                      position = pos,
                      alpha = barplot_alpha,
                      stat = "identity",
                      width = bar_width,
                      size = 1
                    ) ## controls border thickness
  return(p)
}

#' Add a line element
line <- function(p, pos = ggplot2::position_dodge(width=0.1)){
  p <- p + ggplot2::geom_line(
                      size = 2,
                      position = pos,
                      stat = "identity"
                    )

  p <- p + ggplot2::geom_point(
                      size = 4,
                      position = pos,
                      stat = "identity"
                    )
  return(p)
}

#' Add scale elements
scales <- function(p){
  p <- p + ggplot2::scale_x_discrete(
                      x_title,
                      drop = FALSE,
                      limits = rev(x_limits),
                      breaks = x_breaks
                    )


  if (percentage == TRUE) {
    p <- p + ggplot2::scale_y_continuous(
                        y_title,
                        labels = scales::percent,
                        limits = y_limits,
                        breaks = y_breaks
                      )
  } else {
    p <- p + ggplot2::scale_y_continuous(
                        y_title,
                        limits = y_limits,
                        breaks = y_breaks)
  }
  return(p)
}


#' Add colour
colour <- function(p){
  if (!(is.null(colour_palette)) & !(is.null(fill_palette))) {
    p <- p + ggplot2::scale_colour_manual(
                        values = colour_palette,
                        labels = legend_labels)
    p <- p + ggplot2::scale_fill_manual(
                        values = fill_palette,
                        labels = legend_labels)
  }
  else {
    p <- p + ggplot2::scale_colour_brewer(
                        labels = legend_labels,
                        direction = -1)
    p <- p + ggplot2::scale_fill_brewer(
                        labels = legend_labels,
                        direction = -1)
  }
  return(p)
}


#' Add facet
facet <- function(p){
  if (!(y_facet == FALSE)) {
    p <-
      p + ggplot2::facet_grid(y_facet, drop = TRUE, scales = "free_x")
  }
  return(p)
}


#' Add legend
legend <- function(p){
  p <- p + ggplot2::guides(colour = "none",
                           fill = "none")

  if (!(colour_title == FALSE)) {
    p <-
      p + ggplot2::guides(colour = ggplot2::guide_legend(
                                              title = colour_title,
                                              reverse = legend_rev))
  }

  if (!(fill_title == FALSE)) {
    p <-
      p + ggplot2::guides(fill = ggplot2::guide_legend(
                                            title = fill_title,
                                            reverse = legend_rev))
  }
  return(p)
}

#' Add axis flip
flip_axes <- function(p){
  if (flip) {
    p <- p + ggplot2::coord_flip()
  }
  return(p)
}


#' Add bar labels
bar_labels <- function(p){
  if (!(label_vars == FALSE)) {
    p <-
      p + ggplot2::geom_label(
                     ggplot2::aes_string(y = paste(y_var, " - 0.00"), fill = rev(fill_vars)),
                     colour = get_bar_theme()[["bar.label.color"]],
                     size = get_bar_theme()[["bar.label.size"]],
                     show.legend = FALSE,
                     position = pos
                   )
  }
  return(p)
}


#' Add x-ticks
x_ticks <- function(p) {
  if (!x_ticks) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())
  }
  return(p)
}


#' Add title
title <- function(p){
  if (!is.null(main_title)) {
    title <- cowplot::ggdraw() +
      cowplot::draw_label(main_title, size = get_bar_theme()[["main.title.size"]])
    p <- cowplot::plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
  }
  return(p)
}
