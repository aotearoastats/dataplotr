theme_env <- new.env(parent=emptyenv())

#' Sets a theme for a barplot
#'
#' @param text_col Text colour, defaults to black_seq[8]
#' @param basesize Text basesize, defaults to 12
#' @param legend_col legend background colour, default at "#E5E2E0"
#' @param legend_pos legend positions, takes a string ("top", "bottom", 
#'   "right", "left"), or a vector of two numbers between 0 and 1, indicating 
#'   the relative x and y positions.
#' @param keysize size of the legend
#' @export
set_bar_theme <-
  function(text_col="#000000",
           basesize=12,
           legend_col="#E5E2E0",
           legend_pos="bottom",
           keysize=0.7){

    bar_theme <- ggplot2::theme_bw()

    ## R colour codes
    ## http://colorbrewer2.org/
    bar_theme[["black_seq"]] <-
      list('#ffffff', '#f0f0f0', '#d9d9d9',
           '#bdbdbd', '#969696', '#737373',
           '#525252', '#252525', '#000000')

    bar_theme[["blue_seq"]] <-
      list('#f7fbff', '#deebf7', '#c6dbef',
           '#9ecae1', '#6baed6', '#4292c6',
           '#2171b5', '#08519c', '#08306b')

    bar_theme[["green_seq"]] <-
      list('#f7fcf5', '#e5f5e0', '#c7e9c0',
           '#a1d99b', '#74c476', '#41ab5d',
           '#238b45', '#006d2c', '#00441b')

    bar_theme[["orange_seq"]] <-
      list('#fff5eb', '#fee6ce', '#fdd0a2',
           '#fdae6b', '#fd8d3c', '#f16913',
           '#d94801','#a63603','#7f2704')

    bar_theme[["purple_seq"]] <-
      list('#fcfbfd', '#efedf5', '#dadaeb',
           '#bcbddc', '#9e9ac8', '#807dba',
           '#6a51a3', '#54278f', '#3f007d')

    bar_theme[["red_seq"]] <-
      list('#fff5f0', '#fee0d2', '#fcbba1',
           '#fc9272', '#fb6a4a', '#ef3b2c',
           '#cb181d', '#a50f15', '#67000d')

    bar_theme <-
      bar_theme + ggplot2::theme(
                 ## legent_settings
                 legend.text=ggplot2::element_text(
                                        size=basesize - 2,
                                        colour=text_col),
                 legend.title=ggplot2::element_text(
                                         size=basesize,
                                         colour=text_col),
                 legend.key.size=ggplot2::unit(keysize, "cm"),
                 legend.key=ggplot2::element_blank(),
                 legend.background=ggplot2::element_rect(
                                              colour=legend_col),
                 legend.position=legend_pos,
                 ## axis settings
                 axis.title.x=ggplot2::element_text(
                                         vjust=3.1804,
                                         colour=text_col,
                                         size=basesize + 2,
                                         margin=ggplot2::margin(25,0,0,0)),
                 axis.text.x=ggplot2::element_text(
                                        size=basesize,
                                        lineheight=1.2,
                                        vjust=1),
                 axis.title.y=ggplot2::element_text(
                                         vjust=0.6316,
                                         colour=text_col,
                                         size=basesize + 2),
                 axis.text.y=ggplot2::element_text(
                                        size=basesize-2,
                                        colour=text_col,
                                        lineheight=1.2,
                                        vjust=0.5),
                 axis.ticks.length=ggplot2::unit(.10888, "cm"),


                 plot.margin=ggplot2::unit(c(.8835, 0, 0.0748, 0), "cm"),
                 panel.border=ggplot2::element_rect(
                                         color="grey",
                                         fill=NA,
                                         size=1), ## for facet borders
                 strip.background=ggplot2::element_rect(
                                             fill="#ececec",
                                             colour=bar_theme[["black_seq"]][[8]]), # facet background
                 strip.text.x=ggplot2::element_text(
                                         size=basesize), # for facet text
                 strip.text.y=ggplot2::element_text(
                                         size=basesize,
                                         angle=0)) # for facet text

    bar_theme["main.title.size"] <- basesize+5
    bar_theme["bar.label.size"] <- basesize-9
    bar_theme["bar.label.color"] <- text_col

    theme_env$bar_theme <- bar_theme
    ggplot2::theme_set(theme_env$bar_theme)
  }

get_bar_theme <- function(){
  return(theme_env$bar_theme)
}
