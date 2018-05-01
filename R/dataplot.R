#' base function to build build up graphs element by element
dataplot <-
  function(data_frame,
           x_var,
           y_var,
           element_function,
           main_title=NULL,
           bar_width = NULL,
           x_title = ggplot2::waiver(),
           x_ticks = TRUE,
           y_title = ggplot2::waiver(),
           x_limits = NULL,
           y_limits = NULL,
           x_breaks = ggplot2::waiver(),
           y_breaks = ggplot2::waiver(),
           y_facet = FALSE,
           label_vars = FALSE,
           percentage = FALSE,
           colour_vars = FALSE,
           fill_vars = FALSE,
           colour_palette = NULL,
           fill_palette = NULL,
           pos = "dodge",
           barplot_alpha = 1,
           colour_title = FALSE,
           fill_title = FALSE,
           legend_labels = ggplot2::waiver(),
           legend_rev = FALSE,
           flip = FALSE){

    ## comment 30/4/18 AK
    ## quo_name(quo(NULL)) currently fails, see https://github.com/r-lib/rlang/issues/430
    ## all variables above that have a default of NULL could cause the following error:
    ## #> Error: `expr` must quote a symbol, scalar, or call
    ## set to FALSE instead

    p <- ggplot2::ggplot(data = data_frame)

    p <- p + ggplot2::aes_string(x = x_var,
                                 y = y_var,
                                 fill = fill_vars,
                                 colour = colour_vars,
                                 label = label_vars
                                 )

    e <- environment()
    for(f in element_function) {
      environment(f) <- e
      p <- f(p)
    }

    return(p)
  }
