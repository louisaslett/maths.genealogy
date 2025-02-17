#' Plot genealogical tree with `ggenealogy`
#'
#' Plots a genealogical tree using the `ggenealogy` layout engine.
#'
#' This function requires the `ggenealogy` package to be installed.
#' It is only a "Suggests" dependency because this package supports multiple plotting approaches.
#' The presence of this package will be verified when the function is actually called, providing an opportunity to install automatically if needed.
#'
#' This function is not suitable for plotting very large whole genealogical trees.
#' Consider using [plot_grviz()] if you want to see an entire tree.
#'
#' @references
#' Rutter, L., VanderPlas, S., Cook, D. and Graham, M.A. (2019). “ggenealogy: An R Package for Visualizing Genealogical Data”, _Journal of Statistical Software_, **89**(13), 1-31. [doi:10.18637/jss.v089.i13](https://doi.org/10.18637/jss.v089.i13).
#'
#' Wickham, H. (2016). _ggplot2: Elegant Graphics for Data Analysis_. Springer-Verlag New York.
#'
#' @param g
#'        an object of class `genealogy`, as returned by [get_genealogy()].
#' @param max_anc
#'        an `integer(1)` with the maximum number of generations of ancestors to be displayed.
#' @param max_des
#'        an `integer(1)` with the maximum number of generations of descendants to be displayed.
#' @param id
#'        an `integer(1)` or `character(1)` with the mathematician ID to highlight and centre the tree on.
#'        By default this is `NULL` which will use the first ID that was supplied to [get_genealogy()] when retrieving the genealogical tree.
#'        Note that the ID must be one of the IDs searched when calling [get_genealogy()] to construct `g`, since the search for ancestors/descendants only goes directly up/down branches reachable from the initial search ID.
#' @param col
#'        a `character(1)` specifying the colour to highlight the mathematician one whom the graph is centred.
#' @param expand
#'        a `numeric(1)` with the expansion factor for the graph.
#'        This defaults to `0.15`, with larger values causing the `x` axis to expand, smaller values for it to shrink.
#'        This is useful if the nearest common ancestor has a long name, which may cause it to be clipped when plotting: increase this expansion factor to rectify this.
#'
#' @return
#' An object of class `("gg", "ggplot")` which can be displayed, or further manipulated using additional layers or aesthetic modifications from the [`ggplot2`][ggplot2::ggplot2] package.
#'
#' @export
#'
#' @examples
#' # First, you need to use search_id() to find the mathematician ID for the
#' # individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
#' # up in the browser.
#'
#' # For example, the package author would get their own tree using
#' g <- get_genealogy(171971)
#'
#' # Then use the plot_gg() function to use the underlying ggenealogy package
#' plot_gg(g)
plot_gg <- function(g, max_anc = 3L, max_des = 3L, id = NULL, col = "red", expand = 0.15) {
  # Check inputs and for required packages
  check_genealogy(g)

  err <- checkmate::check_integerish(max_anc, lower = 0L, any.missing = FALSE, len = 1L)
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg max_anc} argument: {err}"))
  }
  err <- checkmate::check_integerish(max_des, lower = 0L, any.missing = FALSE, len = 1L)
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg max_des} argument: {err}"))
  }

  check_str(col, 1L)

  # id
  if (!is.null(id)) {
    check_vec(id)
    if (is.character(id)) {
      id <- as.integer(id)
    }
    err <- checkmate::check_integerish(id, lower = 1L, any.missing = FALSE, len = 1L)
    if (!identical(err, TRUE)) {
      cli::cli_abort(c(x = "{.arg id} argument: {err}"))
    }
    if (!checkmate::test_choice(id, attr(g, "start_nodes", TRUE))) {
      cli::cli_abort(c(x = "{.arg id} must be one of the search ids used when {.fun get_genealogy} was called."))
    }
  } else {
    id <- attr(g, "start_nodes", TRUE)[[1L]]
  }

  rlang::check_installed("ggenealogy", reason = "in order to produce ggenealogy plots.")

  pc <- get_parent_child(g, g[[as.character(id)]][["name"]], max_anc, max_des)

  # lintr "no visible binding" fixes
  x <- y <- xend <- yend <- NULL

  # Construct plot with ggenealogy
  original_plot <- ggenealogy::plotAncDes(g[[as.character(id)]][["name"]], pc, mAnc = max_anc, mDes = max_des, vColor = col)
  # Now modify this to work better visually for these kinds of ancestries
  # Want to make lines grey so the text is easier to read, so overplot original
  # lines and then place original text layers back on top.
  # In addition, these read better vertically, so flip coordinates and reverse
  # axis so that older generations at the top
  original_plot +
    ggplot2::geom_segment(data = ggplot2::ggplot_build(original_plot)[["data"]][[2L]], # Fetch the segment data
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          colour = "grey", size = 1.2, inherit.aes = FALSE) +
    ggplot2::geom_segment(data = ggplot2::ggplot_build(original_plot)[["data"]][[3L]], # Fetch the segment data
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          colour = "grey", size = 1.2, inherit.aes = FALSE) +
    original_plot[["layers"]][-(2L:3L)] +
    ggplot2::coord_flip() +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(expand))
}
