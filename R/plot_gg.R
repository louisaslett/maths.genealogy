#' Plot genealogical tree with `ggenealogy`
#'
#' Plots a genealogical tree using the `ggenealogy` layout engine.
#'
#' This function requires the `ggenealogy` package to be installed.
#' It is only a "Suggests" dependency because this package supports multiple plotting approaches.
#' The presence of this package will be verified when the function is actually called, providing an opportunity to install automatically if needed.
#'
#' If the input tree `g` is large, then `ggenealogy` can take some time to process it, even if `max_anc`/`max_des` are small.
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
#'
#' @return
#' An object of class `("gg", "ggplot")` which can be displayed, or further manipulated using additional layers or aesthetic modifications from the [`ggplot2`][ggplot2::ggplot2] package.
#'
#' @export
#'
#' @examples
#' # TODO
plot_gg <- function(g, max_anc = 3L, max_des = 3L, id = NULL, col = "red") {
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

  pc <- get_parent_child(g)

  ggenealogy::plotAncDes(g[[as.character(id)]][["name"]], pc, mAnc = max_anc, mDes = max_des, vColor = col)
}
