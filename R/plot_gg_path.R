#' Plot shortest path in genealogical tree with `ggenealogy`
#'
#' Plots a shortest path between two mathematicians in a genealogical tree using the `ggenealogy` layout engine.
#'
#' This function requires the `ggenealogy` package to be installed.
#' It is only a "Suggests" dependency because this package supports multiple plotting approaches.
#' The presence of this package will be verified when the function is actually called, providing an opportunity to install automatically if needed.
#'
#' The shortest path between the two mathematician IDs provided is plotted, with the `x` position of each label determined by the year of PhD award.
#'
#' **NOTE:** if the name of the nearest common ancestor is long, it can be clipped by `ggplot2`.
#' If this occurs, increase the `expand` argument greater than the default of `0.15`.
#'
#' @references
#' Rutter, L., VanderPlas, S., Cook, D. and Graham, M.A. (2019). “ggenealogy: An R Package for Visualizing Genealogical Data.” _Journal of Statistical Software_, **89**(13), 1-31. [doi:10.18637/jss.v089.i13](https://doi.org/10.18637/jss.v089.i13).
#'
#' @param g
#'        an object of class `genealogy`, as returned by [get_genealogy()].
#' @param id1
#'        an `integer(1)` or `character(1)` with the ID of the first mathematician of interest.
#' @param id2
#'        an `integer(1)` or `character(1)` with the ID of the second mathematician of interest.
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
#' # TODO
#'
plot_gg_path <- function(g, id1 = NULL, id2 = NULL, expand = 0.15) {
  # Check inputs and for required packages
  check_genealogy(g)

  if (length(attr(g, "start_nodes", TRUE)) < 2L) {
    cli::cli_abort(c(x = "The genealogy in {.arg g} must have been built for at least two mathematician IDs.",
                     i = "Hint: the path can only be plotted between mathematicans that were specified in the call to {.fun get_genealogy}, so you need at least two IDs in that call."))
  }

  err <- checkmate::check_number(expand, finite = TRUE)
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg expand} argument: {err}"))
  }

  # ids
  if (!is.null(id1)) {
    check_vec(id1)
    if (is.character(id1)) {
      id1 <- as.integer(id1)
    }
    err <- checkmate::check_integerish(id1, lower = 1L, any.missing = FALSE, len = 1L)
    if (!identical(err, TRUE)) {
      cli::cli_abort(c(x = "{.arg id1} argument: {err}"))
    }
    if (!checkmate::test_choice(id1, attr(g, "start_nodes", TRUE))) {
      cli::cli_abort(c(x = "{.arg id1} must be one of the search IDs used when {.fun get_genealogy} was called."))
    }
  } else {
    id1 <- attr(g, "start_nodes", TRUE)[[1L]]
  }
  if (!is.null(id2)) {
    check_vec(id2)
    if (is.character(id2)) {
      id2 <- as.integer(id2)
    }
    err <- checkmate::check_integerish(id2, lower = 1L, any.missing = FALSE, len = 1L)
    if (!identical(err, TRUE)) {
      cli::cli_abort(c(x = "{.arg id2} argument: {err}"))
    }
    if (!checkmate::test_choice(id2, attr(g, "start_nodes", TRUE))) {
      cli::cli_abort(c(x = "{.arg id2} must be one of the search IDs used when {.fun get_genealogy} was called."))
    }
    if (id1 == id2) {
      cli::cli_abort(c(x = "{.arg id1} and {.arg id2} must be different IDs!"))
    }
  } else {
    id2 <- attr(g, "start_nodes", TRUE)[[2L]]
  }

  rlang::check_installed("ggenealogy", reason = "in order to produce ggenealogy plots.")

  pc <- get_parent_child(g)

  pc_ig <- ggenealogy::dfToIG(pc)

  path12 <- ggenealogy::getPath(g[[as.character(id1)]][["name"]], g[[as.character(id2)]][["name"]], pc_ig, pc, "year")

  ggenealogy::plotPath(path12, pc, "year") +
    ggplot2::ylab("Year of PhD") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(expand))
}
