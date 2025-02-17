#' Plot genealogical tree with Graphviz
#'
#' Plots a genealogical tree either interactively or to PDF using the Graphviz layout engine.
#'
#' This function requires the `DiagrammeR`, `DiagrammeRsvg` and either `svgPanZoom` (interactive) or `rsvg` (pdf output) packages to be installed.
#' They are only "Suggests" dependencies as this package supports multiple plotting options.
#' The presence of these packages will be verified when the function is actually called, providing an opportunity to install them automatically if needed.
#'
#' @references
#' Ellson, J., Gansner, E.R., Koutsofios, E., North, S.C. and Woodhull, G. (2004). “Graphviz and Dynagraph — Static and Dynamic Graph Drawing Tools”. In: Jünger, M., Mutzel, P. (eds) _Graph Drawing Software, Mathematics and Visualization_, 127-148. [10.1007/978-3-642-18638-7_6](https://doi.org/10.1007/978-3-642-18638-7_6).
#'
#' Iannone, R. and Roy, O. (2024). _DiagrammeR: Graph/Network Visualization_. R package, <https://CRAN.R-project.org/package=DiagrammeR>.
#'
#' Iannone, R. (2016). _DiagrammeRsvg: Export DiagrammeR Graphviz Graphs as SVG_. R package, <https://CRAN.R-project.org/package=DiagrammeRsvg>.
#'
#' Ooms, J. (2024). _rsvg: Render SVG Images into PDF, PNG, (Encapsulated) PostScript, or Bitmap Arrays_. R package, <https://CRAN.R-project.org/package=rsvg>.
#'
#' Riutta, A., Tangelder, J., Russell, K., et al. (2020). _svgPanZoom: R 'Htmlwidget' to Add Pan and Zoom to Almost any R Graphic_. R package, <https://CRAN.R-project.org/package=svgPanZoom>.
#'
#' @param g
#'        an object of class `genealogy`, as returned by [get_genealogy()].
#' @param file
#'        an optional file name.
#'        If the file name is specified, then Graphviz will render the genealogical tree to PDF and save in this file.
#'        If the file name is not specified, then the plot will be rendered interactively in the RStudio Viewer panel.
#'
#' @return
#' If a filename was specified, the full path of the saved file is returned as a `character(1)` string.
#' If no filename was specified, then an `htmlwidget` suitable for display in the RStudio Viewer is returned.
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
#' # Then use the plot_grviz() function to produce a full genealogical tree
#' plot_grviz(g)
plot_grviz <- function(g, file = "") {
  # Check inputs and for required packages
  check_genealogy(g)
  check_str(file, 0L)

  # What mode are we operating in?
  if (nchar(file) == 0L) {
    # interactive plot
    rlang::check_installed(c("DiagrammeR", "DiagrammeRsvg", "svgPanZoom"), reason = "in order to produce Graphviz plots in RStudio Viewer.")
  } else {
    # write to pdf
    rlang::check_installed(c("DiagrammeR", "DiagrammeRsvg", "rsvg"), reason = "in order to write Graphviz plots to pdf.")
  }

  # Make the graph
  graph <- get_edges_nodes(g)

  dot <- r"(
digraph {
  graph [ordering="out"];
  node [shape=plaintext];
  edge [style=bold];

)"

  dot <- paste0(dot, paste0("  ", names(graph[["nodes"]]), " [label=\"", gsub('"', '\\"', gsub("'", "\u2019", graph[["nodes"]], fixed = TRUE), fixed = TRUE), "\"];\n", collapse = ""), "\n")
  dot <- paste0(dot, paste0("  ", graph[["edges"]][["from"]], " -> ", graph[["edges"]][["to"]], ";\n", collapse = ""))
  dot <- paste0(dot, "}\n")

  grv <- DiagrammeR::grViz(dot)
  svg <- DiagrammeRsvg::export_svg(grv)

  # Either output to viewer or write to file
  if (nchar(file) == 0L) {
    # interactive plot
    res <- svgPanZoom::svgPanZoom(svg, minZoom = 1.0, maxZoom = 20.0, viewBox = FALSE, width = "100%", height = "92vh")
  } else {
    # write to pdf
    rlang::try_fetch(
      {
        rsvg::rsvg_pdf(charToRaw(svg), file = file)
      },
      error = function(e) {
        cli::cli_abort(c(x = "Unable to write Graphviz tree to PDF file"),
                       parent = e)
      }
    )
    res <- normalizePath(file)
  }
  return(res)
}
