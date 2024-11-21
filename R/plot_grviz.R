#' @export
plot_grviz <- function(g) {


  graph <- get_edges_nodes(g)

  dot <- r"(
digraph {
  graph [ordering="out"];
  node [shape=plaintext];
  edge [style=bold];

)"

  dot <- paste0(dot, paste0("  ", names(graph[["nodes"]]), " [label=\"", gsub('"', '\\"', gsub("'", "’", graph[["nodes"]], fixed = TRUE), fixed = TRUE), "\"];\n", collapse = ""), "\n")
  dot <- paste0(dot, paste0("  ", graph[["edges"]][["from"]], " -> ", graph[["edges"]][["to"]], ";\n", collapse = ""))
  dot <- paste0(dot, "}\n")

  grv <- DiagrammeR::grViz(dot)
  svg <- DiagrammeRsvg::export_svg(grv)
  svgPanZoom::svgPanZoom(svg, minZoom = 1.0, maxZoom = 20, width = "100%", height = "92vh")
}
