# Lineage tree only (ie walk upwards, full downwards)
get_parent_child <- function(g) {
  # "no visible binding" fixes
  from <- to <- NULL

  en <- get_edges_nodes(g)

  data.frame(to = as.integer(names(en[["nodes"]])),
             child = vapply(g[names(en[["nodes"]])], \(x) x[["name"]], "Louis", USE.NAMES = FALSE),
             year = vapply(g[names(en[["nodes"]])], \(x) ifelse(is.null(x[["year"]]), NA, x[["year"]]), 1980L, USE.NAMES = FALSE)) |>
    merge(en[["edges"]], by = "to", all.x = TRUE) |>
    transform(parent = vapply(g[as.character(from)], \(x) ifelse(is.null(x[["name"]]), "", x[["name"]]), "Louis", USE.NAMES = FALSE)) |>
    subset(select = c(-to, -from))
}
