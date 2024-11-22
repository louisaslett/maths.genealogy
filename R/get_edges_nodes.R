# Lineage tree only (ie walk upwards, full downwards)
get_edges_nodes <- function(g) {
  edges_from <- list()
  edges_to <- list()
  nodes <- vector("character")

  start <- attr(g, "start_nodes", exact = TRUE)
  # Descendants
  queue <- as.character(start)
  seen <- vector("character") # safety valve ... ancestry loops should theoretically be impossible, but ...!
  while (length(queue) > 0L) {
    if (queue[[1L]] %in% seen) {
      queue <- queue[-1L]
      next
    }
    x <- g[[queue[[1L]]]]
    seen <- c(seen, queue[[1L]])
    queue <- queue[-1L]

    nodes <- c(nodes,
               stats::setNames(paste0(x[["name"]],
                                      ifelse(!is.null(x[["institution"]]) | !is.null(x[["year"]]), "\\n", ""),
                                      ifelse(!is.null(x[["institution"]]), paste0(x[["institution"]], " "), ""),
                                      ifelse(!is.null(x[["year"]]), paste0("(", x[["year"]], ")"), "")),
                               x[["id"]]))
    # Check descendants
    # first, even if there are descendants, it does does not mean they are in
    # our query set (depending on search choices), so subset only to those we
    # have data on first
    descendants <- x[["descendants"]][!vapply(g[as.character(x[["descendants"]])], is.null, TRUE, USE.NAMES = FALSE)]
    if (length(descendants) == 0L) {
      next
    }

    queue <- c(queue, as.character(descendants))
    edges_from <- c(edges_from, list(rep(x[["id"]], length(descendants))))
    edges_to <- c(edges_to, list(descendants))
  }

  # Ancestors
  queue <- as.character(start)
  seen <- setdiff(seen, queue)
  while (length(queue) > 0L) {
    if (queue[[1L]] %in% seen) {
      queue <- queue[-1L]
      next
    }
    x <- g[[queue[[1L]]]]
    seen <- c(seen, queue[[1L]])
    queue <- queue[-1L]

    nodes <- c(nodes,
               stats::setNames(paste0(x[["name"]],
                                      ifelse(!is.null(x[["institution"]]) | !is.null(x[["year"]]), "\\n", ""),
                                      ifelse(!is.null(x[["institution"]]), paste0(x[["institution"]], " "), ""),
                                      ifelse(!is.null(x[["year"]]), paste0("(", x[["year"]], ")"), "")),
                               x[["id"]]))
    # Check advisors
    # first, even if there are advisors, it does does not mean they are in
    # our query set (depending on search choices), so subset only to those we
    # have data on first
    advisors <- x[["advisors"]][!vapply(g[as.character(x[["advisors"]])], is.null, TRUE, USE.NAMES = FALSE)]
    if (length(advisors) == 0L) {
      next
    }

    queue <- c(queue, as.character(advisors))
    edges_from <- c(edges_from, list(advisors))
    edges_to <- c(edges_to, list(rep(x[["id"]], length(advisors))))
  }

  return(list(nodes = nodes[!duplicated(nodes)],
              edges = data.frame(from = c(edges_from, recursive = TRUE, use.names = FALSE),
                                 to = c(edges_to, recursive = TRUE, use.names = FALSE))))
}
