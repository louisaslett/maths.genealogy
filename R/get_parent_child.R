# Lineage tree only (ie walk upwards, full downwards)
get_parent_child <- function(g, start_node = NULL, max_anc = Inf, max_des = Inf) {
  # "no visible binding" fixes
  from <- to <- NULL

  en <- get_edges_nodes(g)

  # Construct data frame
  pc <- data.frame(to = as.integer(names(en[["nodes"]])),
                   child = vapply(g[names(en[["nodes"]])], \(x) x[["name"]], "Louis", USE.NAMES = FALSE),
                   year = vapply(g[names(en[["nodes"]])], \(x) ifelse(is.null(x[["year"]]), NA, x[["year"]]), 1980L, USE.NAMES = FALSE)) |>
    merge(en[["edges"]], by = "to", all.x = TRUE) |>
    transform(parent = vapply(g[as.character(from)], \(x) ifelse(is.null(x[["name"]]), "", x[["name"]]), "Louis", USE.NAMES = FALSE)) |>
    subset(select = c(-to, -from))

  if(!is.null(start_node) && (!is.infinite(max_anc) || !is.infinite(max_des))) {
    # Restrict ancestral and descendant generations as required
    # First to avoid infinite circular loops (which theoretically should not happen), cap max_anc and max_des at total number of mathematicians in the data (couldn't be more!)
    max_anc <- min(max_anc, length(g))
    max_des <- min(max_des, length(g))
    # Loop over start nodes to find right number of ancestral/descendant generations from there
    inc_anc <- list(start_node)
    i <- 0
    while(i < max_anc) {
      i <- i+1
      if(length(inc_anc[[1]]) == 0) break
      inc_anc <- c(list(subset(pc, child %in% inc_anc[[1]])[["parent"]]), inc_anc)
    }
    inc_des <- list(start_node)
    i <- 0
    while(i < max_des) {
      i <- i+1
      if(length(inc_des[[1]]) == 0) break
      inc_des <- c(list(subset(pc, parent %in% inc_des[[1]])[["child"]]), inc_des)
    }
    pc <- subset(pc, child %in% unique(c(inc_anc, inc_des, recursive = TRUE)))
  }

  return(pc)
}
