#' Retrieve genealogy tree by mathematician ID
#'
#' Queries the genealogy of a set of mathematicians by their ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/)
#'
#' @param id
#'        vector of integer IDs of mathematicians for whom the genealogy should be retrieved
#' @param ancestors
#'        logical indicating whether to include the genealogy backward to include all ancestors, defaults to `TRUE`.
#' @param descendents
#'        logical indicating whether to include the genealogy forward to include all descendents, defaults to `TRUE`.
#'
#' @return
#' A joint genealogy object
#'
#' @export
#'
#' @examples
#' # TODO
get_genealogy <- function(id, ancestors = TRUE, descendents = TRUE) {
  # TODO
}
