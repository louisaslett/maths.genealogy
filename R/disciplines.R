#' Mathematical discipline IDs
#'
#' Map mathematical disciplines to IDs for use in searching for mathematicians.
#'
#' @param search
#'        a character(1) string which will search within disciplines.
#'        This can be a regular expression search term if desired.
#'
#' @return
#' Data frame, with columns:
#' \describe{
#'   \item{`id`}{the discipline ID, as required by [search_id()] when searching for a mathematician within a specific mathematical discipline;}
#'   \item{`discipline`}{the name of the discipline classification, per the Mathematics Genealogy Project.}
#' }
#'
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Lookup the ID of any discipline involving the partial word "stat"
#' disciplines("stat")
#'
#' # Use a regular expression to only exactly match the whole word Statistics and nothing else
#' disciplines("^statistics$")
#'
#' # Use the above to search only for statisticians with the first name Louis
#' search_id(given = "Louis", discipline = disciplines("^statistics$")$id)
disciplines <- function(search = NULL) {
  check_str(search, 1L)

  d <- data.frame(
    id = c(0L, 1L, 3L, 5L, 6L, 8L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 22L, 26L, 28L, 30L, 31L, 32L, 33L, 34L, 35L, 37L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 49L, 51L, 52L, 53L, 54L, 55L, 57L, 58L, 60L, 62L, 65L, 68L, 70L, 74L, 76L, 78L, 80L, 81L, 82L, 83L, 85L, 86L, 90L, 91L, 92L, 93L, 94L, 97L),
    discipline = c("General", "History and biography", "Mathematical logic and foundations", "Combinatorics", "Order, lattices, ordered algebraic structures", "General algebraic systems", "Number theory", "Field theory and polynomials", "Commutative rings and algebras", "Algebraic geometry", "Linear and multilinear algebra; matrix theory", "Associative rings and algebras", "Nonassociative rings and algebras", "Category theory, homological algebra", "K-theory", "Group theory and generalizations", "Topological groups, Lie groups", "Real functions", "Measure and integration", "Functions of a complex variable", "Potential theory", "Several complex variables and analytic spaces", "Special functions", "Ordinary differential equations", "Partial differential equations", "Dynamical systems and ergodic theory", "Finite differences and functional equations", "Sequences, series, summability", "Approximations and expansions", "Fourier analysis", "Abstract harmonic analysis", "Integral transforms, operational calculus", "Integral equations", "Functional analysis", "Operator theory", "Calculus of variations and optimal control", "Geometry", "Convex and discrete geometry", "Differential geometry", "General topology", "Algebraic topology", "Manifolds and cell complexes", "Global analysis, analysis on manifolds", "Probability theory and stochastic processes", "Statistics", "Numerical analysis", "Computer science", "Mechanics of particles and systems", "Mechanics of deformable solids", "Fluid mechanics", "Optics, electromagnetic theory", "Classical thermodynamics, heat transfer", "Quantum Theory", "Statistical mechanics, structure of matter", "Relativity and gravitational theory", "Astronomy and astrophysics", "Geophysics", "Operations research, mathematical programming", "Game theory, economics, social and behavioral sciences", "Biology and other natural sciences", "Systems theory; control", "Information and communication, circuits", "Mathematics education"),
    stringsAsFactors = FALSE
  )

  if (!is.null(search)) {
    return(d[grep(search, d[["discipline"]], ignore.case = TRUE), ])
  }
  d
}
