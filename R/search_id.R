#' Search for mathematician in Mathematics Genealogy Project
#'
#' Perform an online search using information about an individual mathematician to find their ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/).
#'
#' Any one or more of the listed arguments can be provided.
#' This will trigger an online search against the live [Mathematics Genealogy Project](https://mathgenealogy.org/) database, so please be considerate and do not spam queries.
#' All the information returned by a standard search on the website is gathered into a data frame and returned, enabling programmatic access to the data.
#'
#' @param family
#'        a character(1) string with the family names.
#' @param given
#'        a character(1) string with the given names.
#' @param middle
#'        a character(1) string with the collapsed middle name(s).
#' @param university
#'        a character(1) string with the University at which PhD studied.
#' @param year
#'        a character(1) string or integer(1) with the year of completion.
#' @param thesis_keyword
#'        a character(1) string with keyword(s) in the PhD thesis title.
#' @param country
#'        a character(1) string with the country of study.
#' @param discipline
#'        an integer(1) with the mathematical sub-discipline code.
#'
#' @return
#' Data frame containing all matches against the provided search terms, with columns:
#' \describe{
#'   \item{`id`}{Mathematician ID (as required by [get_genealogy()])}
#'   \item{`name`}{The full name (surname first) of the mathematician}
#'   \item{`university`}{The institution at which PhD was obtained}
#'   \item{`year`}{The year PhD was completed}
#' }
#'
#' @export
#'
#' @examples
#' # Search for the package author
#' ids <- search_id("Aslett", "Louis")
#'
#' # Then use this to fetch genealogy (just descendents for speed)
#' # TODO
search_id <- function(family = NULL, given = NULL, middle = NULL, university = NULL, year = NULL, thesis_keyword = NULL, country = NULL, discipline = NULL) {
  if (all(sapply(list(family, given, middle, university, year, thesis_keyword, country, discipline), is.null))) {
    cli::cli_abort(c("x" = "At least one argument must be provided."))
  }

  url <- "https://mathgenealogy.org/query-prep.php"
  params <- list(
    family_name = family,
    given_name = given,
    other_names = middle,
    school = university,
    year = year,
    thesis = thesis_keyword,
    country = country,
    msc = discipline
  )
  params <- params[!sapply(params, is.null)]

  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = paste0("R/", R.version$major, ".", R.version$minor, " ",
                                             "maths.genealogy/", utils::packageVersion("maths.genealogy"))) |>
    httr2::req_body_form(family_name = family,
                         given_name = given,
                         other_names = middle,
                         school = university,
                         year = year,
                         thesis = thesis_keyword,
                         country = country,
                         msc = discipline) |>
    httr2::req_perform()

  if(httr2::resp_is_error(resp)) {
    cli::cli_abort(c("x" = "Failed to perform the HTTP POST request."))
  }

  html <- httr2::resp_body_html(resp)

  links <- rvest::html_elements(html, '#paddingWrapper a')
  if(length(links) == 0) {
    cli::cli_abort(c("x" = "No matches found for this search."))
  }
  ids <- rvest::html_attr(links, "href") |>
    sub(".*id=(\\d+).*", "\\1", x = _) |>
    stats::na.omit()
  names <- rvest::html_text(links)

  universities <- rvest::html_elements(html, '#paddingWrapper td:nth-child(2)') |>
    rvest::html_text()

  years <- rvest::html_elements(html, '#paddingWrapper td:nth-child(3)') |>
    rvest::html_text()

  data.frame(
    id = ids,
    name = names,
    university = universities,
    year = years
  )
}
