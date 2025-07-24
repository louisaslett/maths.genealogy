#' Search for mathematician in Mathematics Genealogy Project
#'
#' Perform an online search using information about an individual mathematician to find their ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/).
#'
#' Any one or more of the listed arguments can be provided.
#' This will trigger an online search against the live [Mathematics Genealogy Project](https://mathgenealogy.org/) database, so please be considerate and do not spam queries.
#' All the information returned by a standard search on the website is gathered into a data frame and returned, enabling programmatic access to the data.
#'
#' If you cannot find the individual you are looking for, it could be that they are not in the [Mathematics Genealogy Project](https://mathgenealogy.org/) database.
#' New data can be submitted by following the instructions in the "How to submit updates" section at <https://mathgenealogy.org/submit.php>.
#'
#' @references
#' Jackson, A. (2007). “A Labor of Love: The Mathematics Genealogy Project”, _Notices of the AMS_, **54**(8), 1002-1003. <https://www.ams.org/notices/200708/tx070801002p.pdf>
#'
#' Mulcahy, C. (2017). “The Mathematics Genealogy Project Comes of Age at Twenty-one”, _Notices of the AMS_, **64**(5), 466-470. <https://www.ams.org/journals/notices/201705/rnoti-p466.pdf>
#'
#' @param family
#'        a `character(1)` string with the family names.
#' @param given
#'        a `character(1)` string with the given names.
#' @param middle
#'        a `character(1)` string with the collapsed middle name(s).
#' @param university
#'        a `character(1)` string with the University at which PhD studied.
#' @param year
#'        a `character(1)` string or `integer(1)` with the year of completion.
#' @param thesis_keyword
#'        a `character(1)` string with keyword(s) in the PhD thesis title.
#' @param country
#'        a `character(1)` string with the country of study.
#' @param discipline
#'        an `integer(1)` with the mathematical sub-discipline code.
#'
#' @return
#' Data frame containing all matches against the provided search terms, with columns:
#' \describe{
#'   \item{`id`}{Mathematician ID (as required by [get_genealogy()]);}
#'   \item{`name`}{The full name (surname first) of the mathematician;}
#'   \item{`university`}{The institution at which PhD was obtained;}
#'   \item{`year`}{The year PhD was completed.}
#' }
#'
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Search for the package author
#' search_id("Aslett", "Louis")
#'
#' # You may find it easier to directly use the https://mathgenealogy.org/
#' # website, and extract the "id" from the URL on the page for the mathematician
#' # of interest.
search_id <- function(family = NULL, given = NULL, middle = NULL, university = NULL, year = NULL, thesis_keyword = NULL, country = NULL, discipline = NULL) {
  # Input checks
  if (all(vapply(list(family, given, middle, university, year, thesis_keyword, country, discipline), is.null, TRUE))) {
    cli::cli_abort(c(x = "At least one argument must be provided."))
  }
  check_str(family, 2L)
  check_str(given, 1L)
  check_str(middle, 1L)
  check_str(university, 2L)
  err <- checkmate::check_integerish(year, lower = 0L, upper = as.integer(format(Sys.Date(), "%Y")), any.missing = FALSE, len = 1L)
  if (!is.null(year) && !identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg year} argument: {err}"))
  }
  check_str(thesis_keyword, 2L)
  check_str(country, 2L)
  err <- checkmate::check_choice(discipline, disciplines()[["id"]])
  if (!is.null(discipline) && !identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg discipline} argument: {err}"))
  }
  rm(err)

  # Input modification
  if (!is.null(discipline)) {
    discipline <- sprintf("%02d", discipline)
  }

  if (!curl::has_internet()) {
    cli::cli_abort(c(x = "Internet connection required."))
  }
  # Query Mathematics Genealogy Project
  rlang::try_fetch(
    {
      resp <- httr2::request("https://mathgenealogy.org/query-prep.php") |>
        httr2::req_headers(`User-Agent` = get_user_agent()) |>
        httr2::req_body_form(family_name = family,
                             given_name = given,
                             other_names = middle,
                             school = university,
                             year = year,
                             thesis = thesis_keyword,
                             country = country,
                             msc = discipline) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (inherits(e, "httr2_failure")) {
        cli::cli_abort(c(x = "Please check your internet connection. If that is working, check whether the Maths Genealogy Project is currently down: {.url https://mathgenealogy.org/} -- if both are ok, please file a detailed reproducible bug report {.url https://github.com/louisaslett/maths.genealogy/issues}"),
                       parent = e,
                       call = rlang::caller_env())
      } else if (inherits(e, "httr2_http")) {
        cli::cli_abort(c(x = "There appears to be a problem at the Maths Genealogy Project. Please try your search directly at {.url https://mathgenealogy.org/} -- if your search works on the site, then please file a detailed reproducible bug report for this package at {.url https://github.com/louisaslett/maths.genealogy/issues}"),
                       parent = e,
                       call = rlang::caller_env())
      } else {
        cli::cli_abort(c(x = "Unknown error. Please file a detailed reproducible bug report for this package at {.url https://github.com/louisaslett/maths.genealogy/issues}"),
                       parent = e,
                       call = rlang::caller_env())
      }
    }
  )

  html <- httr2::resp_body_html(resp)

  links <- rvest::html_elements(html, "#paddingWrapper a")
  if (length(links) == 0L) {
    cli::cli_inform(c(i = "No matches found for this search."))
  }
  ids <- rvest::html_attr(links, "href") |>
    sub(".*id=(\\d+).*", "\\1", x = _) |>
    stats::na.omit()
  nm <- rvest::html_text(links)

  universities <- rvest::html_elements(html, "#paddingWrapper td:nth-child(2)") |>
    rvest::html_text()

  years <- rvest::html_elements(html, "#paddingWrapper td:nth-child(3)") |>
    rvest::html_text()

  data.frame(
    id = ids,
    name = nm,
    university = universities,
    year = years
  )
}
