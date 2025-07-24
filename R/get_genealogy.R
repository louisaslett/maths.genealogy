#' Retrieve genealogy tree by mathematician ID
#'
#' Queries the genealogy of a single or set of mathematicians by their ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/).
#'
#' @references
#' Alber, D. (2024). “'geneagrapher-core' package”, <https://github.com/davidalber/geneagrapher-core>
#'
#' Jackson, A. (2007). “A Labor of Love: The Mathematics Genealogy Project”, _Notices of the AMS_, **54**(8), 1002-1003. <https://www.ams.org/notices/200708/tx070801002p.pdf>
#'
#' Mulcahy, C. (2017). “The Mathematics Genealogy Project Comes of Age at Twenty-one”, _Notices of the AMS_, **64**(5), 466-470. <https://www.ams.org/journals/notices/201705/rnoti-p466.pdf>
#'
#' @param id
#'        `integer` vector of IDs of mathematicians for whom the genealogy should be retrieved
#' @param ancestors
#'        `logical` indicating whether to include the genealogy backward to include all ancestors, defaults to `TRUE`.
#'        This can be a single `logical(1)` which then applies to all mathematicians referenced in the `id` argument, or it can be a vector of the same length as `id` providing different selection for each individual.
#' @param descendants
#'        `logical` indicating whether to include the genealogy forward to include all descendants, defaults to `TRUE`.
#'        This can be a single `logical(1)` which then applies to all mathematicians referenced in the `id` argument, or it can be a vector of the same length as `id` providing different selection for each individual.
#'
#' @return
#' A list object of class `genealogy`.
#' Each element of the list represents a mathematician in the genealogical tree.
#' The name of the element is the mathematician's ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/).
#' Each element of the object is list with containing:
#' \describe{
#'   \item{`id`}{`integer(1)` with Mathematician's ID;}
#'   \item{`name`}{`character(1)` containing the full name of the mathematician;}
#'   \item{`institution`}{`character(1)` containing the institution at which PhD was obtained;}
#'   \item{`year`}{`integer(1)` with the year their PhD was completed;}
#'   \item{`descendants`}{`integer` vector of IDs of any mathematicians who were supervised by this individual for their PhD;}
#'   \item{`advisors`}{`integer` vector of IDs of any mathematicians who were supervisors of this individual for their PhD.}
#' }
#'
#' In addition, there is an attribute named `start_nodes` which contains an `integer` vector of IDs indicating the origin nodes used in the genealogical tree search that produced this object.
#' In other words, the `id` argument as passed to this function.
#'
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' # First, you need to use search_id() to find the mathematician ID for the
#' # individual(s) you wish to plot, or visit https://mathgenealogy.org/ to look
#' # up in the browser. Once you have these IDs the get_genealogy() function will
#' # retrieve the genealogical tree.
#'
#' # For example, to find the package author would search for themselves using
#' search_id("Aslett", "Louis")
#'
#' # Then, use the id to retrieve the genealogy
#' g <- get_genealogy(171971)
#'
#' # With that genealogy, you can then plot using plot_grviz() or other plotting
#' # functions.
get_genealogy <- function(id, ancestors = TRUE, descendants = TRUE) {
  ## Input checks
  # id
  check_vec(id)
  err <- checkmate::check_integerish(id, lower = 1L, any.missing = FALSE, min.len = 1L, unique = TRUE)
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg id} argument: {err}"))
  }
  # ancestors
  check_vec(ancestors)
  if (length(ancestors) == 1L)
    ancestors <- rep(ancestors, length(id))
  err <- checkmate::check_logical(ancestors, any.missing = FALSE, len = length(id))
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg id} argument: {err}"))
  }
  # descendants
  check_vec(descendants)
  if (length(descendants) == 1L)
    descendants <- rep(descendants, length(id))
  err <- checkmate::check_logical(descendants, any.missing = FALSE, len = length(id))
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg id} argument: {err}"))
  }
  rm(err)

  ## Create query object
  records <- data.frame(
    recordId = id,
    getAdvisors = ancestors,
    getDescendants = descendants
  )

  query <- jsonlite::toJSON(list(kind = jsonlite::unbox("build-graph"),
                                 options = list(
                                   reportingCallback = jsonlite::unbox(TRUE)
                                 ),
                                 startNodes = records))

  ## Setup and connect websocket
  ws <- websocket::WebSocket[["new"]]("wss://ggrphr.davidalber.net",
                                      headers = list(`User-Agent` = get_user_agent()),
                                      autoConnect = FALSE)

  done <- FALSE # flag to indicate no more messages incoming
  err <- FALSE # flag to indicate if an error is the reason we finished, since aborting inside a WebSocket message won't eject from get_genealogy call
  msg <- "\U0001F393 Fetching PhD data ..." # for progress message
  res <- NULL
  on_message <- function(event) {
    # ws will get either text or binary: confirm we got text
    if (!checkmate::test_character(event[["data"]], min.chars = 1L, len = 1L)) {
      cli::cli_abort(c(x = "Unexpected binary response on WebSocket connection, aborting"))
    }

    # Then try to decode from JSON and handle errors if not (the error thrown by fromJSON not enough as it won't break out of get_genealogy())
    rlang::try_fetch(
      {
        res <- jsonlite::fromJSON(event[["data"]])
      },
      error = function(e) {
        # signal to get_genealogy() that we're done due to an error
        assign("done", TRUE, envir = parent.env(parent.env(environment())))
        assign("err", TRUE, envir = parent.env(parent.env(environment())))
        ws[["close"]]()
        cli::cli_abort(c(x = "Unable to parse JSON retrieved from WebSocket server"),
                       parent = e,
                       call = rlang::caller_env())
      }
    )

    # Now, check if we have a progress update or the final graph
    if (res[["kind"]] == "progress") {
      f <- res[["payload"]][["fetching"]]
      d <- res[["payload"]][["done"]]
      assign("msg", paste0("\U0001F393 Found ", f + d, " PhD records.  ", d, "/", f + d, " fetched so far ..."), envir = parent.env(environment()))
    } else if (res[["kind"]] == "graph") {
      assign("done", TRUE, envir = parent.env(environment()))
      assign("msg", "\U0001F393 Full genealogy retrieved", envir = parent.env(environment()))
      assign("res", res[["payload"]], envir = parent.env(environment()))
    } else {
      # signal to get_genealogy() that we're done due to an error
      assign("done", TRUE, envir = parent.env(environment()))
      assign("err", TRUE, envir = parent.env(environment()))
      ws[["close"]]()
      cli::cli_abort(c(x = "Unknown response type from WebSocket server"))
    }
    cli::cli_progress_update(.envir = parent.env(environment()))
  }
  ws[["onMessage"]](on_message)
  ws[["onError"]](function(event) {
    ws[["close"]]()
    cli::cli_abort(c(x = "Error on WebSocket connection: {event}"))
  })
  on_close <- function(event) {
    if (!get("done", envir = parent.env(environment()))) {
      assign("done", TRUE, envir = parent.env(environment()))
      assign("err", TRUE, envir = parent.env(environment()))
      cli::cli_abort(c(x = "Premature connection closure, code={event[['code']]} ({event[['reason']]}).\n    The geneagrapher backend is most likely unavailable right now.\n    If this error is not transient, please raise an\n      issue at {.url https://github.com/louisaslett/maths.genealogy/issues}"))
    }
  }
  ws[["onClose"]](on_close)

  connect_ws(ws)

  cli::cli_progress_step("Sending query")
  ws[["send"]](query)

  cli::cli_progress_step("{msg}")
  while (!done) {
    later::run_now(0.1)
    cli::cli_progress_update()
  }
  if (err) {
    cli::cli_progress_done(result = "failed")
    cli::cli_abort(c(x = "Failed to query genealogy, see above errors"))
  }

  ws[["close"]]()
  cli::cli_progress_done() # must end manually or else msg out of scope
  rm(msg)

  res2 <- res[["nodes"]]
  attr(res2, "start_nodes") <- res[["start_nodes"]]
  class(res2) <- c("genealogy", class(res2))
  return(res2)
}
