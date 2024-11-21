#' Retrieve genealogy tree by mathematician ID
#'
#' Queries the genealogy of a single or set of mathematicians by their ID in the [Mathematics Genealogy Project](https://mathgenealogy.org/).
#'
#' @param id
#'        vector of integer IDs of mathematicians for whom the genealogy should be retrieved
#' @param ancestors
#'        logical indicating whether to include the genealogy backward to include all ancestors, defaults to `TRUE`.
#'        This can be a single logical which then applies to all mathematicians referenced in the `id` argument, or it can be a vector of the same length as `id` providing different selection for each individual.
#' @param descendants
#'        logical indicating whether to include the genealogy forward to include all descendants, defaults to `TRUE`.
#'        This can be a single logical which then applies to all mathematicians referenced in the `id` argument, or it can be a vector of the same length as `id` providing different selection for each individual.
#'
#' @return
#' A joint genealogy object
#'
#' @export
#'
#' @examples
#' # TODO
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
  ancestors <- ifelse(length(ancestors) == 1L, rep(ancestors, length(id)), ancestors)
  err <- checkmate::check_logical(ancestors, any.missing = FALSE, len = length(id))
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg id} argument: {err}"))
  }
  # descendants
  check_vec(descendants)
  descendants <- ifelse(length(descendants) == 1L, rep(descendants, length(id)), descendants)
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
  msg <- "Fetching results ..." # for progress message
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
      q <- res[["payload"]][["queued"]]
      f <- res[["payload"]][["fetching"]]
      d <- res[["payload"]][["done"]]
      assign("msg", paste0("Found ", f + d, " records (", q, " in queue). Currently ", d, "/", f + d, " fetched ..."), envir = parent.env(environment()))
    } else if (res[["kind"]] == "graph") {
      assign("done", TRUE, envir = parent.env(environment()))
      assign("msg", "Full genealogy retrieved", envir = parent.env(environment()))
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
