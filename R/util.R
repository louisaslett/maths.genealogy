## For checking an argument to a function.

# Checks that `s` is a character(1) string with at least `min.chars` characters.
# Throws error in caller's environment if not, referencing the name of what was passed in the error message.
check_str <- function(s, min.chars) {
  name <- deparse(substitute(s))
  err <- checkmate::check_character(s, min.chars = min.chars, len = 1L)
  if (!is.null(s) && !identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg {name}} argument: {err}"), call = rlang::caller_env(), .frame = rlang::caller_env())
  }
  c(name, err)
}

# Check it is a vector, not a matrix/list/etc
check_vec <- function(x) {
  name <- deparse(substitute(x))
  err <- checkmate::check_atomic_vector(x)
  if (!identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg {name}} argument: {err}"), call = rlang::caller_env(), .frame = rlang::caller_env())
  }
  c(name, err)
}

# Check it is an object returned by get_genealogy()
check_genealogy <- function(x) {
  name <- deparse(substitute(x))
  if (!checkmate::test_class(x, "genealogy")) {
    cli::cli_abort(c(x = "{.arg {name}} must be a {.cls genealogy} object as returned by {.fun get_genealogy}."), call = rlang::caller_env(), .frame = rlang::caller_env())
  }
  name
}



# Construct a User-Agent string for sending on HTTP/WebSocket requests, following the format requested in https://github.com/davidalber/geneagrapher/issues/38
get_user_agent <- function() {
  paste0("R/", R.version[["major"]], ".", R.version[["minor"]], " ",
         utils::packageName(), "/", utils::packageVersion(utils::packageName()))
}



# Polling for WebSocket connection, per {websocket} docs https://github.com/rstudio/websocket/issues/40
connect_ws <- function(ws, timeout = 4L) {
  cli::cli_progress_step("Connecting to {.emph geneagrapher-core} WebSocket server", spinner = TRUE)
  ws[["connect"]]()
  cli::cli_progress_update()

  connected <- FALSE
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    later::run_now(0.05)

    ready_state <- ws[["readyState"]]()
    if (ready_state == 0L) {
      cli::cli_progress_update()
    } else if (ready_state == 1L) {
      connected <- TRUE
    } else {
      break
    }
  }

  cli::cli_progress_done(result = ifelse(connected, "done", "failed"))

  if (!connected) {
    cli::cli_abort(c(x = "Unable to establish WebSocket connection"))
  }
}
