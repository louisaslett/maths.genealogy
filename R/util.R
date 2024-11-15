# For checking an argument to a function.
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



# Construct a User-Agent string for sending on HTTP/WebSocket requests, following the format requested in https://github.com/davidalber/geneagrapher/issues/38
get_user_agent <- function() {
  paste0("R/", R.version[["major"]], ".", R.version[["minor"]], " ",
         utils::packageName(), "/", utils::packageVersion(utils::packageName()))
}
