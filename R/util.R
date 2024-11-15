check_str <- function(s, min.chars) {
  name <- deparse(substitute(s))
  err <- checkmate::check_character(s, min.chars = min.chars, len = 1L)
  if (!is.null(s) && !identical(err, TRUE)) {
    cli::cli_abort(c(x = "{.arg {name}} argument: {err}"), call = rlang::caller_env(), .frame = rlang::caller_env())
  }
  c(name, err)
}
