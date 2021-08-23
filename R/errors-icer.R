stop_not_cea_estimate <- function() {
  x <- "`x` must be an object of class 'cea_estimate'."
  rlang::cnd_signal(rlang::error_cnd("cea_error_not_cea_estimate", message = x))
}
