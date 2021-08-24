stop_not_cea_estimate <- function() {
  x <- "`x` must be an object of class 'cea_estimate'."
  rlang::cnd_signal(rlang::error_cnd("cea_error_not_cea_estimate", message = x))
}

stop_not_formula_spec <- function(fn) {
  x <- paste0(
    "`x` must be estimated using the default 'formula' specification.\n",
    rlang::format_error_bullets(c(
      i = paste0("With a custom 'linear_pred' specification, ", fn,
                 " must be calculated manually.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_not_formula_spec", message = x))
}
