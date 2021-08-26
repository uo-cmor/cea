warn_formula_override <- function(lhs) {
  x <- paste0(
    "`linear_pred` specification used in place of `formula`.\n",
    rlang::format_error_bullets(c(
      i = paste("Returned object will have class `mcglm` and will not work",
                "with other `cea`-package functions"),
      i = paste("Provide only one of `linear_pred` or `formula` to create",
                "`cea_estimate` objects.")
    ))
  )
  rlang::cnd_signal(rlang::warning_cnd("cea_warning_formula_override", message = x))
}

stop_not_string <- function(var) {
  x <- paste0("Argument `", var, "` must be a string scalar.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_not_string", message = x))
}

stop_not_character <- function(var) {
  x <- paste0("Argument `", var, "` must be a character vector.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_not_character", message = x))
}

stop_variable_not_found <- function(var, df) {
  x <- paste0("Can't find column `", var, "` in `", df, "`.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_variable_not_found", message = x))
}
