stop_incorrect_class <- function(class) {
  x <- paste0("`x` must be an object of class '", class, "'.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_incorrect_class", message = x))
}

stop_unknown_estimand <- function(estimand) {
  x <- rlang::format_error_bullets(c(
    "`estimand` must be one of 'ATE', 'ATT', 'ATC'.",
    x = paste0("You've provided '", estimand, "'`.")
  ))
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_estimand", message = x))
}

stop_unknown_outcome <- function(outcome) {
  x <- paste0("Outcome `", outcome, "` not found in model.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_outcome", message = x))
}

stop_unknown_treatment <- function(tx) {
  x <- paste0("Treatment variable `", tx, "` not found in model.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_treatment", message = x))
}
