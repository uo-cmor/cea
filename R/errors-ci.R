stop_unknown_method <- function(method) {
  x <- rlang::format_error_bullets(c(
    "Invalid `method` argument.",
    i = "'boot' and 'delta' methods are currently implemented.",
    x = paste0("You've supplied '", method, "'.")
  ))

  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_method", message = x))
}

stop_invalid_outcome <- function() {
  x <- "Invalid `outcomes` argument."

  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_outcome", message = x))
}

stop_missing_wtp <- function() {
  x <- "`wtp` must be specified if 'INMB' or 'INHB' are included in `outcomes`."
  rlang::cnd_signal(rlang::error_cnd("cea_error_missing_wtp", message = x))
}

stop_missing_R <- function() {
  x <- "`R` must be specified if `method` == 'boot'."
  rlang::cnd_signal(rlang::error_cnd("cea_error_missing_R", message = x))
}

stop_invalid_ci_type <- function(type) {
  x <- rlang::format_error_bullets(c(
    "Invalid interval type specified.",
    i = "`type` must include only values from `c('perc', 'norm', 'basic', 'bca')`.",
    x = if (type == "stud")
      "`boot::boot` 'stud' type is not implemented (variance estimate is unavailable).",
    x = if (type != "stud") paste0("Invalid type: '", type, "'.")
  ))
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_ci_type", message = x))
}

stop_R_too_small <- function(R, N) {
  x <- rlang::format_error_bullets(c(
    "`R` is too small.",
    i = "BCa intervals require R >= N(observations)",
    x = paste0("You've supplied `R` = ", R, ", but data has N = ", N, ".")
  ))
  rlang::cnd_signal(rlang::error_cnd("cea_error_R_too_small", message = x))
}

stop_invalid_bca_parametric <- function() {
  x <- "`type` = 'bca' cannot be used with `sim` = 'parametric'"
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_bca_parametric", message = x))
}

message_unused_brms_arguments <- function(fn) {
  x <- rlang::format_error_bullets(paste0(
    "`", fn, "()`: Arguments ", if (fn == "ci") "'type', ",
    "'method', 'R', and 'sim' are ignored for `cea_brms` objects"
  ))
  rlang::cnd_signal(rlang::message_cnd("cea_message_unused_brms_arguments", message = x))
}
