stop_unknown_method <- function(method) {
  x <- paste0(
    "Invalid `method` argument.\n",
    rlang::format_error_bullets(c(
      i = "Only the 'boot' method is currently implemented.",
      x = paste0("You've supplied '", method, "'.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_method", message = x))
}

stop_unknown_outcome <- function(outcome) {
  x <- paste0(
    "Invalid outcome specified.\n",
    rlang::format_error_bullets(c(
      i = "`outcome` must include only values from `c('QALYs', 'Costs', 'INMB', 'INHB')`.",
      x = paste0("Invalid outcome: '", outcome, "'.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_outcome", message = x))
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
  x <- paste0(
    "Invalid interval type specified.\n",
    rlang::format_error_bullets(c(
      i = "`type` must include only values from `c('perc', 'norm', 'basic', 'bca')`.",
      x = if (type == "stud")
        "`boot::boot` 'stud' type is not implemented (variance estimate is unavailable).",
      x = if (type != "stud") paste0("Invalid type: '", type, "'.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_ci_type", message = x))
}

stop_R_too_small <- function(R, N) {
  x <- paste0(
    "`R` is too small.\n",
    rlang::format_error_bullets(c(
      i = "BCa intervals require R >= N(observations)",
      x = paste0("You've supplied `R` = ", R, ", but data has N = ", N, ".")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_R_too_small", message = x))
}

stop_invalid_bca_parametric <- function() {
  x <- "`type` = 'bca' cannot be used with `sim` = 'parametric'"
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_bca_parametric", message = x))
}
