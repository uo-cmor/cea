warn_formula_override <- function() {
  x <- "`linear_pred` specification overriding `QALYs`, `costs`, and `covars`."
  rlang::cnd_signal(rlang::warning_cnd("cea_warning_formula_override", message = x))
}

warn_cluster_override <- function(variable) {
  x <- paste0("`matrix_pred` specification overriding `", variable, "`.")
  rlang::cnd_signal(rlang::warning_cnd("cea_warning_cluster_override", message = x))
}

warn_not_factor <- function(variable, cluster) {
  x <- paste0(
    "Coercing the ", variable, " variable `", cluster, "` to a factor.\n",
    rlang::format_error_bullets(c(
      i = paste0("Specify a factor variable as `", variable, "` to silence this message.")
    ))
  )
  rlang::cnd_signal(rlang::warning_cnd("cea_warning_not_factor", message = x))
}

stop_cluster_centre <- function() {
  x <- paste0("Only one of `centre` or `cluster` should be specified.")
  rlang::cnd_signal(rlang::error_cnd("cea_error_cluster_centre", message = x))
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

stop_invalid_treatment <- function(tx, type) {
  x <- paste0(
    "Treatment variable must be a factor or 0/1 dummy variable.\n",
    rlang::format_error_bullets(c(
      i = paste0("Variable `", tx, "` in `data` is of type '", type, "'.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_treatment", message = x))
}

stop_mice_not_installed <- function(version = NULL) {
  x <- paste0(
    "`mice` version 3.0 or greater is required for `estimate.mids()`.\n",
    rlang::format_error_bullets(c(
      i = if (!is.null(version)) paste("Version", version, "is currently installed."),
      "*" = paste(if (is.null(version)) "Install" else "Update", "with `install.packages('mice')`.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_mice_not_installed", message = x))
}
