warn_formula_override <- function(var) {
  x <- paste0("`", var, "` specification overriding `QALYs`, `costs`, and `covars`.")
  rlang::cnd_signal(rlang::warning_cnd("cea_warning_formula_override", message = x))
}

warn_cluster_override <- function(argument, cluster) {
  x <- paste0("`", argument, "` specification overriding `", cluster, "`.")
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

stop_wrong_type <- function(arg, required) {
  x <- paste0(
    "Invalid argument.\n",
    rlang::format_error_bullets(c(i = paste0("`", arg, "` must be ", required, ".")))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_wrong_type", message = x))
}

stop_invalid_family <- function(i) {
  x <- paste0(
    "Invalid family.\n",
    rlang::format_error_bullets(c(i = paste0("Supplied family #", i, " is not recognised.")))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_family", message = x))
}

stop_unknown_variance <- function(i) {
  x <- paste0(
    "Unknown variance function.\n",
    rlang::format_error_bullets(c(
      i = paste0("Variance for family #", i, " could not be identified.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_variance", message = x))
}

stop_invalid_method <- function(method) {
  x <- paste0(
    "Argument `method` must be one of \"mcglm\", \"mglmmPQL\".\n",
    rlang::format_error_bullets(c(
      x = paste0("You've supplied ", rlang::as_label(method), ".")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_invalid_method", message = x))
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

stop_pkg_not_installed <- function(pkg, fn, req_vers = NULL, version = NULL) {
  x <- paste0(
    "Package `", pkg, "` ", if (!is.null(req_vers)) paste("version", req_vers, "or greater "),
    "is required for ", fn, ".\n",
    rlang::format_error_bullets(c(
      i = if (!is.null(version)) paste("Version", version, "is currently installed."),
      "*" = paste0(if (is.null(version)) "Install" else "Update", " with `install.packages('", pkg, "')`.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_pkg_not_installed", message = x))
}
