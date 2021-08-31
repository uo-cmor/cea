stop_unknown_sim <- function(sim) {
  x <- paste0(
    "Invalid simulation type specified.\n",
    rlang::format_error_bullets(c(
      i = "`sim` must be one of `c('ordinary', 'parametric', 'balanced', 'permutation')`.",
      x = if (sim == "antithetic")
        "`boot::boot` 'antithetic' type is not implemented.",
      x = if (sim != "antithetic") paste0("Invalid type: '", sim, "'.")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_unknown_sim", message = x))
}

stop_bootstrap_pooled <- function() {
  x <- paste0(
    "The non-parametric bootstrap is not implemented for imputation analyses.\n",
    rlang::format_error_bullets(c(
      i = "Use the parametric boostrap with `sim` = 'parametric' or use the delta method",
      " " = "to calculate standard errors with `method` = 'delta' in `ci()` and `ceac()`"
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("cea_error_bootstrap_pooled", message = x))
}
