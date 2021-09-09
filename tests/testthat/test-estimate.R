test_that("estimate works with default specification", {
  expect_s3_class(fit, "cea_mcglm")
  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit_mcglm, ignore_attr = c("class", "call", "tx"), ignore_formula_env = TRUE)
  expect_s3_class(fit_fct, "cea_estimate")
  expect_s3_class(fit_fct2, "cea_estimate")
  expect_output(estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex,
                         verbose = TRUE))
})

test_that("estimate gives appropriate messages", {
  expect_error(estimate(1, "Cost", "booster", c("age", "sex"), data = moa2_ex),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", 2, "booster", c("age", "sex"), data = moa2_ex),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", "Cost", 3, c("age", "sex"), data = moa2_ex),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", "Cost", "booster", 4, data = moa2_ex),
               class = "cea_error_not_character")
  expect_error(estimate("qalys", "Cost", "booster", c("age", "sex"), data = moa2_ex),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "costs", "booster", c("age", "sex"), data = moa2_ex),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_ex),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_chr),
               class = "cea_error_invalid_treatment")
  expect_error(estimate("QALYs", "Cost", "booster", c("age", "gender"), data = moa2_ex),
               class = "cea_error_variable_not_found")
  expect_error(estimate(1, "Cost", "booster", c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
               class = "cea_error_not_string")
  expect_error(
    estimate("QALYs", 2, "booster", c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_not_string"
  )
  expect_error(estimate("QALYs", "Cost", 3, c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", "Cost", "booster", 4, data = moa2_ex, method = "mglmmPQL"),
               class = "cea_error_not_character")
  expect_error(
    estimate("qalys", "Cost", "booster", c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  expect_error(
    estimate("QALYs", "costs", "booster", c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  expect_error(
    estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_chr, method = "mglmmPQL"),
    class = "cea_error_invalid_treatment"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "gender"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  expect_warning(
    estimate("QALYs", "Cost", "booster", linear_pred = list(Cost ~ booster + age + sex),
             data = moa2_ex),
    class = "cea_warning_formula_override"
  )
  expect_warning(
    estimate("QALYs", "Cost", "booster", mvfixed = list(Costs = Cost ~ booster + age + sex),
             data = moa2_ex, method = "mglmmPQL"),
    class = "cea_warning_formula_override"
  )
  suppressWarnings(expect_error(
    estimate("QALYs", "Cost", 1, linear_pred = list(Cost ~ booster + age + sex),
             c("age", "sex"), data = moa2_ex),
    class = "cea_error_not_string"
  ))
  suppressWarnings(expect_error(
    estimate("QALYs", "Cost", 1, linear_pred = list(Cost ~ booster + age + sex),
             c("age", "sex"), data = moa2_ex, method = "mglmmPQL"),
    class = "cea_error_not_string"
  ))
  expect_error(
    withr::with_options(
      list("rlang:::is_installed_hook" = function(pkg, version) FALSE),
      estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_mi)
    ),
    regexp = "mice", class = "cea_error_pkg_not_installed"
  )
  expect_error(
    withr::with_options(
      list("rlang:::is_installed_hook" = function(pkg, version) version < 3.0),
      estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_mi)
    ),
    regexp = "mice", class = "cea_error_pkg_not_installed"
  )
  expect_error(
    withr::with_options(
      list("rlang:::is_installed_hook" = function(pkg, version) FALSE),
      estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)
    ),
    regexp = "mcglm", class = "cea_error_pkg_not_installed"
  )
  expect_error(
    withr::with_options(
      list("rlang:::is_installed_hook" = function(pkg, version) FALSE),
      estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex, method = "mglmmPQL")
    ),
    regexp = "mglmmPQL", class = "cea_error_pkg_not_installed"
  )
  expect_warning(
    estimate(
      "QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "centre",
      matrix_pred = rep(
        list(c(mcglm::mc_id(moa2_centre), mcglm::mc_mixed(~0 + centre, moa2_centre))),
        2
      )
    ),
    class = "cea_warning_cluster_override"
  )
  expect_warning(
    estimate(
      "QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre",
      matrix_pred = rep(
        list(c(mcglm::mc_id(moa2_centre), mcglm::mc_mixed(~0 + centre, moa2_centre))),
        2
      )
    ),
    class = "cea_warning_cluster_override"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre",
             cluster = "cluster"),
    class = "cea_error_cluster_centre"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = 1),
    class = "cea_error_not_string"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "cluster"),
    class = "cea_error_variable_not_found"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = 1),
    class = "cea_error_not_string"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "cluster"),
    class = "cea_error_variable_not_found"
  )
  expect_warning(
    estimate(
      "QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "centre",
      random = ~outvar - 1 | centre, method = "mglmmPQL"
    ),
    class = "cea_warning_cluster_override"
  )
  expect_warning(
    estimate(
      "QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre",
      random = ~outvar - 1 | centre, method = "mglmmPQL"
    ),
    class = "cea_warning_cluster_override"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre",
             cluster = "cluster", method = "mglmmPQL"),
    class = "cea_error_cluster_centre"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = 1,
             method = "mglmmPQL"),
    class = "cea_error_not_string"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "cluster",
             method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = 1,
             method = "mglmmPQL"),
    class = "cea_error_not_string"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "cluster",
             method = "mglmmPQL"),
    class = "cea_error_variable_not_found"
  )
  moa2_centre$centre <- as.integer(moa2_centre$centre)
  expect_warning(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre"),
    class = "cea_warning_not_factor"
  )
  expect_warning(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "centre"),
    class = "cea_warning_not_factor"
  )
  expect_warning(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, centre = "centre",
             method = "mglmmPQL"),
    class = "cea_warning_not_factor"
  )
  expect_warning(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre, cluster = "centre",
             method = "mglmmPQL"),
    class = "cea_warning_not_factor"
  )
  expect_error(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex, method = "method"),
    class = "cea_error_invalid_method"
  )
})

test_that("estimate works with custom `linear_pred`", {
  expect_s3_class(fit_lp, "mcglm")
  expect_equal(fit_lp, fit_mcglm,
               ignore_attr = c("class", "tx", "call"), ignore_formula_env = TRUE)
})

test_that("estimate works with list data", {
  moa2_ex <- as.list(moa2_ex)
  fit_list <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)

  expect_s3_class(fit_list, "cea_estimate")
  expect_s3_class(fit_list, "mcglm")
  expect_equal(fit_list, fit, ignore_formula_env = TRUE, ignore_attr = "call")
})

test_that("estimate works with mids data", {
  skip_if_not_installed("mice", "3.0")
  expect_s3_class(fit_mi, "cea_mira")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit_mi$analyses[[1]], fit_fct, ignore_formula_env = TRUE, ignore_attr = "call")
})

test_that("estimate works with missing covars", {
  fit <- estimate("QALYs", "Cost", "booster", data = moa2_ex)
  fit2 <- with_sink(
    tempfile(),
    mcglm::mcglm(
      linear_pred = c(QALYs = QALYs ~ booster, Costs = Cost ~ booster),
      matrix_pred = list(mcglm::mc_id(moa2_ex), mcglm::mc_id(moa2_ex)),
      link = c("identity", "log"), variance = c("constant", "tweedie"),
      data = moa2_ex, control_algorithm = list(max_iter = 50)
    )
  )

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit2, ignore_attr = c("class", "tx", "call"), ignore_formula_env = TRUE)
})

test_that("estimate works with clustered data", {
  expect_s3_class(fit_cluster, "cea_estimate")
  expect_length(fit_cluster$matrix_pred, 2)
  expect_length(fit_cluster$matrix_pred[[1]], 2)
  expect_length(fit_cluster$Covariance, 5)
  expect_equal(attr(fit_cluster, "cluster"), "centre")

  expect_equal(fit_cluster, fit_centre, ignore_attr = c("call", "cluster", "centre"),
               ignore_formula_env = TRUE)
  expect_equal(attr(fit_centre, "centre"), "centre")

  expect_equal(fit_cluster, fit_mp, ignore_attr = c("call", "cluster"), ignore_formula_env = TRUE)
})

test_that("estimate works with mglmmPQL method", {
  expect_s3_class(fit_mglmmPQL, "cea_mglmmPQL")
  expect_s3_class(fit_mglmmPQL, "cea_estimate")
  expect_s3_class(fit_mglmmPQL, "mglmmPQL")
  expect_s3_class(fit_mglmmPQL, "lme")
  expect_equal(
    fit_mglmmPQL[-c(11, 18, 21:22)],
    {
      data = make_data_longform(moa2_ex, c("QALYs", "Cost")); data$.cons <- 1
      mglmmPQL(list(QALYs = value ~ booster + age + sex, Costs = value ~ booster + age + sex),
               ~ outvar - 1 | .cons, c("gaussian", "quasipoisson"), verbose = FALSE,
               weights = nlme::varIdent(form = ~1 | outvar), data = data,
               outcomevar = "outcome")[-c(11, 18)]
    }, ignore_attr = TRUE
  )
  expect_equal(
    extract_coefs(fit_mglmmPQL),
    c(outvarQALYs = 0.827995485, outvarCost = 6.542768952, "booster:outvarQALYs" = 0.069154392,
      "booster:outvarCost" = 0.675092770, "outvarQALYs:age" = 0.005092244,
      "outvarCost:age" = 0.021166115, "outvarQALYs:sex" = 0.159939192,
      "outvarCost:sex" = -0.183367224)
  )
  expect_s3_class(
    estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex, random = NULL,
             method = "mglmmPQL"),
    "cea_mglmmPQL"
  )
})

test_that("cea_estimate print methods work", {
  expect_snapshot_output(fit)
  expect_snapshot_output(fit_mglmmPQL)
  with_sink(tempfile(), expect_equal(print(fit), fit))
  expect_snapshot_output(fit_fct)
  skip_if_not_installed("mice", "3.0")
  expect_snapshot_output(fit_mi)
})
