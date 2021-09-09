test_that("estimate gives appropriate messages", {
  expect_error(pool_cea(fit_fct), class = "cea_error_incorrect_class")
})

skip_if_not_installed("mice", "3.0")

test_that("pool_cea works as expected", {
  expect_s3_class(fit_pooled, "cea_mcglm_pooled")
  expect_s3_class(fit_pooled, "cea_estimate")
  expect_equal(fit_pooled$m, 2)
  expect_equal(fit_pooled$Regression, fit_fct$Regression)
  expect_equal(fit_pooled$Covariance, fit_fct$Covariance)
  expect_equal(fit_pooled$vcov, as.matrix(fit_fct$vcov), ignore_attr = "dimnames")
  expect_equal(fit_pooled$Information, fit_fct$Information)
  expect_equal(fit_pooled$beta_names, fit_fct$beta_names)
  expect_equal(fit_pooled$power_fixed, fit_fct$power_fixed)
  expect_equal(fit_pooled$n_obs, fit_fct$n_obs)
  expect_equal(fit_pooled$link, fit_fct$link)
  expect_equal(fit_pooled$variance, fit_fct$variance)
  expect_equal(fit_pooled$covariance, fit_fct$covariance)
  expect_equal(fit_pooled$linear_pred, fit_fct$linear_pred, ignore_formula_env = TRUE)
  expect_equal(fit_pooled$list_X, fit_fct$list_X, ignore_attr = "names")
  expect_equal(fit_pooled$matrix_pred, fit_fct$matrix_pred)
  expect_equal(fit_pooled$Ntrial, fit_fct$Ntrial)
  expect_equal(fit_pooled$offset, fit_fct$offset)
  expect_equal(fit_pooled$data, fit_fct$data)
  expect_s3_class(fit_pooled$pooled, "data.frame")
  expect_named(fit_pooled$pooled,
               c("estimate", "ubar", "b", "t", "dfcom", "df", "riv", "lambda", "fmi"))
  expect_equal(fit_pooled$pooled$estimate, c(fit_pooled$Regression, fit_pooled$Covariance))
  expect_equal(fit_pooled$pooled$ubar, diag(as.matrix(fit_fct$vcov)))
  expect_equal(fit_pooled$pooled$b, rep(0, 15))
  expect_equal(fit_pooled$pooled$t, fit_pooled$pooled$ubar)
  expect_equal(fit_pooled$pooled$t, diag(fit_pooled$vcov), ignore_attr = "names")
  expect_equal(fit_pooled$pooled$dfcom, rep(138, 15))
  expect_equal(fit_pooled$pooled$df, rep(136.042553, 15))
  expect_equal(fit_pooled$pooled$riv, rep(0, 15))
  expect_equal(fit_pooled$pooled$lambda, rep(0, 15))
  expect_equal(fit_pooled$pooled$fmi, rep(0.0143840857, 15))
})

test_that("coef.fit_pooled works", {
  expect_equal(coef(fit_pooled), c(fit_pooled$Regression, fit_pooled$Covariance))
})

test_that("vcov.fit_pooled works", {
  expect_equal(vcov(fit_pooled), as.matrix(fit_pooled$vcov))
})

test_that("print.cea_mcglm_pooled works", {
  expect_snapshot_output(fit_pooled)
  with_sink(tempfile(), expect_equal(print(fit), fit))
})
