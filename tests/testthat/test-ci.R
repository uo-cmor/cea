suppressWarnings({
  fit_ci <- ci(fit, c("QALYs", "Costs", "INMB"), conf = 0.8, type = "perc", R = 9, wtp = 60000)
  fit_ci_boot <- ci(boot_est, c("QALYs", "Costs", "INMB"), conf = 0.8, type = "perc", wtp = 60000)
})

test_that("ci works with cea_estimate objects", {
  expect_s3_class(fit_ci, "cea_ci")
  expect_equal(dim(fit_ci), c(3, 2))
  expect_true(all(fit_ci[, 1] < c(0.074458875, 2022.699, 2444.833)))
  expect_true(all(fit_ci[, 2] > c(0.074458875, 2022.699, 2444.833)))
  expect_equal(attr(fit_ci, "conf"), 0.8)
  expect_equal(attr(fit_ci, "type"), "perc")
  expect_equal(attr(fit_ci, "R"), 9)
})

test_that("ci works with cea_boot objects", {
  expect_s3_class(fit_ci_boot, "cea_ci")
  expect_equal(dim(fit_ci_boot), c(3, 2))
  expect_true(all(fit_ci_boot[, 1] < c(0.074458875, 2022.699, 2444.833)))
  expect_true(all(fit_ci_boot[, 2] > c(0.074458875, 2022.699, 2444.833)))
  expect_equal(attr(fit_ci_boot, "conf"), 0.8)
  expect_equal(attr(fit_ci_boot, "type"), "perc")
  expect_equal(attr(fit_ci_boot, "R"), 9)
})

test_that("ci gives appropriate error messages", {
  expect_error(ci(fit_mcglm), "no applicable method")
  expect_error(ci(fit, method = "x"), class = "cea_error_unknown_method")
  expect_error(ci(fit, outcomes = c("QALYs", "costs")), class = "cea_error_unknown_outcome")
  expect_error(ci(fit), class = "cea_error_missing_wtp")
  expect_error(ci(fit, wtp = 60000), class = "cea_error_missing_R")
  expect_error(ci(fit, wtp = 60000, R = 1, type = "stud"), class = "cea_error_invalid_ci_type")
  expect_error(ci(fit, wtp = 60000, R = 1, type = "all"), class = "cea_error_invalid_ci_type")
  expect_error(ci(fit, wtp = 60000, R = 1), class = "cea_error_R_too_small")
  expect_error(ci(fit, wtp = 60000, R = 39, sim = "parametric"),
               class = "cea_error_invalid_bca_parametric")

  expect_error(ci(boot_est, outcomes = c("QALYs", "costs")), class = "cea_error_unknown_outcome")
  expect_error(ci(boot_est), class = "cea_error_missing_wtp")
  expect_error(ci(boot_est, wtp = 60000, type = "stud"), class = "cea_error_invalid_ci_type")
  expect_error(ci(boot_est, wtp = 60000, type = "all"), class = "cea_error_invalid_ci_type")
  expect_error(ci(boot_est, wtp = 60000), class = "cea_error_R_too_small")
  expect_error(ci(boot_est_par, wtp = 60000), class = "cea_error_invalid_bca_parametric")
})

test_that("print.cea_ci works", {
  ci_ex <- structure(
    matrix(c(-0.043, -23.4, -4689, 0.247, 4241, 13993), ncol = 2,
           dimnames = list(c("QALYs", "Costs", "INMB"), c("Lower", "Upper"))),
    class = "cea_ci", conf = 0.8, type = "bca", R = 99
  )
  expect_snapshot_output(ci_ex)
  with_sink(tempfile(), expect_equal(print(ci_ex), ci_ex))
})
