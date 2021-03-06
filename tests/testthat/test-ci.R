suppressWarnings({
  fit_ci <- ci(fit, c("QALYs", "Costs", "INMB"), conf = 0.8, type = "perc", R = 9, wtp = 60000,
               method = "boot", sim = "ordinary")
  fit_ci_delta <- ci(fit, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000, method = "delta")
  fit_ci_boot <- ci(boot_est, c("QALYs", "Costs", "INMB"), conf = 0.8, type = "perc", wtp = 60000)
  fit_ci_fct <- ci(fit_fct, c("QALYs", "Costs", "INMB"), conf = 0.8, type = "perc", R = 9,
                   wtp = 60000, method = "boot", sim = "ordinary")
  fit_ci_fct_delta <- ci(fit_fct, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000,
                         method = "delta")
  fit_ci_fct_delta2 <- ci(fit_fct2, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000,
                          method = "delta")
  fit_ci_pooled <- suppressWarnings(
    ci(fit_pooled, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000, method = "boot",
       sim = "parametric", R = 9, type = "perc")
  )
  fit_ci_pooled_delta <- ci(fit_pooled, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000,
                            method = "delta")
  fit_ci_cluster <- suppressWarnings(
    ci(fit_cluster, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000, method = "boot",
       sim = "parametric", R = 9, type = "perc")
  )
  fit_ci_cluster_delta <- ci(fit_cluster, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000,
                             method = "delta")
  fit_ci_mglmmPQL <- ci(fit_mglmmPQL, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000,
                        method = "boot", sim = "parametric", R = 9, type = "perc")
})

test_that("ci works with cea_mcglm objects", {
  expect_s3_class(fit_ci, "cea_ci")
  expect_length(fit_ci, 3)
  expect_equal(dim(fit_ci[[1]]), c(1, 2))
  expect_true(fit_ci[[1]][, 1] < fit_ci[[1]][, 2])
  expect_true(fit_ci[[2]][, 1] < fit_ci[[2]][, 2])
  expect_true(fit_ci[[3]][, 1] < fit_ci[[3]][, 2])
  expect_equal(attr(fit_ci, "conf"), 0.8)
  expect_equal(attr(fit_ci, "method"), "boot")
  expect_equal(attr(fit_ci, "type"), "perc")
  expect_equal(attr(fit_ci, "R"), 9)
})

test_that("ci works with cea_mglmmPQL objects", {
  expect_s3_class(fit_ci_mglmmPQL, "cea_ci")
  expect_length(fit_ci_mglmmPQL, 3)
  expect_equal(dim(fit_ci_mglmmPQL[[1]]), c(1, 2))
  expect_true(fit_ci_mglmmPQL[[1]][, 1] < fit_ci_mglmmPQL[[1]][, 2])
  expect_true(fit_ci_mglmmPQL[[2]][, 1] < fit_ci_mglmmPQL[[2]][, 2])
  expect_true(fit_ci_mglmmPQL[[3]][, 1] < fit_ci_mglmmPQL[[3]][, 2])
  expect_equal(attr(fit_ci_mglmmPQL, "conf"), 0.8)
  expect_equal(attr(fit_ci_mglmmPQL, "method"), "boot")
  expect_equal(attr(fit_ci_mglmmPQL, "type"), "perc")
  expect_equal(attr(fit_ci_mglmmPQL, "R"), 9)
})

test_that("ci works with delta method", {
  expect_s3_class(fit_ci_delta, "cea_ci")
  expect_length(fit_ci_delta, 3)
  expect_equal(dim(fit_ci_delta[[1]]), c(1, 2))
  expect_true(fit_ci_delta[[1]][, 1] < fit_ci_delta[[1]][, 2])
  expect_true(fit_ci_delta[[2]][, 1] < fit_ci_delta[[2]][, 2])
  expect_true(fit_ci_delta[[3]][, 1] < fit_ci_delta[[3]][, 2])
  expect_equal(attr(fit_ci_delta, "conf"), 0.8)
  expect_equal(attr(fit_ci_delta, "method"), "delta")
})

test_that("ci works with factor treatments", {
  expect_s3_class(fit_ci_fct, "cea_ci")
  expect_length(fit_ci_fct, 3)
  expect_equal(dim(fit_ci_fct[[1]]), c(3, 2))
  expect_true(all(fit_ci_fct[[1]][, 1] < fit_ci_fct[[1]][, 2]))
  expect_true(all(fit_ci_fct[[2]][, 1] < fit_ci_fct[[2]][, 2]))
  expect_true(all(fit_ci_fct[[3]][, 1] < fit_ci_fct[[3]][, 2]))
  expect_equal(attr(fit_ci_fct, "conf"), 0.8)
  expect_equal(attr(fit_ci_fct, "method"), "boot")
  expect_equal(attr(fit_ci_fct, "type"), "perc")
  expect_equal(attr(fit_ci_fct, "R"), 9)

  expect_s3_class(fit_ci_fct_delta, "cea_ci")
  expect_equal(fit_ci_fct_delta$QALYs[1, ], -fit_ci_fct_delta2$QALYs[1, 2:1], ignore_attr = TRUE)
})

test_that("ci works with pooled regression analysis", {
  expect_s3_class(fit_ci_pooled, "cea_ci")
  expect_length(fit_ci_pooled, 3)
  expect_equal(dim(fit_ci_pooled[[1]]), c(3, 2))
  expect_true(all(fit_ci_pooled[[1]][, 1] < fit_ci_pooled[[1]][, 2]))
  expect_true(all(fit_ci_pooled[[2]][, 1] < fit_ci_pooled[[2]][, 2]))
  expect_true(all(fit_ci_pooled[[3]][, 1] < fit_ci_pooled[[3]][, 2]))
  expect_equal(attr(fit_ci_pooled, "conf"), 0.8)
  expect_equal(attr(fit_ci_pooled, "method"), "boot")
  expect_equal(attr(fit_ci_pooled, "sim"), "parametric")
  expect_equal(attr(fit_ci_pooled, "type"), "perc")
  expect_equal(attr(fit_ci_pooled, "R"), 9)

  expect_s3_class(fit_ci_pooled_delta, "cea_ci")
  expect_length(fit_ci_pooled_delta, 3)
  expect_equal(dim(fit_ci_pooled_delta[[1]]), c(3, 2))
  expect_true(all(fit_ci_pooled_delta[[1]][, 1] < fit_ci_pooled_delta[[1]][, 2]))
  expect_true(all(fit_ci_pooled_delta[[2]][, 1] < fit_ci_pooled_delta[[2]][, 2]))
  expect_true(all(fit_ci_pooled_delta[[3]][, 1] < fit_ci_pooled_delta[[3]][, 2]))
  expect_equal(attr(fit_ci_pooled_delta, "conf"), 0.8)
  expect_equal(attr(fit_ci_pooled_delta, "method"), "delta")
})

test_that("ci works with clustered regression analysis", {
  expect_s3_class(fit_ci_cluster, "cea_ci")
  expect_length(fit_ci_cluster, 3)
  expect_equal(dim(fit_ci_cluster[[1]]), c(1, 2))
  expect_true(all(fit_ci_cluster[[1]][, 1] < fit_ci_cluster[[1]][, 2]))
  expect_true(all(fit_ci_cluster[[2]][, 1] < fit_ci_cluster[[2]][, 2]))
  expect_true(all(fit_ci_cluster[[3]][, 1] < fit_ci_cluster[[3]][, 2]))
  expect_equal(attr(fit_ci_cluster, "conf"), 0.8)
  expect_equal(attr(fit_ci_cluster, "method"), "boot")
  expect_equal(attr(fit_ci_cluster, "sim"), "parametric")
  expect_equal(attr(fit_ci_cluster, "type"), "perc")
  expect_equal(attr(fit_ci_cluster, "R"), 9)

  expect_s3_class(fit_ci_cluster_delta, "cea_ci")
  expect_length(fit_ci_cluster_delta, 3)
  expect_equal(dim(fit_ci_cluster_delta[[1]]), c(1, 2))
  expect_true(all(fit_ci_cluster_delta[[1]][, 1] < fit_ci_cluster_delta[[1]][, 2]))
  expect_true(all(fit_ci_cluster_delta[[2]][, 1] < fit_ci_cluster_delta[[2]][, 2]))
  expect_true(all(fit_ci_cluster_delta[[3]][, 1] < fit_ci_cluster_delta[[3]][, 2]))
  expect_equal(attr(fit_ci_cluster_delta, "conf"), 0.8)
  expect_equal(attr(fit_ci_cluster_delta, "method"), "delta")
})

test_that("ci works with cea_boot objects", {
  expect_s3_class(fit_ci_boot, "cea_ci")
  expect_length(fit_ci_boot, 3)
  expect_equal(dim(fit_ci_boot[[1]]), c(1, 2))
  expect_true(fit_ci_boot[[1]][, 1] < fit_ci_boot[[1]][, 2])
  expect_true(fit_ci_boot[[2]][, 1] < fit_ci_boot[[2]][, 2])
  expect_true(fit_ci_boot[[3]][, 1] < fit_ci_boot[[3]][, 2])
  expect_equal(attr(fit_ci_boot, "conf"), 0.8)
  expect_equal(attr(fit_ci_boot, "method"), "boot")
  expect_equal(attr(fit_ci_boot, "type"), "perc")
  expect_equal(attr(fit_ci_boot, "R"), 9)
})

test_that("ci gives appropriate error messages", {
  expect_error(ci(fit_mcglm), "no applicable method")
  expect_error(ci(fit, method = "x"), class = "cea_error_unknown_method")
  expect_error(ci(fit, 1), class = "cea_error_invalid_outcome")
  expect_error(ci(fit, outcomes = c("QALYs", "costs")), class = "cea_error_unknown_outcome")
  expect_error(ci(fit), class = "cea_error_missing_wtp")
  expect_error(ci(fit, wtp = 60000, method = "boot"), class = "cea_error_missing_R")
  expect_error(ci(fit, wtp = 60000, method = "boot", R = 1, type = "stud"), class = "cea_error_invalid_ci_type")
  expect_error(ci(fit, wtp = 60000, method = "boot", R = 1, type = "all"), class = "cea_error_invalid_ci_type")
  expect_error(ci(fit, wtp = 60000, method = "boot", type = "bca", R = 1), class = "cea_error_R_too_small")
  expect_error(ci(fit, wtp = 60000, method = "boot", type = "bca", R = 39, sim = "parametric"),
               class = "cea_error_invalid_bca_parametric")

  expect_error(ci(boot_est, outcomes = c("QALYs", "costs")), class = "cea_error_unknown_outcome")
  expect_error(ci(boot_est), class = "cea_error_missing_wtp")
  expect_error(ci(boot_est, wtp = 60000, type = "stud"), class = "cea_error_invalid_ci_type")
  expect_error(ci(boot_est, wtp = 60000, type = "all"), class = "cea_error_invalid_ci_type")
  expect_error(ci(boot_est, wtp = 60000), class = "cea_error_R_too_small")
  expect_error(ci(boot_est_par, wtp = 60000, type = "bca"), class = "cea_error_invalid_bca_parametric")

  expect_error(
    ci(fit_pooled, c("QALYs", "Costs", "INMB"), conf = 0.8, wtp = 60000, R = 9, type = "perc",
       method = "boot", sim = "ordinary"),
    class = "cea_error_bootstrap_pooled"
  )
})

test_that("print.cea_ci works", {
  ci_ex <- structure(
    list(
      QALYs = matrix(c(-0.07, 0.25), ncol = 2,
                     dimnames = list("", c("Lower", "Upper"))),
      Costs = matrix(c(-2000, 5200), ncol = 2,
                     dimnames = list("", c("Lower", "Upper"))),
      INMB = matrix(c(-9700, 13000), ncol = 2,
                    dimnames = list("", c("Lower", "Upper")))
    ),
    class = "cea_ci", conf = 0.8, method = "boot", type = "bca", R = 99, sim = "ordinary"
  )
  expect_snapshot_output(ci_ex)

  ci_ex_delta <- structure(
    list(
      QALYs = matrix(c(-0.06, 0.20), ncol = 2,
                     dimnames = list("", c("Lower", "Upper"))),
      Costs = matrix(c(-340, 4500), ncol = 2,
                     dimnames = list("", c("Lower", "Upper"))),
      INMB = matrix(c(-6400, 10500), ncol = 2,
                    dimnames = list("", c("Lower", "Upper")))
    ),
    class = "cea_ci", conf = 0.8, method = "delta"
  )
  expect_snapshot_output(ci_ex_delta)

  with_sink(tempfile(), expect_equal(print(ci_ex), ci_ex))
})
