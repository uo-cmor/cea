test_that("tidy.cea_mcglm works", {
  td <- tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_equal(dim(td), c(11, 7))
  expect_equal(td$component, rep(c("regression", "correlation", "dispersion"), c(8, 1, 2)))
  expect_equal(td$y.level, c(rep(c("QALYs", "Costs"), each = 4), NA, "QALYs", "Costs"))
  expect_equal(td$term,
               c(rep(c("(Intercept)", "booster", "age", "sex"), 2), "rho12", "tau11", "tau21"))
  expect_equal(td$estimate, coef(fit)$Estimates)
  expect_equal(td$std.error, unname(sqrt(diag(as.matrix(vcov(fit))))))
  expect_equal(td$statistic, td$estimate / td$std.error)
  expect_equal(td$p.value, 2 * (1 - stats::pnorm(abs(td$statistic))))

  td_fct <- tidy(fit_fct)
  expect_s3_class(td_fct, "tbl_df")
  expect_equal(dim(td_fct), c(15, 7))
  expect_equal(td_fct$component, rep(c("regression", "correlation", "dispersion"), c(12, 1, 2)))
  expect_equal(td_fct$y.level, c(rep(c("QALYs", "Costs"), each = 6), NA, "QALYs", "Costs"))
  expect_equal(
    td_fct$term,
    c(rep(c("(Intercept)", "txExB", "txMT", "txMT + ExB", "age", "sex"), 2),
      "rho12", "tau11", "tau21")
  )
  expect_equal(td_fct$estimate, coef(fit_fct)$Estimates)
  expect_equal(td_fct$std.error, unname(sqrt(diag(as.matrix(vcov(fit_fct))))))
  expect_equal(td_fct$statistic, td_fct$estimate / td_fct$std.error)
  expect_equal(td_fct$p.value, 2 * (1 - stats::pnorm(abs(td_fct$statistic))))
})

test_that("tidy.cea_mglmmPQL works", {
  td <- tidy(fit_mglmmPQL)
  expect_s3_class(td, "tbl_df")
  expect_equal(dim(td), c(8, 8))
  expect_equal(td$component, rep("regression", 8))
  expect_equal(td$y.level, rep(c("QALYs", "Costs"), each = 4))
  expect_equal(td$term, rep(c("(Intercept)", "booster", "age", "sex"), 2))
  expect_equal(td$estimate,
               unlist(coef(fit_mglmmPQL), use.names = FALSE)[c(1, 3, 5, 7, 2, 4, 6, 8)])
  expect_equal(td$std.error,
               unname(sqrt(diag(as.matrix(vcov(fit_mglmmPQL)))))[c(1, 3, 5, 7, 2, 4, 6, 8)])
  expect_equal(td$df, unname(fit_mglmmPQL$fixDF$X)[c(1, 3, 5, 7, 2, 4, 6, 8)])
  expect_equal(td$statistic, td$estimate / td$std.error)
  expect_equal(td$p.value, 2 * (1 - stats::pt(abs(td$statistic), td$df)))

  td_fct <- tidy(fit_fct_mglmmPQL)
  expect_s3_class(td_fct, "tbl_df")
  expect_equal(dim(td_fct), c(12, 8))
  expect_equal(td_fct$component, rep("regression", 12))
  expect_equal(td_fct$y.level, rep(c("QALYs", "Costs"), each = 6))
  expect_equal(td_fct$term, rep(c("(Intercept)", "txExB", "txMT", "txMT + ExB", "age", "sex"), 2))
  expect_equal(td_fct$estimate,
               unlist(coef(fit_fct_mglmmPQL), use.names = FALSE)[c(1, 3:5, 9, 11, 2, 6:8, 10, 12)])
  expect_equal(
    td_fct$std.error,
    unname(sqrt(diag(as.matrix(vcov(fit_fct_mglmmPQL)))))[c(1, 3:5, 9, 11, 2, 6:8, 10, 12)]
  )
  expect_equal(td_fct$df, unname(fit_fct_mglmmPQL$fixDF$X)[c(1, 3:5, 9, 11, 2, 6:8, 10, 12)])
  expect_equal(td_fct$statistic, td_fct$estimate / td_fct$std.error)
  expect_equal(td_fct$p.value, 2 * (1 - stats::pt(abs(td_fct$statistic), td_fct$df)))
})

test_that("tidy.cea_mcglm_pooled works", {
  td <- tidy(fit_pooled)
  expect_s3_class(td, "tbl_df")
  expect_equal(dim(td), c(15, 7))
  expect_equal(td$component, rep(c("regression", "correlation", "dispersion"), c(12, 1, 2)))
  expect_equal(td$y.level, c(rep(c("QALYs", "Costs"), each = 6), NA, "QALYs", "Costs"))
  expect_equal(
    td$term,
    c(rep(c("(Intercept)", "txExB", "txMT", "txMT + ExB", "age", "sex"), 2),
      "rho12", "tau11", "tau21")
  )
  expect_equal(td$estimate, coef(fit_pooled))
  expect_equal(td$std.error, unname(sqrt(diag(vcov(fit_pooled)))))
  expect_equal(td$statistic, td$estimate / td$std.error)
  expect_equal(td$p.value, 2 * (1 - stats::pnorm(abs(td$statistic))))
})
