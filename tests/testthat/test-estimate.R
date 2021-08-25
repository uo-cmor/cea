test_that("estimate works with default specification", {
  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit_mcglm, ignore_attr = c("class", "spec", "call"), ignore_formula_env = TRUE)
})

test_that("estimate gives appropriate messages", {
  expect_error(estimate(1, "Cost", "booster", c("age", "sex"), data = moa2),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", 2, "booster", c("age", "sex"), data = moa2),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", "Cost", 3, c("age", "sex"), data = moa2),
               class = "cea_error_not_string")
  expect_error(estimate("QALYs", "Cost", "booster", 4, data = moa2),
               class = "cea_error_not_character")
  expect_error(estimate("qalys", "Cost", "booster", c("age", "sex"), data = moa2),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "costs", "booster", c("age", "sex"), data = moa2),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2),
               class = "cea_error_variable_not_found")
  expect_error(estimate("QALYs", "Cost", "booster", c("age", "gender"), data = moa2),
               class = "cea_error_variable_not_found")
  expect_warning(
    estimate("QALYs", "Cost", "tx", linear_pred = list(Cost ~ booster + age + sex), data = moa2),
    class = "cea_warning_formula_override"
  )
})

test_that("estimate works with custom `linear_pred`", {
  expect_s3_class(fit_lp, "mcglm")
  expect_equal(fit_lp, fit_mcglm, ignore_formula_env = TRUE)
})

test_that("estimate works with list data", {
  moa2 <- as.list(moa2)
  fit_list <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit_list, fit, ignore_formula_env = TRUE)
})

test_that("estimate works with missing covars", {
  fit <- estimate("QALYs", "Cost", "booster", data = moa2)
  fit2 <- mcglm::mcglm(
    linear_pred = c(QALYs = QALYs ~ booster, Costs = Cost ~ booster),
    matrix_pred = list(mcglm::mc_id(moa2), mcglm::mc_id(moa2)),
    link = c("identity", "log"), variance = c("constant", "tweedie"),
    data = moa2
  )

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit2, ignore_attr = c("class", "spec", "call"), ignore_formula_env = TRUE)
})

test_that("print.cea_estimate works", {
  expect_snapshot_output(fit)
  expect_equal(print(fit), fit)
  attr(fit, "spec") <- "linear_pred"
  expect_output(print(fit))
})
