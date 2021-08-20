fit_mcglm <- mcglm::mcglm(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  matrix_pred = list(mcglm::mc_id(moa2), mcglm::mc_id(moa2)),
  link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2
)

test_that("estimate works with default specification", {
  fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit_mcglm, ignore_attr = c("class", "spec"), ignore_formula_env = TRUE)
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
  fit <- estimate(
    linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
    link = c("identity", "log"), variance = c("constant", "tweedie"),
    data = moa2
  )

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit_mcglm, ignore_attr = c("class", "spec"), ignore_formula_env = TRUE)
})

test_that("estimate works with list data", {
  fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = as.list(moa2))

  expect_s3_class(fit, "cea_estimate")
  expect_s3_class(fit, "mcglm")
  expect_equal(fit, fit_mcglm, ignore_attr = c("class", "spec"), ignore_formula_env = TRUE)
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
  expect_equal(fit, fit2, ignore_attr = c("class", "spec"), ignore_formula_env = TRUE)
})

test_that("print.cea_estimate works", {
  fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)
  expect_snapshot_output(fit)
  class(fit) <- class(fit)[-1]
  expect_output(print(fit))
})
