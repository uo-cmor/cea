test_that("ICER works as expected", {
  fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)

  expect_equal(ICER(fit), 27165.318)
})

test_that("ICER gives appropriate error messages", {
  fit_mcglm <- mcglm::mcglm(
    linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
    matrix_pred = list(mcglm::mc_id(moa2), mcglm::mc_id(moa2)),
    link = c("identity", "log"), variance = c("constant", "tweedie"),
    data = moa2
  )

  expect_error(ICER(fit_mcglm), class = "cea_error_not_cea_estimate")
})
