fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)
fit_mcglm <- mcglm::mcglm(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  matrix_pred = list(mcglm::mc_id(moa2), mcglm::mc_id(moa2)),
  link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2
)

test_that("INMB works as expected", {
  expect_equal(INMB(fit, 60000), 2444.8335)
})

test_that("INMB gives appropriate error messages", {
  expect_error(INMB(fit_mcglm, 60000), class = "cea_error_not_cea_estimate")
})

test_that("INHB works as expected", {
  expect_equal(INHB(fit, 60000), 0.040747225)
})

test_that("INHB gives appropriate error messages", {
  expect_error(INHB(fit_mcglm, 60000), class = "cea_error_not_cea_estimate")
})
