test_that("QALYs works as expected", {
  expect_equal(QALYs(fit), 0.074458875)
})

test_that("QALYs gives appropriate error messages", {
  expect_error(QALYs(fit_mcglm), class = "cea_error_not_cea_estimate")
})
