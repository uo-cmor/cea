test_that("ICER works as expected", {
  expect_equal(ICER(fit), 27165.318)
})

test_that("ICER gives appropriate error messages", {
  expect_error(ICER(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(ICER(fit_lp), class = "cea_error_not_formula_spec")
})
