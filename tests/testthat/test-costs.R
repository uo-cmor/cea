test_that("Costs works as expected", {
  expect_equal(Costs(fit), 2022.699)
  expect_equal(Costs(fit, "ATT"), 2024.0034)
  expect_equal(Costs(fit, "ATC"), 2021.39456)
})

test_that("Costs gives appropriate error messages", {
  expect_error(Costs(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(Costs(fit, "ABC"), class = "cea_error_unknown_estimand")
})
