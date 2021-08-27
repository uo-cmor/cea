test_that("extract_dmu works as expected", {
  expect_equal(
    extract_dmu(fit, "Costs"),
    matrix(c(0, 0, 0, 0, c(2022.699, 4685.837, 134611.481, 1087.425, 0, 0, 0)),
           nrow = 1)
  )
  expect_equal(extract_dmu(fit, "QALYs"), matrix(c(0, 1, rep(0, 9)), nrow = 1))
  expect_equal(
    extract_dmu(fit, "Costs", "ATT"),
    matrix(c(0, 0, 0, 0, c(2024.003, 4688.859, 135601.075, 1198.564, 0, 0, 0)),
           nrow = 1)
  )
  expect_equal(
    extract_dmu(fit, "Costs", "ATC"),
    matrix(c(0, 0, 0, 0, c(2021.395, 4682.815, 133621.888, 976.286, 0, 0, 0)),
           nrow = 1)
  )
})

test_that("extract_dmu gives appropriate errors", {
  expect_error(extract_dmu(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(extract_dmu(fit, "X"), class = "cea_error_unknown_outcome")
  expect_error(extract_dmu(fit, "QALYs", "ABC"), class = "cea_error_unknown_estimand")
  attr(fit, "tx") <- "X"
  expect_error(extract_dmu(fit, "QALYs"), class = "cea_error_unknown_treatment")
})
