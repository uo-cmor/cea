test_that("extract_dmu works as expected", {
  expect_equal(
    extract_dmu(fit, "Costs"),
    setNames(
      list(matrix(c(0, 0, 0, 0, 2421.241, 4930.886, 161052.964, 1250.661), nrow = 1)),
      ""
    ),
    tolerance = 1e-7
  )
  expect_equal(extract_dmu(fit, "QALYs"), setNames(list(matrix(c(0, 1, rep(0, 6)), nrow = 1)), ""))
  expect_equal(
    extract_dmu(fit, "Costs", "ATT"),
    setNames(list(matrix(c(0, 0, 0, 0, 2412.042, 4912.153, 160941.587, 1481.698), nrow = 1)), ""),
    tolerance = 1e-7
  )
  expect_equal(
    extract_dmu(fit, "Costs", "ATC"),
    setNames(list(matrix(c(0, 0, 0, 0, 2430.44, 4949.62, 161164.34, 1019.62), nrow = 1)), ""),
    tolerance = 1e-7
  )
  expect_equal(
    extract_dmu(fit_fct, "QALYs"),
    list(ExB = matrix(c(0, 1, rep(0, 10)), nrow = 1),
         MT = matrix(c(0, 0, 1, rep(0, 9)), nrow = 1),
         "MT + ExB" = matrix(c(0, 0, 0, 1, rep(0, 8)), nrow = 1))
  )
})

test_that("extract_dmu gives appropriate errors", {
  expect_error(extract_dmu(fit_mcglm), class = "cea_error_incorrect_class")
  expect_error(extract_dmu(fit, "X"), class = "cea_error_unknown_outcome")
  expect_error(extract_dmu(fit, "QALYs", "ABC"), class = "cea_error_unknown_estimand")
  attr(fit, "tx") <- "X"
  expect_error(extract_dmu(fit, "QALYs"), class = "cea_error_unknown_treatment")
})
