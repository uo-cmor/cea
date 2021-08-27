test_that("extract_dmu works as expected", {
  expect_equal(
    extract_dmu(fit, "Costs"),
    setNames(
      list(matrix(c(0, 0, 0, 0, 2088.402, 4728.016, 137860.09, 1069.510, 0, 0, 0), nrow = 1)),
      ""
    ),
    tolerance = 1e-7
  )
  expect_equal(extract_dmu(fit, "QALYs"), setNames(list(matrix(c(0, 1, rep(0, 9)), nrow = 1)), ""))
  expect_equal(
    extract_dmu(fit, "Costs", "ATT"),
    setNames(list(matrix(c(0, 0, 0, 0, 2071.148, 4688.953, 137076.73, 1239.201, 0, 0, 0), nrow = 1)), ""),
    tolerance = 1e-7
  )
  expect_equal(
    extract_dmu(fit, "Costs", "ATC"),
    setNames(list(matrix(c(0, 0, 0, 0, 2105.656, 4767.078, 138643.45, 899.8192, 0, 0, 0), nrow = 1)), ""),
    tolerance = 1e-7
  )
  expect_equal(
    extract_dmu(fit_fct, "QALYs"),
    list(ExB = matrix(c(0, 1, rep(0, 13)), nrow = 1),
         MT = matrix(c(0, 0, 1, rep(0, 12)), nrow = 1),
         "MT + ExB" = matrix(c(0, 0, 0, 1, rep(0, 11)), nrow = 1))
  )
})

test_that("extract_dmu gives appropriate errors", {
  expect_error(extract_dmu(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(extract_dmu(fit, "X"), class = "cea_error_unknown_outcome")
  expect_error(extract_dmu(fit, "QALYs", "ABC"), class = "cea_error_unknown_estimand")
  attr(fit, "tx") <- "X"
  expect_error(extract_dmu(fit, "QALYs"), class = "cea_error_unknown_treatment")
})
