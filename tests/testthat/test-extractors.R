test_that("QALYs works as expected", {
  expect_equal(QALYs(fit), setNames(0.06957871, ""))
  expect_equal(QALYs(fit_fct), c(ExB = 0.088299212, MT = 0.170677168, "MT + ExB" = 0.007374031))
  expect_equal(QALYs(fit_fct2), (c(Ex = 0, QALYs(fit_fct)) - QALYs(fit_fct)[[1]])[-2])
})

test_that("QALYs gives appropriate error messages", {
  expect_error(QALYs(fit_mcglm), class = "cea_error_incorrect_class")
})

test_that("Costs works as expected", {
  expect_equal(Costs(fit), setNames(2088.4017, ""), tolerance = 1e-7)
  expect_equal(Costs(fit, "ATT"), setNames(2071.1476, ""), tolerance = 1e-7)
  expect_equal(Costs(fit, "ATC"), setNames(2105.6559, ""), tolerance = 1e-7)
  expect_equal(Costs(fit_fct), c(ExB = 1875.4691, MT = 1833.7388, "MT + ExB" = 1093.5596),
               tolerance = 1e-7)
  expect_equal(Costs(fit_fct2), (c(Ex = 0, Costs(fit_fct)) - Costs(fit_fct)[[1]])[-2])
})

test_that("Costs gives appropriate error messages", {
  expect_error(Costs(fit_mcglm), class = "cea_error_incorrect_class")
  expect_error(Costs(fit, "ABC"), class = "cea_error_unknown_estimand")
})

test_that("ICER works as expected", {
  expect_equal(ICER(fit), setNames(30014.953, ""), tolerance = 1e-7)
})

test_that("ICER gives appropriate error messages", {
  expect_error(ICER(fit_mcglm), class = "cea_error_incorrect_class")
})

test_that("INMB works as expected", {
  expect_equal(INMB(fit, 60000), setNames(2086.321, ""), tolerance = 1e-7)
})

test_that("INMB gives appropriate error messages", {
  expect_error(INMB(fit_mcglm, 60000), class = "cea_error_incorrect_class")
})

test_that("INHB works as expected", {
  expect_equal(INHB(fit, 60000), setNames(0.034772015, ""), tolerance = 1e-7)
})

test_that("INHB gives appropriate error messages", {
  expect_error(INHB(fit_mcglm, 60000), class = "cea_error_incorrect_class")
})

test_that("extract gives appropriate error messages", {
  expect_error(extract(fit, "X"), class = "cea_error_unknown_outcome")
  attr(fit, "tx") <- "X"
  expect_error(extract(fit, "QALYs"), class = "cea_error_unknown_treatment")
})
