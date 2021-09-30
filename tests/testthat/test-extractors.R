test_that("QALYs works as expected", {
  expect_equal(QALYs(fit), setNames(0.069154392, ""))
  expect_equal(QALYs(fit_fct), c(ExB = 0.088581150, MT = 0.170858994, "MT + ExB" = 0.007465283))
  expect_equal(QALYs(fit_fct2), (c(Ex = 0, QALYs(fit_fct)) - QALYs(fit_fct)[[1]])[-2])
})

test_that("QALYs gives appropriate error messages", {
  expect_error(QALYs(fit_mcglm), class = "cea_error_incorrect_class")
})

test_that("Costs works as expected", {
  expect_equal(Costs(fit), setNames(2421.241, ""), tolerance = 1e-7)
  expect_equal(Costs(fit, "ATT"), setNames(2412.0424, ""), tolerance = 1e-7)
  expect_equal(Costs(fit, "ATC"), setNames(2430.4396, ""), tolerance = 1e-7)
  expect_equal(Costs(fit_fct), c(ExB = 2238.0926, MT = 2083.9103, "MT + ExB" = 1251.9401),
               tolerance = 1e-7)
  expect_equal(Costs(fit_fct2), (c(Ex = 0, Costs(fit_fct)) - Costs(fit_fct)[[1]])[-2])
})

test_that("Costs gives appropriate error messages", {
  expect_error(Costs(fit_mcglm), class = "cea_error_incorrect_class")
  expect_error(Costs(fit, "ABC"), class = "cea_error_unknown_estimand")
})

test_that("ICER works as expected", {
  expect_equal(ICER(fit), setNames(35012.11, ""), tolerance = 1e-7)
})

test_that("ICER gives appropriate error messages", {
  expect_error(ICER(fit_mcglm), class = "cea_error_incorrect_class")
})

test_that("INMB works as expected", {
  expect_equal(INMB(fit, 60000), setNames(1728.0225, ""), tolerance = 1e-7)
})

test_that("INMB gives appropriate error messages", {
  expect_error(INMB(fit_mcglm, 60000), class = "cea_error_incorrect_class")
})

test_that("INHB works as expected", {
  expect_equal(INHB(fit, 60000), setNames(0.028800375, ""), tolerance = 1e-7)
})

test_that("INHB gives appropriate error messages", {
  expect_error(INHB(fit_mcglm, 60000), class = "cea_error_incorrect_class")
})

test_that("extract gives appropriate error messages", {
  expect_error(extract(fit, "X"), class = "cea_error_unknown_outcome")
  attr(fit, "tx") <- "X"
  expect_error(extract(fit, "QALYs"), class = "cea_error_unknown_treatment")
})

test_that("extract_coefs works", {
  expect_equal(
    extract_coefs.cea_mcglm(fit, "reg", TRUE),
    setNames(
      fit$Regression,
      paste0(rep(c("QALYs", "Costs"), each = 4), ".",
             rep(c("(Intercept)", "booster", "age", "sex")))
    )
  )
  expect_equal(extract_coefs.cea_mcglm(fit, 1, TRUE),
               setNames(fit$Regression[1:4], c("(Intercept)", "booster", "age", "sex")))
  expect_equal(extract_coefs.cea_mglmmPQL(fit_mglmmPQL), fit_mglmmPQL$coefficients$fixed)
  expect_equal(extract_coefs.cea_mglmmPQL(fit_mglmmPQL, nm = FALSE),
               unname(fit_mglmmPQL$coefficients$fixed))
})
