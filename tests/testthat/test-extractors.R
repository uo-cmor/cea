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

test_that("extract works with all `cea_` classes", {
  expect_equal(extract(fit, "QALYs"), setNames(0.069154392, ""))
  expect_equal(extract(fit_mglmmPQL, "QALYs"), setNames(0.069154392, ""))
  expect_equal(extract(fit_brms, "QALYs"),
               array(brms::fixef(fit_brms)[[3, 1]], c(1, 1), list(NULL, "")))
  expect_equal(extract(fit_fct, "QALYs"),
               c(ExB = 0.088581150, MT = 0.170858994, "MT + ExB" = 0.007465283))
  expect_equal(extract(fit_fct_mglmmPQL, "QALYs"),
               c(ExB = 0.088581150, MT = 0.170858994, "MT + ExB" = 0.007465283))
  expect_equal(
    extract(fit_fct_brms, "QALYs"),
    array(brms::fixef(fit_fct_brms)[3:5, 1], c(1, 3), list(NULL, c("ExB", "MT", "MTPExB")))
  )

  expect_equal(extract(fit, "Costs"), setNames(2421.241, ""), tolerance = 1e-7)
  expect_equal(extract(fit_mglmmPQL, "Costs"), setNames(2421.244, ""), tolerance = 1e-7)
  X0_brms <- X1_brms <- model.matrix(fit_brms$formula$forms[[2]]$formula, fit_brms$data)
  X0_brms[, 2] <- 0; X1_brms[, 2] <- 1
  coef_brms <- brms::fixef(fit_brms)[c(2, 6:8), 1]
  expect_equal(extract(fit_brms, "Costs"),
               setNames(mean(exp(X1_brms %*% coef_brms) - exp(X0_brms %*% coef_brms)), ""))
  expect_equal(extract(fit_fct, "Costs"),
               c(ExB = 2238.0926, MT = 2083.9103, "MT + ExB" = 1251.9401), tolerance = 1e-7)
  expect_equal(extract(fit_fct_mglmmPQL, "Costs"),
               c(ExB = 2238.0968, MT = 2083.9139, "MT + ExB" = 1251.9434), tolerance = 1e-7)
  coef_brms <- brms::fixef(fit_fct_brms)[c(2, 8:12), 1]
  X0_brms <- X1_brms <- model.matrix(fit_fct_brms$formula$forms[[2]]$formula, fit_fct_brms$data)
  X0_brms[, 2:4] <- X1_brms[, 2:4] <- 0; X1_brms[, 2] <- 1
  ExB <- mean(exp(X1_brms %*% coef_brms) - exp(X0_brms %*% coef_brms))
  X0_brms[, 2:4] <- X1_brms[, 2:4] <- 0; X1_brms[, 3] <- 1
  MT <- mean(exp(X1_brms %*% coef_brms) - exp(X0_brms %*% coef_brms))
  X0_brms[, 2:4] <- X1_brms[, 2:4] <- 0; X1_brms[, 4] <- 1
  MTPExB <- mean(exp(X1_brms %*% coef_brms) - exp(X0_brms %*% coef_brms))
  expect_equal(extract(fit_fct_brms, "Costs"), c(ExB = ExB, MT = MT, MTPExB = MTPExB))
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
  expect_equal(extract_coefs.cea_mglmmPQL(fit_mglmmPQL, 1), fit_mglmmPQL$coefficients$fixed[c(1, 3, 5, 7)])
  expect_equal(extract_coefs.cea_mglmmPQL(fit_mglmmPQL, 2), fit_mglmmPQL$coefficients$fixed[c(2, 4, 6, 8)])
  expect_equal(extract_coefs.cea_mglmmPQL(fit_mglmmPQL, 1, nm = FALSE),
               unname(fit_mglmmPQL$coefficients$fixed[c(1, 3, 5, 7)]))
})
