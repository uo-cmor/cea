test_that("QALYs works as expected", {
  expect_equal(QALYs(fit), 0.074458875)
})

test_that("QALYs gives appropriate error messages", {
  expect_error(QALYs(fit_mcglm), class = "cea_error_not_cea_estimate")
})

test_that("Costs works as expected", {
  expect_equal(Costs(fit), 2022.699)
  expect_equal(Costs(fit, "ATT"), 2024.0034)
  expect_equal(Costs(fit, "ATC"), 2021.39456)
})

test_that("Costs gives appropriate error messages", {
  expect_error(Costs(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(Costs(fit, "ABC"), class = "cea_error_unknown_estimand")
})

test_that("ICER works as expected", {
  expect_equal(ICER(fit), 27165.318)
})

test_that("ICER gives appropriate error messages", {
  expect_error(ICER(fit_mcglm), class = "cea_error_not_cea_estimate")
})

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

test_that("extract gives appropriate error messages", {
  expect_error(extract(fit, "X"), class = "cea_error_unknown_outcome")
  attr(fit, "tx") <- "X"
  expect_error(extract(fit, "QALYs"), class = "cea_error_unknown_treatment")
})
