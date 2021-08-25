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
