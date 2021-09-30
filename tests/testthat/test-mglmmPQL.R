test_that("extractlinkinv works", {
  expect_equal(
    extractlinkinv(1, data.frame(outvar = factor(1:2), eta = 2:1, w = c(0.5, 0.6)),
                   list(gaussian(), Gamma("log"))),
    data.frame(mu = 2, mu.eta.val = 1, wz = 0.5)
  )
  expect_equal(
    extractlinkinv(2, data.frame(outvar = factor(1:2), eta = 2:1, w = c(0.5, 0.6)),
                   list(gaussian(), Gamma("log"))),
    data.frame(mu = exp(1), mu.eta.val = exp(1), wz = 0.6)
  )
})

test_that("mergeFormula works", {
  expect_equal(mergeFormula(list(QALYs = value ~ tx + age, Costs = value ~ tx + age)),
               value ~ tx:outvar + age:outvar + tx:outvar + age:outvar + outvar - 1)
  expect_error(mergeFormula(list(QALYs = QALYs ~ tx + age, Costs = Cost ~ tx + age)))
})
