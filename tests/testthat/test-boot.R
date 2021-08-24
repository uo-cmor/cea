test_that("boot works as expected", {
  boot_est <- boot(fit, R = 9)
  res <- cea_extract_estimate(fit)
  t0 <- c(QALYs = res$QALYs$effect, Costs = res$Costs$effect)

  expect_s3_class(boot_est, "cea_boot")
  expect_equal(boot_est$t0, t0)
  expect_equal(dim(boot_est$t), c(9, 2))
  expect_equal(boot_est$R, 9)
  expect_equal(boot_est$sim, "ordinary")
  expect_equal(boot_est$stype, "i")
})
