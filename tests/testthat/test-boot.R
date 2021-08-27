t0 <- c(QALYs = QALYs(fit), Costs = Costs(fit))

test_that("boot works as expected", {
  expect_s3_class(boot_est, "cea_boot")
  expect_equal(boot_est$t0, t0)
  expect_equal(dim(boot_est$t), c(9, 2))
  expect_equal(boot_est$R, 9)
  expect_equal(boot_est$sim, "ordinary")
  expect_equal(boot_est$stype, "i")
})

test_that("parametric boot works as expected", {
  expect_s3_class(boot_est_par, "cea_boot")
  expect_equal(boot_est_par$t0, t0)
  expect_equal(dim(boot_est_par$t), c(9, 2))
  expect_equal(boot_est_par$R, 9)
  expect_equal(boot_est_par$sim, "parametric")
})


test_that("boot gives appropriate error messages", {
  expect_error(boot(fit_mcglm, R = 9), class = "cea_error_not_cea_estimate")
  expect_error(boot(fit, sim = "antithetic"), class = "cea_error_unknown_sim")
  expect_error(boot(fit, sim = "abc"), class = "cea_error_unknown_sim")
})

test_that("autoplot.cea_boot works as expected", {
  plt <- autoplot(boot_est)
  expect_s3_class(plt, "gg")
  expect_equal(unname(as.matrix(plt$data)), boot_est$t)
  expect_length(plt$layers, 4)
  expect_s3_class(plt$layers[[1]]$geom, "GeomHline")
  expect_s3_class(plt$layers[[2]]$geom, "GeomVline")
  expect_s3_class(plt$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[4]]$geom, "GeomPoint")
  expect_mapequal(
    plt$labels,
    list(y = "Incremental Costs", x = "Incremental QALYs", yintercept = "yintercept",
         xintercept = "xintercept")
  )
  plt <- autoplot(boot_est, wtp = 60000)
  expect_s3_class(plt, "gg")
  expect_length(plt$layers, 5)
  expect_s3_class(plt$layers[[1]]$geom, "GeomHline")
  expect_s3_class(plt$layers[[2]]$geom, "GeomVline")
  expect_s3_class(plt$layers[[3]]$geom, "GeomAbline")
  expect_s3_class(plt$layers[[4]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[5]]$geom, "GeomPoint")
  expect_error(autoplot(boot_est, wtp = 60000, QALYs = "X"), class = "cea_error_unknown_outcome")
})

test_that("plot.cea_boot works as expected", {
  plt <- with_sink(tempfile(), plot(boot_est))
  expect_s3_class(plt, "gg")
  expect_equal(unname(as.matrix(plt$data)), boot_est$t)
  expect_length(plt$layers, 4)
  expect_s3_class(plt$layers[[1]]$geom, "GeomHline")
  expect_s3_class(plt$layers[[2]]$geom, "GeomVline")
  expect_s3_class(plt$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(plt$layers[[4]]$geom, "GeomPoint")
  expect_mapequal(
    plt$labels,
    list(y = "Incremental Costs", x = "Incremental QALYs", yintercept = "yintercept",
         xintercept = "xintercept")
  )
})
