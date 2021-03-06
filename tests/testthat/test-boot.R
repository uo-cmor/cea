t0 <- c(QALYs = QALYs(fit), Costs = Costs(fit))
t0_fct <- cbind(QALYs = QALYs(fit_fct), Costs = Costs(fit_fct))
t0_pooled <- cbind(QALYs = QALYs(fit_pooled), Costs = Costs(fit_pooled))
t0_cluster <- c(QALYs = QALYs(fit_cluster), Costs = Costs(fit_cluster))

test_that("ordinary boot works as expected", {
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

  expect_s3_class(boot_est_cluster_par, "cea_boot")
  expect_equal(boot_est_cluster_par$t0, t0_cluster)
  expect_equal(dim(boot_est_cluster_par$t), c(9, 2))
  expect_equal(boot_est_cluster_par$R, 9)
  expect_equal(boot_est_cluster_par$sim, "parametric")
})

test_that("boot works with factor treatment variable", {
  expect_s3_class(boot_est_fct, "cea_boot")
  expect_equal(boot_est_fct$t0, t0_fct)
  expect_equal(dim(boot_est_fct$t), c(9, 6))
  expect_equal(boot_est_fct$R, 9)
  expect_equal(boot_est_fct$sim, "parametric")
  expect_equal(attr(boot_est_fct, "tx"), c("ExB", "MT", "MT + ExB"))
  expect_s3_class(boot_est_fct2, "cea_boot")
  expect_equal(attr(boot_est_fct2, "tx"), c("Ex", "MT", "MT + ExB"))
})

test_that("boot works with pooled regression analysis", {
  expect_s3_class(boot_pooled, "cea_boot")
  expect_equal(boot_pooled$t0, t0_pooled)
  expect_equal(dim(boot_pooled$t), c(9, 6))
  expect_equal(boot_pooled$R, 9)
  expect_equal(boot_pooled$sim, "parametric")
  expect_equal(attr(boot_pooled, "tx"), c("ExB", "MT", "MT + ExB"))
})

test_that("boot gives appropriate error messages", {
  expect_error(boot_cea(fit_mcglm, R = 9), class = "cea_error_incorrect_class")
  expect_error(boot_cea(fit, sim = "antithetic"), class = "cea_error_unknown_sim")
  expect_error(boot_cea(fit, sim = "abc"), class = "cea_error_unknown_sim")
  expect_error(boot_cea(fit_pooled, sim = "ordinary"), class = "cea_error_bootstrap_pooled")
  expect_error(boot_cea(fit_cluster, R = 9, sim = "ordinary"),
               class = "cea_error_bootstrap_cluster")
  expect_error(boot_cea(fit_centre, R = 9, sim = "ordinary"), class = "cea_error_bootstrap_cluster")
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

  plt <- autoplot(boot_est_fct)
  expect_s3_class(plt, "gg")
  expect_length(plt$layers, 3)
  expect_s3_class(plt$layers[[1]]$geom, "GeomHline")
  expect_s3_class(plt$layers[[2]]$geom, "GeomVline")
  expect_s3_class(plt$layers[[3]]$geom, "GeomPoint")
  expect_mapequal(
    plt$labels,
    list(y = "Incremental Costs", x = "Incremental QALYs", colour = ".tx", shape = ".tx",
         yintercept = "yintercept", xintercept = "xintercept")
  )
})

test_that("plot.cea_boot works as expected", {
  plt <- with_null_pdf(plot(boot_est))
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

