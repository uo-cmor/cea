fit_ceac <- ceac(fit, R = 9, wtp_max = 100000, wtp_step = 10000, method = "boot", sim = "ordinary")
fit_ceac_fct <- ceac(fit_fct, R = 9, wtp_max = 100000, wtp_step = 10000, method = "boot",
                     sim = "ordinary")
fit_ceac_delta <- ceac(fit, wtp_max = 100000, wtp_step = 10000, method = "delta")
fit_ceac_boot <- ceac(boot_est, wtp_max = 100000, wtp_step = 10000)
fit_ceac_boot_fct <- ceac(boot_est_fct, wtp_max = 100000, wtp_step = 10000)
fit_ceac_boot_fct2 <- ceac(boot_est_fct2, wtp_max = 100000, wtp_step = 10000)
fit_ceac_pooled <- ceac(fit_pooled, R = 9, wtp_max = 100000, wtp_step = 10000, method = "boot",
                        sim = "parametric")
fit_ceac_pooled_delta <- ceac(fit_pooled, wtp_max = 100000, wtp_step = 10000, method = "delta")
fit_ceac_cluster <- ceac(fit_cluster, R = 9, wtp_max = 100000, wtp_step = 10000, method = "boot",
                         sim = "parametric")
fit_ceac_cluster_delta <- ceac(fit_cluster, wtp_max = 100000, wtp_step = 10000, method = "delta")

test_that("ceac works with cea_estimate objects", {
  expect_s3_class(fit_ceac, "cea_ceac")
  expect_equal(dim(fit_ceac), c(11, 2))
  expect_equal(fit_ceac$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac$ceac >= 0 & fit_ceac$ceac <= 1))
  expect_equal(attr(fit_ceac, "method"), "boot")
  expect_equal(attr(fit_ceac, "R"), 9)
  expect_equal(attr(fit_ceac, "sim"), "ordinary")
})

test_that("ceac works with factor treatment variable", {
  expect_s3_class(fit_ceac_fct, "cea_ceac")
  expect_equal(dim(fit_ceac_fct), c(33, 3))
  expect_equal(fit_ceac_fct$wtp, rep(seq.int(0, 100000, 10000), 3))
  expect_true(all(fit_ceac_fct$ceac >= 0 & fit_ceac$ceac <= 1))
  expect_equal(fit_ceac_fct$tx, rep(c("ExB", "MT", "MT + ExB"), each = 11))
  expect_equal(attr(fit_ceac_fct, "method"), "boot")
  expect_equal(attr(fit_ceac_fct, "R"), 9)
  expect_equal(attr(fit_ceac_fct, "sim"), "ordinary")
})

test_that("ceac works with delta method", {
  expect_s3_class(fit_ceac_delta, "cea_ceac")
  expect_equal(dim(fit_ceac_delta), c(11, 2))
  expect_equal(fit_ceac_delta$wtp, seq.int(0, 100000, 10000))
  expect_equal(
    fit_ceac_delta$ceac,
    c(0.08997622, 0.20655243, 0.35534809, 0.46222819, 0.52996406, 0.57395541, 0.60402827,
      0.62560631, 0.64172976, 0.65418307, 0.66406562),
    tolerance = 1e-7
  )
  expect_equal(attr(fit_ceac_delta, "method"), "delta")
})

test_that("ceac works with pooled regression analysis", {
  expect_s3_class(fit_ceac_pooled, "cea_ceac")
  expect_equal(dim(fit_ceac_pooled), c(33, 3))
  expect_equal(fit_ceac_pooled$wtp, rep(seq.int(0, 100000, 10000), 3))
  expect_true(all(fit_ceac_pooled$ceac >= 0 & fit_ceac_pooled$ceac <= 1))
  expect_equal(fit_ceac_pooled$tx, rep(c("ExB", "MT", "MT + ExB"), each = 11))
  expect_equal(attr(fit_ceac_pooled, "method"), "boot")
  expect_equal(attr(fit_ceac_pooled, "R"), 9)
  expect_equal(attr(fit_ceac_pooled, "sim"), "parametric")

  expect_s3_class(fit_ceac_pooled_delta, "cea_ceac")
  expect_equal(dim(fit_ceac_pooled_delta), c(33, 3))
  expect_equal(fit_ceac_pooled_delta$wtp, rep(seq.int(0, 100000, 10000), 3))
  expect_equal(
    fit_ceac_pooled_delta$ceac,
    c(0.08723686, 0.23922318, 0.42849627, 0.54837912, 0.61699348, 0.65870206, 0.68600755,
      0.70502877, 0.71894370, 0.72952288, 0.73781680, 0.10434818, 0.42267776, 0.69456828,
      0.80774027, 0.85705881, 0.88242301, 0.89731937, 0.90694532, 0.91361231, 0.91847512,
      0.92216566, 0.18356658, 0.24334959, 0.32531814, 0.37891944, 0.41213290, 0.43390500,
      0.44906075, 0.46014501, 0.46857505, 0.47518925, 0.48051082),
    tolerance = 1e-7
  )
  expect_equal(fit_ceac_pooled_delta$tx, rep(c("ExB", "MT", "MT + ExB"), each = 11))
  expect_equal(attr(fit_ceac_pooled_delta, "method"), "delta")
})

test_that("ceac works with clustered regression analysis", {
  expect_s3_class(fit_ceac_cluster, "cea_ceac")
  expect_equal(dim(fit_ceac_cluster), c(11, 2))
  expect_equal(fit_ceac_cluster$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac_cluster$ceac >= 0 & fit_ceac_cluster$ceac <= 1))
  expect_equal(attr(fit_ceac_cluster, "method"), "boot")
  expect_equal(attr(fit_ceac_cluster, "R"), 9)
  expect_equal(attr(fit_ceac_cluster, "sim"), "parametric")

  expect_s3_class(fit_ceac_cluster_delta, "cea_ceac")
  expect_equal(dim(fit_ceac_cluster_delta), c(11, 2))
  expect_equal(fit_ceac_cluster_delta$wtp, seq.int(0, 100000, 10000))
  expect_equal(
    fit_ceac_cluster_delta$ceac,
    c(0.08997452, 0.20321651, 0.35308347, 0.46168612, 0.53032672, 0.57470840, 0.60493097,
      0.62655009, 0.64266554, 0.65508909, 0.66493302),
    tolerance = 1e-7
  )
  expect_equal(attr(fit_ceac_cluster_delta, "method"), "delta")
})

test_that("ceac works with cea_boot objects", {
  expect_s3_class(fit_ceac_boot, "cea_ceac")
  expect_equal(dim(fit_ceac_boot), c(11, 2))
  expect_equal(fit_ceac_boot$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac_boot$ceac >= 0 & fit_ceac_boot$ceac <= 1))
  expect_equal(attr(fit_ceac_boot, "method"), "boot")
  expect_equal(attr(fit_ceac_boot, "R"), 9)
  expect_equal(attr(fit_ceac_boot, "sim"), "ordinary")

  expect_s3_class(fit_ceac_boot_fct, "cea_ceac")
  expect_equal(dim(fit_ceac_boot_fct), c(33, 3))
  expect_equal(fit_ceac_boot_fct$wtp, rep(seq.int(0, 100000, 10000), 3))
  expect_true(all(fit_ceac_boot_fct$ceac >= 0 & fit_ceac$ceac <= 1))
  expect_equal(fit_ceac_boot_fct$tx, rep(c("ExB", "MT", "MT + ExB"), each = 11))
  expect_equal(attr(fit_ceac_boot_fct, "method"), "boot")
  expect_equal(attr(fit_ceac_boot_fct, "R"), 9)
  expect_equal(attr(fit_ceac_boot_fct, "sim"), "parametric")

  expect_s3_class(fit_ceac_boot_fct2, "cea_ceac")
  expect_equal(fit_ceac_boot_fct2$tx, rep(c("Ex", "MT", "MT + ExB"), each = 11))
})

test_that("ceac gives appropriate errors", {
  expect_error(ceac(fit_mcglm), "no applicable method")
  expect_error(ceac(fit, method = "x"), class = "cea_error_unknown_method")
  expect_error(ceac(fit, method = "boot"), class = "cea_error_missing_R")
  expect_error(ceac(fit, R = 9, QALYs = "X"), class = "cea_error_unknown_outcome")
  expect_error(ceac(boot_est, QALYs = "X"), class = "cea_error_unknown_outcome")
  expect_error(
    ceac(fit_pooled, R = 9, wtp_max = 100000, wtp_step = 10000, method = "boot", sim = "ordinary"),
    class = "cea_error_bootstrap_pooled"
  )
})

test_that("autoplot.cea_ceac works as expected", {
  plt <- autoplot(fit_ceac)
  expect_s3_class(plt, "gg")
  expect_equal(plt$data, fit_ceac)
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold"))

  plt <- autoplot(fit_ceac, wtp = 60000)
  expect_s3_class(plt, "gg")
  expect_length(plt$layers, 2)
  expect_s3_class(plt$layers[[1]]$geom, "GeomVline")
  expect_s3_class(plt$layers[[2]]$geom, "GeomLine")

  plt <- autoplot(fit_ceac_fct)
  expect_s3_class(plt, "gg")
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold",
                                   colour = "tx", linetype = "tx"))
})

test_that("plot.cea_ceac works as expected", {
  plt <- with_null_pdf(plot(fit_ceac))
  expect_s3_class(plt, "gg")
  expect_equal(plt$data, fit_ceac)
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold"))
})
