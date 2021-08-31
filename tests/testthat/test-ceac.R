fit_ceac <- ceac(fit, R = 9, wtp_max = 100000, wtp_step = 10000)
fit_ceac_fct <- ceac(fit_fct, R = 9, wtp_max = 100000, wtp_step = 10000)
fit_ceac_delta <- ceac(fit, wtp_max = 100000, wtp_step = 10000, method = "delta")
fit_ceac_boot <- ceac(boot_est, wtp_max = 100000, wtp_step = 10000)
fit_ceac_boot_fct <- ceac(boot_est_fct, wtp_max = 100000, wtp_step = 10000)
fit_ceac_boot_fct2 <- ceac(boot_est_fct2, wtp_max = 100000, wtp_step = 10000)
fit_ceac_pooled <- ceac(fit_pooled, R = 9, wtp_max = 100000, wtp_step = 10000, sim = "parametric")
fit_ceac_pooled_delta <- ceac(fit_pooled, wtp_max = 100000, wtp_step = 10000, method = "delta")

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
    c(0.1351090, 0.2637621, 0.4043865, 0.4998886, 0.5593634, 0.5977586, 0.6239453, 0.6427170,
      0.6567379, 0.6675656, 0.6761580),
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
    c(0.1413257, 0.3096712, 0.4835808, 0.5873104, 0.6456926, 0.6809783, 0.7040220, 0.7200541,
      0.7317737, 0.7406794, 0.7476590, 0.1549362, 0.4753990, 0.7190983, 0.8208210, 0.8655385,
      0.8886021, 0.9021477, 0.9108917, 0.9169393, 0.9213438, 0.9246816, 0.2541600, 0.2974421,
      0.3578533, 0.4003587, 0.4278110, 0.4461955, 0.4591445, 0.4686810, 0.4759658, 0.4816980,
      0.4863193),
    tolerance = 1e-7
  )
  expect_equal(fit_ceac_pooled_delta$tx, rep(c("ExB", "MT", "MT + ExB"), each = 11))
  expect_equal(attr(fit_ceac_pooled_delta, "method"), "delta")
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
  expect_error(ceac(fit), class = "cea_error_missing_R")
  expect_error(ceac(fit, R = 9, QALYs = "X"), class = "cea_error_unknown_outcome")
  expect_error(ceac(boot_est, QALYs = "X"), class = "cea_error_unknown_outcome")
  expect_error(ceac(fit_pooled, R = 9, wtp_max = 100000, wtp_step = 10000),
               class = "cea_error_bootstrap_pooled")
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
