fit_ceac <- ceac(fit, R = 9, wtp_max = 100000, wtp_step = 10000)
fit_ceac_delta <- ceac(fit, wtp_max = 100000, wtp_step = 10000, method = "delta")
fit_ceac_boot <- ceac(boot_est, wtp_max = 100000, wtp_step = 10000)

test_that("ceac works with cea_estimate objects", {
  expect_s3_class(fit_ceac, "cea_ceac")
  expect_equal(dim(fit_ceac), c(11, 2))
  expect_equal(fit_ceac$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac$ceac >= 0 & fit_ceac$ceac <= 1))
  expect_equal(attr(fit_ceac, "method"), "boot")
  expect_equal(attr(fit_ceac, "R"), 9)
  expect_equal(attr(fit_ceac, "sim"), "ordinary")
})

test_that("ceac works with delta method", {
  expect_s3_class(fit_ceac_delta, "cea_ceac")
  expect_equal(dim(fit_ceac_delta), c(11, 2))
  expect_equal(fit_ceac_delta$wtp, seq.int(0, 100000, 10000))
  expect_equal(
    fit_ceac_delta$ceac,
    c(0.14117573, 0.27930620, 0.42579953, 0.52280834, 0.58217543, 0.62005865, 0.64569095,
      0.66396032, 0.67754782, 0.68800630, 0.6962838)
  )
  expect_equal(attr(fit_ceac_delta, "method"), "delta")
})

test_that("ceac works with cea_boot objects", {
  expect_s3_class(fit_ceac_boot, "cea_ceac")
  expect_equal(dim(fit_ceac_boot), c(11, 2))
  expect_equal(fit_ceac_boot$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac_boot$ceac >= 0 & fit_ceac_boot$ceac <= 1))
  expect_equal(attr(fit_ceac_boot, "method"), "boot")
  expect_equal(attr(fit_ceac_boot, "R"), 9)
  expect_equal(attr(fit_ceac_boot, "sim"), "ordinary")
})

test_that("ceac gives appropriate errors", {
  expect_error(ceac(fit_mcglm), "no applicable method")
  expect_error(ceac(fit, method = "x"), class = "cea_error_unknown_method")
  expect_error(ceac(fit), class = "cea_error_missing_R")
  expect_error(ceac(fit, R = 9, QALYs = "X"), class = "cea_error_unknown_outcome")
  expect_error(ceac(boot_est, QALYs = "X"), class = "cea_error_unknown_outcome")
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
})

test_that("plot.cea_ceac works as expected", {
  plt <- with_sink(tempfile(), plot(fit_ceac))
  expect_s3_class(plt, "gg")
  expect_equal(plt$data, fit_ceac)
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold"))
})
