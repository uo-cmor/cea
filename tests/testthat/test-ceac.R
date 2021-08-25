fit_ceac <- ceac(fit, R = 9, wtp_max = 100000, wtp_step = 10000)

test_that("ceac works as expected", {
  expect_s3_class(fit_ceac, "cea_ceac")
  expect_equal(dim(fit_ceac), c(11, 2))
  expect_equal(fit_ceac$wtp, seq.int(0, 100000, 10000))
  expect_true(all(fit_ceac$ceac >= 0 & fit_ceac$ceac <= 1))
  expect_equal(attr(fit_ceac, "R"), 9)
  expect_equal(attr(fit_ceac, "sim"), "ordinary")
})

test_that("ceac gives appropriate errors", {
  expect_error(ceac(fit_mcglm), class = "cea_error_not_cea_estimate")
  expect_error(ceac(fit, method = "x"), class = "cea_error_unknown_method")
  expect_error(ceac(fit), class = "cea_error_missing_R")
})

test_that("autoplot.cea_ceac works as expected", {
  plt <- autoplot(fit_ceac)
  expect_s3_class(plt, "gg")
  expect_equal(plt$data, fit_ceac)
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold"))
})

test_that("plot.cea_ceac works as expected", {
  plt <- plot(fit_ceac)
  expect_s3_class(plt, "gg")
  expect_equal(plt$data, fit_ceac)
  expect_length(plt$layers, 1)
  expect_s3_class(plt$layers[[1]]$geom, "GeomLine")
  expect_mapequal(plt$labels, list(y = "CEAC", x = "Willingness-to-pay threshold"))
})
