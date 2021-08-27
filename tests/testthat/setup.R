fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)

fit_fct <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2)

fit_mcglm <- with_sink(
  tempfile(),
  mcglm::mcglm(
    linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
    matrix_pred = list(mcglm::mc_id(moa2_ex), mcglm::mc_id(moa2_ex)),
    link = c("identity", "log"), variance = c("constant", "tweedie"),
    data = moa2_ex
  )
)

fit_lp <- estimate(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  treatment = "booster", link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2_ex
)

boot_est <- boot(fit, R = 9)
boot_est_par <- boot(fit, R = 9, sim = "parametric")
boot_est_fct <- boot(fit_fct, R = 9, sim = "parametric")
