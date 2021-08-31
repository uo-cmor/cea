fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)

fit_fct <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2)

moa2_base_exb <- moa2
contrasts(moa2_base_exb$tx) <- contr.treatment(levels(moa2_base_exb$tx), base = 2)
fit_fct2 <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_base_exb)

moa2_chr <- moa2_ex
moa2_chr$booster <- as.character(moa2_chr$booster)

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
boot_est_fct2 <- boot(fit_fct2, R = 9, sim = "parametric")

### MI methods require `mice`

skip_if_not_installed("mice", "3.0")

moa2_mi <- rbind(moa2, moa2, moa2)
moa2_mi$.imp <- as.character(rep(0:2, each = nrow(moa2)))
moa2_mi <- mice::as.mids(moa2_mi)
fit_mi <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_mi)

fit_pooled <- pool_cea(fit_mi)

boot_pooled <- boot(fit_pooled, R = 9, sim = "parametric")
