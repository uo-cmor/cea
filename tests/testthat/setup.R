fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)
fit_mglmmPQL <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex,
                         method = "mglmmPQL")

fit_fct <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2)
fit_fct_mglmmPQL <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2,
                             method = "mglmmPQL")

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
    data = moa2_ex, control_algorithm = list(max_iter = 50)
  )
)

fit_lp <- estimate(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  treatment = "booster", link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2_ex
)
fit_mv <- estimate(
  mvfixed = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  treatment = "booster", data = moa2_ex, method = "mglmmPQL"
)

fit_cluster <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                        cluster = "centre")
fit_centre <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                       centre = "centre")
fit_centre_mglmmPQL <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                                centre = "centre", method = "mglmmPQL")
fit_mp <- estimate(
  "QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
  matrix_pred = rep(
    list(c(mcglm::mc_id(moa2_centre), mcglm::mc_mixed(~0 + centre, moa2_centre))),
    2
  )
)

boot_est <- boot_cea(fit, R = 9, sim = "ordinary")
boot_est_par <- boot_cea(fit, R = 9, sim = "parametric")
boot_est_fct <- boot_cea(fit_fct, R = 9, sim = "parametric")
boot_est_fct2 <- boot_cea(fit_fct2, R = 9, sim = "parametric")
boot_est_mglmmPQL <- boot_cea(fit_mglmmPQL, R = 9, sim = "ordinary")
boot_est_fct_mglmmPQL <- boot_cea(fit_fct_mglmmPQL, R = 9, sim = "parametric")

#boot_est_cluster <- boot_cea(fit_cluster, R = 9, sim = "ordinary")
boot_est_cluster_par <- boot_cea(fit_cluster, R = 9, sim = "parametric")

### MI methods require `mice`

skip_if_not_installed("mice", "3.0")

moa2_mi <- rbind(moa2, moa2, moa2)
moa2_mi$.imp <- as.character(rep(0:2, each = nrow(moa2)))
moa2_mi <- mice::as.mids(moa2_mi)
fit_mi <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_mi)
fit_mi_mglmmPQL <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_mi,
                            method = "mglmmPQL")

fit_pooled <- pool_cea(fit_mi)
fit_pooled_mglmmPQL <- pool_cea(fit_mi_mglmmPQL)

boot_pooled <- boot_cea(fit_pooled, R = 9, sim = "parametric")
boot_pooled_mglmmPQL <- boot_cea(fit_pooled_mglmmPQL, R = 9, sim = "parametric")
