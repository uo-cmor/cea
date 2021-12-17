fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex)
fit_mglmmPQL <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex,
                         method = "mglmmPQL")
fit_brms <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_ex, method = "brms")

fit_fct <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2)
fit_fct_mglmmPQL <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2,
                             method = "mglmmPQL")
fit_fct_brms <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2, method = "brms")

moa2_base_exb <- moa2
contrasts(moa2_base_exb$tx) <- contr.treatment(levels(moa2_base_exb$tx), base = 2)
fit_fct2 <- estimate("QALYs", "Cost", "tx", c("age", "sex"), data = moa2_base_exb)

moa2_chr <- moa2_ex
moa2_chr$booster <- as.character(moa2_chr$booster)

fit_mcglm <- with_sink(
  tempfile(),
  {
    control_initial <- mcglm::mc_initial_values(
      linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
      matrix_pred = list(mcglm::mc_id(moa2_ex), mcglm::mc_id(moa2_ex)),
      link = c("identity", "log"), variance = c("constant", "tweedie"),
      covariance = c("identity", "identity"), offset = c(0, 0), data = moa2_ex
    )
    control_initial$power <- list(0, 2)
    mcglm::mcglm(
      linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
      matrix_pred = list(mcglm::mc_id(moa2_ex), mcglm::mc_id(moa2_ex)),
      link = c("identity", "log"), variance = c("constant", "tweedie"), data = moa2_ex,
      control_algorithm = list(max_iter = 100), control_initial = control_initial
    )
  }
)

fit_lp <- estimate(
  fixed = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  treatment = "booster", family = list("gaussian", Gamma("log")), data = moa2_ex
)
fit_mv <- estimate(
  fixed = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  treatment = "booster", data = moa2_ex, method = "mglmmPQL"
)

fit_cluster <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                        cluster = "centre", method = "mglmmPQL")
fit_centre <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                       centre = "centre", method = "mglmmPQL")
fit_cl <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2_centre,
                   random = ~ 1 | centre, method = "mglmmPQL")

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
