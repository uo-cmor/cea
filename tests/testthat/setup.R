fit <- estimate("QALYs", "Cost", "booster", c("age", "sex"), data = moa2)

fit_mcglm <- mcglm::mcglm(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  matrix_pred = list(mcglm::mc_id(moa2), mcglm::mc_id(moa2)),
  link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2
)

fit_lp <- estimate(
  linear_pred = c(QALYs = QALYs ~ booster + age + sex, Costs = Cost ~ booster + age + sex),
  link = c("identity", "log"), variance = c("constant", "tweedie"),
  data = moa2
)
