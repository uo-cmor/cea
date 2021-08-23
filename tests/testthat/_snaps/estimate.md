# print.cea_estimate works

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    QALYs model: QALYs ~ booster + age + sex 
                 * Link function: identity 
                 * Variance function: constant 
                 * Covariance function: identity 
    
    Costs model: Cost ~ booster + age + sex 
                 * Link function: log 
                 * Variance function: tweedie 
                 * Covariance function: identity 
    
    Call: estimate(QALYs = "QALYs", costs = "Cost", treatment = "booster", 
        covars = c("age", "sex"), data = moa2) 
    
    Incremental Treatment Effects:
      QALYs: +0.074 
      Costs: +2023 
      ICER: 27165 
    
    ===============================================

