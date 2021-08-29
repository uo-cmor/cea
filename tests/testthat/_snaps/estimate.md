# print.cea_estimate works

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Call: estimate(QALYs = "QALYs", costs = "Cost", treatment = "booster", 
        covars = c("age", "sex"), data = moa2_ex) 
    
    ------------------
    Univariate Models:
    
      QALYs: QALYs ~ booster + age + sex
        * Link function: identity 
        * Variance function: constant 
        * Covariance function: identity 
    
      Costs: Cost ~ booster + age + sex
        * Link function: log 
        * Variance function: tweedie 
        * Covariance function: identity 
    
    ------------------
    Incremental Treatment Effects:
      QALYs:     +0.070 
      Costs:      +2088 
      ICER:       30015 
    
    ===============================================

---

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Call: estimate(QALYs = "QALYs", costs = "Cost", treatment = "tx", covars = c("age", 
        "sex"), data = moa2) 
    
    ------------------
    Univariate Models:
    
      QALYs: QALYs ~ tx + age + sex
        * Link function: identity 
        * Variance function: constant 
        * Covariance function: identity 
    
      Costs: Cost ~ tx + age + sex
        * Link function: log 
        * Variance function: tweedie 
        * Covariance function: identity 
    
    ------------------
    Incremental Treatment Effects:
                    ExB         MT   MT + ExB 
      QALYs:     +0.088     +0.171     +0.007 
      Costs:      +1875      +1834      +1094 
      ICER:       21240      10744     148299 
    
    ===============================================

