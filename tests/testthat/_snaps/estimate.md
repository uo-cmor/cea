# cea_estimate print methods work

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Multivariate Covariate Generalized Linear Model
    
    Call:
     estimate.data.frame(QALYs = "QALYs", costs = "Cost", treatment = "booster", 
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
      QALYs:     +0.069 
      Costs:      +2421 
      ICER:       35012 
    
    ===============================================

---

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Multivariate Generalized Linear Mixed-Effects Model
    
    Call:
     estimate.data.frame(QALYs = "QALYs", costs = "Cost", treatment = "booster", 
        covars = c("age", "sex"), data = moa2_ex, method = "mglmmPQL") 
    
    ------------------
    Univariate Models:
    
      QALYs: QALYs ~ booster + age + sex
        * Family: gaussian 
        * Link: identity 
      Costs: Cost ~ booster + age + sex
        * Family: Gamma 
        * Link: log 
    ------------------
    Incremental Treatment Effects:
      QALYs:     +0.069 
      Costs:      +2421 
      ICER:       35012 
    
    ===============================================

---

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Multivariate Covariate Generalized Linear Model
    
    Call:
     estimate.data.frame(QALYs = "QALYs", costs = "Cost", treatment = "tx", 
        covars = c("age", "sex"), data = moa2) 
    
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
      QALYs:     +0.089     +0.171     +0.007 
      Costs:      +2238      +2084      +1252 
      ICER:       25266      12197     167702 
    
    ===============================================

---

    ===============================================
    === Cost-Effectiveness Regression Estimates ===
    ===============================================
    
    Multivariate Generalized Linear Mixed-Effects Model
    
    Call:
     estimate.data.frame(QALYs = "QALYs", costs = "Cost", treatment = "tx", 
        covars = c("age", "sex"), data = moa2, method = "mglmmPQL") 
    
    ------------------
    Univariate Models:
    
      QALYs: QALYs ~ tx + age + sex
        * Family: gaussian 
        * Link: identity 
      Costs: Cost ~ tx + age + sex
        * Family: Gamma 
        * Link: log 
    ------------------
    Incremental Treatment Effects:
                    ExB         MT   MT + ExB 
      QALYs:     +0.089     +0.171     +0.007 
      Costs:      +2238      +2084      +1252 
      ICER:       25266      12197     167702 
    
    ===============================================

---

    ================================================================
    === Multiply-Imputed Cost-Effectiveness Regression Estimates ===
    ================================================================
    Based on 2 imputed datasets.
    
    Call:
     estimate.mids(QALYs = "QALYs", costs = "Cost", treatment = "tx", 
        covars = c("age", "sex"), data = moa2_mi) 
    
    Data:
     mice(data = data, m = m, where = where, maxit = 0, remove.collinear = FALSE, 
        allow.na = TRUE) 
    
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
    (From first imputed dataset; use `pool_cea()` to compute pooled estimates)
                    ExB         MT   MT + ExB 
      QALYs:     +0.089     +0.171     +0.007 
      Costs:      +2238      +2084      +1252 
      ICER:       25266      12197     167702 
    
    ===============================================

