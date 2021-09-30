# print.cea_mcglm_pooled works

    ======================================================
    === Pooled Cost-Effectiveness Regression Estimates ===
    ======================================================
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
                    ExB         MT   MT + ExB 
      QALYs:     +0.089     +0.171     +0.007 
      Costs:      +2238      +2084      +1252 
      ICER:       25266      12197     167702 
    
    ===============================================

---

    ======================================================
    === Pooled Cost-Effectiveness Regression Estimates ===
    ======================================================
    Based on 2 imputed datasets.
    
    Call:
     estimate.mids(QALYs = "QALYs", costs = "Cost", treatment = "tx", 
        covars = c("age", "sex"), data = moa2_mi, method = "mglmmPQL") 
    
    Data:
     mice(data = data, m = m, where = where, maxit = 0, remove.collinear = FALSE, 
        allow.na = TRUE) 
    
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

