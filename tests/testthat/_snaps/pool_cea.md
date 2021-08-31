# print.cea_pooled works

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
      QALYs:     +0.088     +0.171     +0.007 
      Costs:      +1875      +1834      +1094 
      ICER:       21240      10744     148299 
    
    ===============================================

