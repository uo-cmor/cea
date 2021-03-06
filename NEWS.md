# cea 0.0.0.9000

* Add `method = 'brms'` option to `estimate()` to estimate Bayesian model.
  (methods for `cea_brms` objects still to be added)

* Restructure `estimate()` to use common arguments & structure as much as
  possible.

* Change default GLM family for Costs outcome to `Gamma("log")`.

* Renamed `boot()` to `boot_cea()`.

* Added `pool_cea()` and associated `print()`, `coef()`, `vcov()` and `tidy()`
  methods to pool multiple-imputation regression results.

* Added `mids` method for `estimate()`

* Added `CEAC()` to calculate cost-effectiveness acceptability curve from
  regression model.

* Added `QALYs()` and `Costs()` to extract fitted regression components.

* Added `ci()` to calculate bootstrap confidence intervals from regression
  model.
  
* Added `boot()` to calculate bootstrap replicates from regression model.

* Added `INMB()` & `INHB()` to calculate INMB/INHB from regression model
  results.

* Added `ICER()` to calculate ICER from regression model results.

* Added `estimate()` to estimate CEA regression model.

* Added a `NEWS.md` file to track changes to the package.
