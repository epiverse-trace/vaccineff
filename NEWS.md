## New Features

The number of functions and steps for computing vaccine effectiveness has 
been drastically reduced in `{vaccineff 0.0.2}`. The new pipeline for 
estimation now consists of three main functions:

* **`make_immunization()`**: Prepares information on immunization dates 
  and vaccine status. It can handle multiple columns for vaccine dates and 
  custom vaccine statuses. In such cases, it returns the name of the column 
  selected as immunizing and the custom name, if provided.

* **`match_cohort()`**: This function has been improved and generalized 
  to reduce observation bias in cohorts. The default matching strategy is 
  static, based on nearest and exact characteristics using Mahalanobis 
  distance. The exposure times of the pairs are adjusted after matching. In 
  future releases, rolling calendar matching will be introduced as a more 
  accurate method to account for exposure times. The function returns an S3 
  object of class `match`, from which a summary and balance of the cohorts 
  can be printed using the `summary()` method. The matched cohort can be 
  extracted using the `get_dataset()` method. The matched cohort contains 
  all the necessary information to estimate vaccine effectiveness.

* **`effectiveness()`**: Receives a (matched) cohort and estimates vaccine 
  effectiveness using the Hazard Ratio (HR). An S3 object of class 
  `effectiveness` is returned, compatible with the `plot()` and `summary()` 
  methods. Future releases will provide relative risk (RR) as an alternative 
  for cases where the proportional hazards assumption is not satisfied.

## Breaking Changes

The following functions are no longer accessible to users. However, they 
are called within `make_immunization()`:

* `get_immunization_date()`
* `get_immunization_dose()`
* `get_immunization_vaccine()`
* `set_status()`

Similarly, the `effectiveness()` function deprecates the use of 
`coh_eff_noconf()`, and the `plot()` method now returns a log-log plot, 
replacing the `plot_loglog()` function.
