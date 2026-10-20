## R CMD check results

0 errors | 0 warnings | 0 note

* This patch release fixes errors that risk CRAN archival. The failing tests
  relied on the exact matched counts produced by `{MatchIt}`; `{MatchIt}` 4.8.0
  changed how nearest-neighbor matching handles exact matching with a covariate
  caliper and the Mahalanobis distance. The affected tests now assert the
  `match_summary()` contract rather than fixed counts.
