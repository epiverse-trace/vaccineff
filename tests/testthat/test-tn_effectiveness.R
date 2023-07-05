test_that("Errors are returned as expected", {
  data(testnegdata)
  expect_error(estimate_tn_eff(testnegdata, vaccine12mo_yn, flu_final_posneg))
  expect_error(estimate_tn_eff(testnegdata, vaccine12mo_yn, "flu_final_posneg"))
  expect_error(estimate_tn_eff(testnegdata, "vaccine12mo_yn", flu_final_posneg))
  expect_error(estimate_tn_eff(testnegdata[, 1], vaccine12mo_yn,
                               flu_final_posneg))
})

test_that("Vaccine effectiveness is calculated", {
  expect_true(is.numeric(estimate_tn_eff(testnegdata,
                                         "vaccine12mo_yn",
                                         "flu_final_posneg"))
              )
})
