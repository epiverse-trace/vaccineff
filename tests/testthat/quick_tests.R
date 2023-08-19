data(cohortdata)
cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = c("vaccine_date_1", "vaccine_date_2"),
  operator = "%",
  status = c("v", "u")
)

head(cohortdata)

operato <- "&"
operator <- match.arg(operator, several.ok = FALSE)
  checkmate::assert_string(
    operator,
    n.chars = 1L
  )