% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_vaccineff.R
\name{estimate_vaccineff}
\alias{estimate_vaccineff}
\title{Estimate Vaccine Effectiveness (VE)}
\usage{
estimate_vaccineff(vaccineff_data, at)
}
\arguments{
\item{vaccineff_data}{Object of the class \code{vaccineff_data} with
vaccineff data.}

\item{at}{Number of days at which VE is estimated from the beginning of the
follow-up period.}
}
\value{
Object of the class \code{vaccineff}: a list with results from the
estimation of VE.
\code{ve}: \code{data.frame} with VE(CI95\%)
\code{cox_model}: \code{survival} object with Cox model results
\code{kaplan_meier}: \code{survival} object with Kaplan-Meier estimator
}
\description{
This function provides methods for estimating VE. It relies on
the Kaplan-Meier estimator and the Cox model for proportional hazards
from the \code{{survival}} package. Currently, the default method is VE = 1 - HR,
where HR is the Hazard Ratio calculated using the Cox model. The
proportional hazards assumption is tested using the Schoenfeld test,
with the p-value provided in the results. Log-log plots are also generated
using the Kaplan-Meier estimator for a visual test of the proportional
hazards hypothesis. The function uses column names provided in the tags
\code{outcome_status_col}, \code{time_to_event_col}, and \code{vaccine_status_col} of the
\code{linelist} object and status names from \code{make_vaccineff_data}.
The return is an \verb{S3 class} object with the VE (CI95\%), results from the Cox
model, and the Kaplan-Meier estimator. This object is compatible with
\code{summary} and \code{plot} methods.
}
\examples{
\donttest{
# Load example data
data("cohortdata")

# Create `vaccineff_data`
vaccineff_data <- make_vaccineff_data(data_set = cohortdata,
  outcome_date_col = "death_date",
  censoring_date_col = "death_other_causes",
  vacc_date_col = "vaccine_date_2",
  vaccinated_status = "v",
  unvaccinated_status = "u",
  immunization_delay = 15,
  end_cohort = as.Date("2021-12-31"),
  match = TRUE,
  exact = c("age", "sex"),
  nearest = NULL
)

# Estimate the Vaccine Effectiveness (VE)
ve <- estimate_vaccineff(vaccineff_data, 90)

# Print summary of VE
summary(ve)

# Generate loglog plot to check proportional hazards
plot(ve, type = "loglog")

# Generate Survival plot
plot(ve, type = "surv", percentage = FALSE, cumulative = FALSE)
}
}
