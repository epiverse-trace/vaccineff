#' @title Construct `vaccineff_data` Object
#'
#' @description This function construct `vaccineff_data`
#'
#' @param data_set `data.frame` with cohort information (see example).
#' @param outcome_date_col Name of the column that contains the outcome dates.
#' @param censoring_date_col Name of the column that contains the censoring
#' date. NULL by default.
#' @param vacc_date_col Name of the column(s) that contain the vaccine dates.
#' @param vacc_name_col Name of the column(s) that contain custom vaccine
#' names for the vaccines (e.g. brand name, type of vaccine). If provided,
#' must be of the same length as `vacc_date_col`.
#' @param vaccinated_status Status assigned to the vaccinated population.
#' Default is `v`.
#' @param unvaccinated_status Status assigned to the unvaccinated population.
#' Default is `u`.
#' @param immunization_delay Characteristic time in days before the patient
#' is considered immune.
#' @param start_cohort Start date of the study.
#' @param end_cohort End date of the study.
#' @param method `TRUE`: cohort matching is performed. Default is `FALSE`
#' @param exact Name(s) of column(s) for `exact` matching. Default is `NULL`.
#' @param nearest Named vector with name(s) of column(s) for `nearest` matching
#' and caliper(s) for each variable (e.g., `nearest = c("characteristic1" = n1,
#' "characteristic2" = n2)`, where `n1` and `n2` are the calipers). Default is
#' `NULL`.
#' @param take_first `FALSE`: takes the latest vaccine date. `TRUE`: takes the
#' earliest vaccine date.
#' @return Original `data.frame` passed in `data_set` and additional columns
#' containing information on the immunization.

make_vaccineff_data <- function(data_set,
                                outcome_date_col,
                                censoring_date_col = NULL,
                                vacc_date_col,
                                vacc_name_col = NULL,
                                vaccinated_status = "v",
                                unvaccinated_status = "u",
                                immunization_delay = 0,
                                start_cohort,
                                end_cohort,
                                match = FALSE,
                                exact = NULL,
                                nearest = NULL,
                                take_first = FALSE) {
  # check inputs
  check_vaccineff_inputs(
    data_set = data_set,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    vacc_date_col = vacc_date_col,
    vacc_name_col = vacc_name_col,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    immunization_delay = immunization_delay,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    match = match,
    exact = exact,
    nearest = nearest,
    take_first = take_first
  )

  cohort_data <- linelist::make_linelist(
    x = as.data.frame(
      make_immunization(
        data_set = data_set,
        outcome_date_col = outcome_date_col,
        censoring_date_col = censoring_date_col,
        vacc_date_col = vacc_date_col,
        vacc_name_col = vacc_name_col,
        vaccinated_status = vaccinated_status,
        unvaccinated_status = unvaccinated_status,
        immunization_delay = immunization_delay,
        end_cohort = end_cohort,
        take_first = take_first
      )
    )
  )

  cohort_data <- linelist::set_tags(
    x = cohort_data,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    vacc_date_col = vacc_date_col,
    vacc_name_col = vacc_name_col,
    immunization_date_col = "immunization_date",
    vacc_status_col = "vaccine_status",
    allow_extra = TRUE
  )

  matching <- match_cohort(
    data_set = cohort_data,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    method = "static",
    exact = exact,
    nearest = nearest
  )

  matching <- match_cohort(
    data_set = cohort_data,
    outcome_date_col = outcome_date_col,
    censoring_date_col = censoring_date_col,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    method = "static",
    exact = exact,
    nearest = nearest,
    immunization_date_col = "immunization_date",
    vacc_status_col = "vaccine_status",
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status
  )
  vaccineff_data <- list(
    cohort_data = cohort_data,
    start_cohort = start_cohort,
    end_cohort = end_cohort,
    vaccinated_status = vaccinated_status,
    unvaccinated_status = unvaccinated_status,
    immunization_delay = immunization_delay,
    matching = matching$match
  )

  return(vaccineff_data)
}