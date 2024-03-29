#' Internal function to extract summary output from survival models.
#'
#' @param model survival object with model
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @return Data frame with survival data
#' @keywords internal
extract_surv_model <- function(model, start_cohort, end_cohort) {
  days <- end_cohort - start_cohort
  tte <- seq(0, as.numeric(days) - 1, by = 1)
  res <- summary(model, times = tte, scale = 1)
  cols <- lapply(c(2:6, 8:16), function(x) res[x])
  tbl <- do.call(data.frame, cols)
  tbl$date <- tbl$time + start_cohort
  return(tbl)
}

#' Function to plot the Survival probability based on the Kaplan-Meir model
#'
#' The function relies on the implementation of the Kaplan-Meier model from
#' survival package.
#' It returns a plot of the Survival Probability or the Cumulative Hazard (if
#' cumulative = TRUE).
#' The return is a ggplot2 element of the curves with 95% C.I.
#' It is possible to manipulate the colors, labels, legend and most of
#' the graphic elements.
#' @importFrom rlang .data
#' @param data dataset with cohort information (see example)
#' @param outcome_status_col name of the column containing status of the
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event
#' @param vacc_status_col name of the column containing the vaccination
#' status
#' @param vaccinated_status string assigned to the vaccinated population
#' @param unvaccinated_status string assigned to the unvaccinated population
#' @param vaccinated_color color assigned to the vaccinated population
#' @param unvaccinated_color color assigned to the unvaccinated population
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @param percentage if TRUE returns probability in percentage
#' @param cumulative if TRUE returns cumulative Hazards (1-Survival)
#' @return ggplot2 plot of survival/cumulative hazard
#' @examples
#' # load example package data
#' data("cohortdata")
#' head(cohortdata)
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#'
#' cohortdata$immunization <-
#'   get_immunization_date(
#'     data = cohortdata,
#'     outcome_date_col = "death_date",
#'     outcome_delay = 0,
#'     immunization_delay = 14,
#'     vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
#'     end_cohort = end_cohort,
#'     take_first = FALSE
#'   )
#'
#' cohortdata$vaccine_status <- set_status(
#'   data = cohortdata,
#'   col_names = "immunization",
#'   status = c("v", "u")
#' )
#'
#' cohortdata$death_status <- set_status(
#'   data = cohortdata,
#'   col_names = "death_date"
#' )
#'
#' cohortdata$time_to_death <- get_time_to_event(
#'   data = cohortdata,
#'   outcome_date_col = "death_date",
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort,
#'   start_from_immunization = FALSE
#' )
#'
#' plot_survival(
#'   data = cohortdata,
#'   outcome_status_col = "death_status",
#'   time_to_event_col = "time_to_death",
#'   vacc_status_col = "vaccine_status",
#'   vaccinated_status = "v",
#'   unvaccinated_status = "u",
#'   vaccinated_color = "steelblue",
#'   unvaccinated_color = "darkred",
#'   start_cohort = start_cohort,
#'   end_cohort = end_cohort,
#'   percentage = TRUE,
#'   cumulative = TRUE
#' )
#' @export
plot_survival <- function(data, outcome_status_col,
                          time_to_event_col,
                          vacc_status_col,
                          vaccinated_status = "v",
                          unvaccinated_status = "u",
                          vaccinated_color = "steelblue",
                          unvaccinated_color = "darkred",
                          start_cohort,
                          end_cohort,
                          percentage = TRUE,
                          cumulative = FALSE) {
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_names(
    names(data),
    must.include = c(outcome_status_col, time_to_event_col, vacc_status_col)
  )
  checkmate::assert_character(
    c(vaccinated_color, unvaccinated_color)
  )
  checkmate::assert_logical(
    percentage,
    len = 1
  )
  checkmate::assert_logical(
    cumulative,
    len = 1
  )
  # check date types
  checkmate::assert_date(
    start_cohort,
    any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort,
    any.missing = FALSE, len = 1
  )
  checkmate::assert_names(
    data[[vacc_status_col]],
    must.include = c(vaccinated_status, unvaccinated_status)
  )

  # KM model
  km_model <- survival::survfit(
    survival::Surv(
      data[[time_to_event_col]],
      data[[outcome_status_col]]
    ) ~ data[[vacc_status_col]]
  )

  # Extract data from survival element
  results <- extract_surv_model(km_model, start_cohort, end_cohort)

  if (cumulative) {
    results$plot <- 1 - results$surv
    results$plot_lower <- 1 - results$upper
    results$plot_upper <- 1 - results$lower
  } else {
    results$plot <- results$surv
    results$plot_lower <- results$upper
    results$plot_upper <- results$lower
  }

  results$strata <- factor(results$strata,
    levels = c(
      paste0("data[[vacc_status_col]]=", vaccinated_status),
      paste0("data[[vacc_status_col]]=", unvaccinated_status)
    )
  )
  levels(results$strata) <- c(vaccinated_status, unvaccinated_status)

  # set colour names for vaccination status
  colors <- c(vaccinated_color, unvaccinated_color)
  vacc_status <- c(vaccinated_status, unvaccinated_status)
  names(colors) <- c(vaccinated_status, unvaccinated_status)

  plt <-
    ggplot2::ggplot(data = results) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = .data$time,
        ymin = .data$plot_lower, ymax = .data$plot_upper,
        fill = .data$strata
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_step(
      ggplot2::aes(
        x = .data$time, y = .data$plot,
        color = .data$strata
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = ifelse(
        test = percentage, yes = scales::label_percent(),
        no = scales::label_number()
      )
    ) +
    ggplot2::scale_color_manual(
      name = "Vaccine Status",
      values = colors,
      labels = vacc_status
    ) +
    ggplot2::scale_fill_manual(
      name = "Vaccine Status",
      values = colors,
      labels = vacc_status
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Time to event (Days)",
      y = ifelse(
        cumulative, yes = "Cumulative hazard", no = "Survival probability"
      )
    ) +
    ggplot2::xlab("Time to event (Days)") +
    ggplot2::labs(colour = "Vaccine Status")
  return(plt)
}

#' Function to plot the vaccine coverage in a cohort
#'
#' The function returns a plot of the vaccine coverage or the cumulative
#' coverage (if cumulative = TRUE).
#' The return is a 2-axis ggplot2 element with the number of vaccines per date
#' in the left-axes and the coverage per date in the right-axes.
#' It is possible to manipulate the colors, labels, legend and most of
#' the graphic elements.
#' @importFrom rlang .data
#' @inheritParams coh_coverage
#' @param doses_count_color color assigned to the doses count
#' @param coverage_color color assigned to the coverage calculation
#' @param cumulative if TRUE returns the cumulative number of doses
#' over the time window
#' @return 2-axis ggplot2 plot of vaccine coverage and daily doses
#' @examples
#' data("cohortdata")
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#' date_interval <- c(start_cohort, end_cohort)
#' plot_coverage(
#'   data = cohortdata,
#'   vacc_date_col = "vaccine_date_1",
#'   unit = "month",
#'   doses_count_color = "steelblue",
#'   coverage_color = "mediumpurple",
#'   date_interval = date_interval,
#'   cumulative = FALSE
#' )
#' @export
plot_coverage <- function(data,
                          vacc_date_col,
                          unit = c("day", "month", "year"),
                          doses_count_color = "steelblue",
                          coverage_color = "mediumpurple",
                          date_interval = NULL,
                          cumulative = FALSE) {
  checkmate::assert_data_frame(
    data,
    min.rows = 1L
  )
  checkmate::assert_character(
    vacc_date_col,
    min.len = 1L
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c(vacc_date_col)
  )
  checkmate::assert_date(
    data[[vacc_date_col]]
  )
  unit <- match.arg(unit, several.ok = FALSE)
  checkmate::assert_string(
    unit
  )
  checkmate::assert_logical(
    cumulative,
    len = 1
  )
  checkmate::assert_character(
    c(doses_count_color, coverage_color)
  )

  coverage <- coh_coverage(
    data = data,
    vacc_date_col = vacc_date_col,
    unit = unit,
    date_interval = date_interval
  )

  if (cumulative) {
    coverage$dose_plot <- coverage$cum_doses
  } else {
    coverage$dose_plot <- coverage$doses
  }

  plt <- ggplot2::ggplot(coverage) +
    ggplot2::geom_col(
      ggplot2::aes(
        x = .data$date,
        y = .data$dose_plot,
        fill = "Doses"
      ),
      alpha = 0.6
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$date,
        y = .data$coverage * max(.data$dose_plot),
        group = 1,
        colour = "Coverage"
      ),
      linetype = "dashed"
    ) +
    ggplot2::scale_x_date(name = NULL, date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(
      name = paste0("Doses per ", unit),
      labels = scales::label_number(
        scale_cut = scales::cut_short_scale()
      ),
      sec.axis = ggplot2::sec_axis(~. / max(coverage$dose_plot),
                                   labels = scales::percent,
                                   name = "Percentage of coverage")
    ) +
    ggplot2::scale_fill_manual(
      name = NULL, values = doses_count_color
    ) +
    ggplot2::scale_color_manual(
      name = NULL, values = coverage_color
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "top")

  return(plt)
}
