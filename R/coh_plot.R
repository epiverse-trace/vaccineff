#' Internal function to extract summary output from survival models.
#'
#' @param model survival object with model
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @return Data frame with survival data
#' @export
extract_surv_model <- function(model, start_cohort, end_cohort) {
  days <- end_cohort - start_cohort
  tte <- seq(0, as.numeric(days) - 1, by = 1)
  res <- summary(model, times = tte, scale = 1)
  cols <- lapply(c(2:6, 8:16), function(x) res[x])
  tbl <- do.call(data.frame, cols)
  tbl$date <- tbl$time + start_cohort
  return(tbl)
}

#' Function to plot the Survival probability based on the Kaplan-Meir model.
#'
#' The function relies on the implementation of the Kaplan-Meier model from
#' survival package.
#' It returns a plot of the Survival Probability or the Cumulative Hazard (if
#' cumulative = TRUE).
#' The return is a ggplot2 element of the curves with 95% C.I that can be
#' manipulated in order to change the colors, labels, legend, etc.
#' @param data dataset with cohort information (see example)
#' @param outcome_status_col name of the column containing status of the
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event
#' @param vacc_status_col name of the column containing the vaccination
#' status
#' @param vacc_status two-element vector specifying the values assigned that
#' indicate whether the individual is vaccinated or not e.g. c("v","u").
#' It must coincide with the values of the column `vacc_status_col`
#' @param start_cohort start date of the study
#' @param end_cohort end date of the study
#' @param colors list of two colors the type:
#' c("c1" = "steelblue", "c2" = "darkred")
#' @param percentage if TRUE returns probability in percentage
#' @param cumulative if TRUE returns cumulative Hazards (1-Survival)
#' @return Survival/Cumulative hazard plot: ggplot2 element
#' @examples
#' # load example package data
#' data("cohortdata")
#' head(cohortdata)
#' start_cohort <- as.Date("2044-01-01")
#' end_cohort <- as.Date("2044-12-31")
#'
#' cohortdata$immunization_death <-
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
#'   col_names = "immunization_death",
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
#' survival_plot <- plot_survival(data = cohortdata,
#'                                outcome_status_col = "death_status",
#'                                time_to_event_col = "time_to_death",
#'                                vacc_status_col = "vaccine_status",
#'                                status = c("v", "u"),
#'                                start_cohort = start_cohort,
#'                                end_cohort = end_cohort,
#'                                colors = c("steelblue", "darkred"),
#'                                percentage = TRUE,
#'                                cumulative = TRUE)
#' @export
plot_survival <- function(data, outcome_status_col,
                          time_to_event_col,
                          vacc_status_col,
                          vacc_status,
                          start_cohort,
                          end_cohort,
                          colors = c("c1" = "steelblue",
                                     "c2" = "darkred"),
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
    colors, len = 2
  )
  checkmate::assert_logical(
    percentage, len = 1
  )
  checkmate::assert_logical(
    cumulative, len = 1
  )
  # check date types
  checkmate::assert_date(
    start_cohort, any.missing = FALSE, len = 1
  )
  checkmate::assert_date(
    end_cohort, any.missing = FALSE, len = 1
  )

  #KM model
  km_model <- survival::survfit(
    survival::Surv(
      data[[time_to_event_col]],
      data[[outcome_status_col]]
    ) ~ data[[vacc_status_col]]
  )

  #Extract data from survival element
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

  #Filter data by status
  conn1 <- paste0("data[[vacc_status_col]]=", vacc_status[1])
  results_1 <- results[results$strata == conn1, ]
  conn2 <- paste0("data[[vacc_status_col]]=", vacc_status[2])
  results_2 <- results[results$strata == conn2, ]

  #Plot
  plt <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = results_1$time,
        ymin = results_1$plot_lower, ymax = results_1$plot_upper
      ),
      fill = colors[1], alpha = 0.2
    ) +
    ggplot2::geom_step(
      ggplot2::aes(x = results_1$time, y = results_1$plot,
        color = "c1"
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = results_2$time,
        ymin = results_2$plot_lower, ymax = results_2$plot_upper
      ),
      fill = colors[2], alpha = 0.2
    ) +
    ggplot2::geom_step(
      ggplot2::aes(x = results_2$time, y = results_2$plot,
        color = "c2"
      )
    ) + {
    if (percentage) {
      ggplot2::scale_y_continuous(labels = scales::percent)
    }
  } +
    ggplot2::scale_color_manual(name = "Vaccine Status",
      values = colors,
      labels = vacc_status
    ) +
    ggplot2::theme_classic() + {
    if (cumulative) {
      ggplot2::ylab("Cumulative hazard")
    } else {
      ggplot2::ylab("Survival probability")
    }
  } +
    ggplot2::xlab("Time to event (Days)") +
    ggplot2::labs(colour = "Vaccine Status") +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                   axis.title = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_text(size = 15),
                   legend.text = ggplot2::element_text(size = 13))
  return(plt)
}
