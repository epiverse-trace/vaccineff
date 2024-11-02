#' @title Plot Log-Log Test for Proportional Hazards Hypothesis
#'
#' @description This function uses the return from the Cox model
#' to create a log-log plot.
#' @importFrom rlang .data
#' @param cox_model_prediction Prediction from Cox model.
#' @return Log-log plot.
#' @keywords internal

plot_loglog <- function(cox_model_prediction) {
  # strata levels were defined as order actors to extract like this
  vaccinated_status <- levels(cox_model_prediction$strata)[1]
  unvaccinated_status <- levels(cox_model_prediction$strata)[2]
  # Plot colors
  vaccinated_color <- "steelblue"
  unvaccinated_color <- "darkred"
  plt <- ggplot2::ggplot(data = cox_model_prediction) +
    ggplot2::geom_step(ggplot2::aes(x = .data$logtime,
                                    y = .data$loglog,
                                    color = .data$strata)
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Log[Time to event] (Days)",
                  y = "-Log[-Log[Surv.]]") +
    ggplot2::labs(colour = "Vaccine Status") +
    ggplot2::scale_color_manual(
      name = "Vaccine Status",
      values = c(vaccinated_color, unvaccinated_color),
      labels = c(vaccinated_status, unvaccinated_status)
    )
  return(plt)
}

#' @title Plot the Survival Probability Based on the Kaplan-Meier Model
#'
#' @description This function relies on the implementation of the Kaplan-Meier
#' model from the package `{survival}`. It returns a plot of the Survival
#' Probability or the Cumulative Hazard (if cumulative = TRUE).
#' The return is a ggplot2 element of the curves with 95% C.I. It is possible
#' to manipulate the colors, labels, legend, and most of the graphic elements.
#'
#' @importFrom rlang .data
#' @param km Kaplan-Meier estimation created with `km_model`.
#' @param percentage If `TRUE`, returns probability in percentage.
#' @param cumulative If `TRUE`, returns cumulative incidence
#' @return `{ggplot2}` object with plot of survival or cumulative incidence.
#' @keywords internal

plot_survival <- function(km,
                          percentage = TRUE,
                          cumulative = FALSE) {

  if (cumulative) {
    km$plot <- km$cumincidence
    km$plot_lower <- km$cumincidence_lower
    km$plot_upper <- km$cumincidence_upper
  } else {
    km$plot <- km$surv
    km$plot_lower <- km$upper
    km$plot_upper <- km$lower
  }

  # strata levels were defined as order actors to extract like this
  vaccinated_status <- levels(km$strata)[1]
  unvaccinated_status <- levels(km$strata)[2]
  # Plot colors
  vaccinated_color <- "steelblue"
  unvaccinated_color <- "darkred"

  colors <- c(vaccinated_color, unvaccinated_color)
  vacc_status <- c(vaccinated_status, unvaccinated_status)

  plt <-
    ggplot2::ggplot(data = km) +
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
        cumulative, yes = "Cumulative incidence", no = "Survival probability"
      )
    ) +
    ggplot2::xlab("Time to event (Days)") +
    ggplot2::labs(colour = "Vaccine Status")

  return(plt)
}
