#' @title Calculate Vaccine Coverage
#'
#' @description This function returns the vaccination coverage of a dose along
#' the cohort study. The coverage can be calculated grouped by year, day, and
#' month. This must be specified in the parameter `unit`. If there are no
#' records for some dates, the function assigns 0 instead of NA to calculate
#' the cumulative coverage.
#'
#' @param data_set `data.frame` with cohort information.
#' @param vacc_date_col Name of the column(s) that contain the vaccine date to
#' calculate the coverage.
#' @param unit Aggregation unit, must be either "year", "month", or "day".
#' @param date_interval If NULL, the function calculates the coverage interval
#' based on the min() and max() of the `vacc_date_col`. It is also possible to
#' pass a custom date interval to truncate or expand the date interval (see
#' example).
#' @return `data.frame` with the number of vaccine doses per date, cumulative
#' count of doses, and vaccine coverage.
#' @keywords internal

coh_coverage <- function(data_set,
                         vacc_date_col,
                         unit = c("day", "month", "year"),
                         date_interval = NULL) {
  # Sanity check for developers
  unit <- match.arg(unit, several.ok = FALSE)
  checkmate::assert_string(
    unit
  )

  # Create continuous date column
  # For fixed intervals, use date_intervals
  # In other case use min and max of data_set
  if (is.null(date_interval)) {
    start <- min(data_set[[vacc_date_col]], na.rm = TRUE)
    end <- max(data_set[[vacc_date_col]], na.rm = TRUE)
  } else {
    start <- date_interval[1]
    end <- date_interval[2]
  }
  start <- as.Date(trunc(as.POSIXlt(start), units = unit))
  dates <- seq(from = start, to = end, by = unit)
  coverage <- data.frame(date  = dates)

  count <- as.data.frame(
    table(
      cut(data_set[[vacc_date_col]], breaks = unit),
      dnn = "date"
    ),
    responseName = "doses"
  )
  count$date <- as.Date(count$date)
  coverage <- merge(x = coverage, y = count,
    by = "date", all.x = TRUE
  )
  coverage$doses[is.na(coverage$doses)] <- 0
  coverage$cum_doses <- cumsum(coverage$doses)
  coverage$coverage <- coverage$cum_doses / nrow(data_set)
  return(coverage)
}

#' @title Plot Vaccine Coverage
#'
#' @description This function returns a plot of the vaccine coverage or the
#' cumulative coverage (if cumulative = TRUE). The return is a 2-axis `ggplot2`
#' element with the number of vaccines per date on the left axis and the
#' coverage per date on the right axis. When a matching routine is performed,
#' the left axis also accounts for the doses of the matched cohort.
#' @inheritParams coh_coverage
#' @param vaccineff_data Object of the class `vaccineff_data` with
#' vaccineff data.
#' @param cumulative If `TRUE`, returns the cumulative number of doses over the
#' time window.
#' @return 2-axis ggplot2 plot of vaccine coverage and daily doses.
#' @keywords internal

plot_coverage <- function(vaccineff_data,
                          date_interval = NULL,
                          cumulative = FALSE) {
  # Due to an update on scales >1.3.0 unit = c("day", "month", "year")
  # is now deprecated and fixed to "day"
  unit <- "day"
  tags <- linelist::tags(vaccineff_data$cohort_data)

  coverage <- coh_coverage(
    data_set = vaccineff_data$cohort_data,
    vacc_date_col = tags$vacc_date_col,
    unit = unit,
    date_interval = date_interval
  )

  if (cumulative) {
    coverage$dose_plot <- coverage$cum_doses
  } else {
    coverage$dose_plot <- coverage$doses
  }

  plt <- ggplot2::ggplot() +

    ggplot2::geom_col(data = coverage,
      ggplot2::aes(
        x = .data$date,
        y = .data$coverage * max(.data$dose_plot),
        fill = "Coverage"
      ),
      alpha = 0.68
    ) +
    ggplot2::geom_col(data = coverage,
      ggplot2::aes(
        x = .data$date,
        y = .data$dose_plot,
        fill = "Total doses"
      ),
      alpha = 0.47
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
    )

  if (!is.null(vaccineff_data$matching)) {
    coverage_match <- coh_coverage(
      data_set = vaccineff_data$matching$match,
      vacc_date_col = tags$vacc_date_col,
      unit = unit,
      date_interval = date_interval
    )

    colors <- c(
      "Total doses" = "steelblue",
      "Matched doses" = "green",
      Coverage = "mediumpurple"
    )

    if (cumulative) {
      coverage_match$dose_plot <- coverage_match$cum_doses
    } else {
      coverage_match$dose_plot <- coverage_match$doses
    }

    plt <- plt + ggplot2::geom_col(
      data = coverage_match,
      ggplot2::aes(
        x = .data$date,
        y = .data$dose_plot,
        fill = "Matched doses"
      ),
      alpha = 0.3
    ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(alpha = c(0.68, 0.47, 0.3))
        )
      )
  } else {
    colors <- c(
      "Total doses" = "steelblue",
      Coverage = "mediumpurple"
    )
    plt <- plt +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(alpha = c(0.68, 0.47, 0.3))
        )
      )
  }

  plt <- plt +
    ggplot2::scale_fill_manual(
      name = NULL, values = colors
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "top")

  return(plt)
}
