data("cohortdata")
names(cohortdata)
head(cohortdata)
str(cohortdata)

start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")
date_interval <- c(start_cohort, end_cohort)


cohort_coverage <- function(data,
                            vaccine_date_col,
                            unit = "day",
                            date_interval = FALSE) {
  # Create continuous date column
  # For fixed intervals, use date_intervales
  # In other case use min and max of data
  if (!isFALSE(date_interval)) {
    start <- date_interval[1]
    end <- date_interval[2]
  } else {
    start <- min(data[[vaccine_date_col]], na.rm = TRUE)
    end <- max(data[[vaccine_date_col]], na.rm = TRUE)
  }
  if (unit == "month") {
    # Asign first day of the month
    st <- as.POSIXlt(start)
    st$mday <- 1
    start <- as.Date(st)
  } else if (unit == "year") {
    # Asign first day of the month and first day of the year
    st <- as.POSIXlt(start)
    st$mon <- 0
    st$mday <- 1
    start <- as.Date(st)
  }
  dates <- seq(from = start, to = end, by = unit)
  coverage <- data.frame(date  = dates)

  count <- as.data.frame(table(cut(data[[vaccine_date_col]], breaks = unit)))
  names(count) <- c("date", "doses")
  count$date <- as.Date(count$date)
  coverage <- merge(x = coverage, y = count,
    by = "date", all.x = TRUE
  )
  coverage$doses[is.na(coverage$doses)] <- 0
  coverage$cum_doses <- cumsum(coverage$doses)
  coverage$coverage <- coverage$cum_doses / nrow(data)
  return(coverage)
}

plot_coverage <- function(data,
                          vaccine_date_col,
                          unit = "day",
                          date_interval = FALSE,
                          cumulative = FALSE) {
  colors <- c("c1" = "steelblue", "c2" = "#6d5aa1")
  lbls <- c("c1" = "Doses", "c2" = "Coverage")

  coverage <- cohort_coverage(data = data,
    vaccine_date_col = vaccine_date_col,
    unit = unit,
    date_interval = date_interval
  )
  if (cumulative) {
    coverage$dose_plot <- coverage$cum_dose
  } else {
    coverage$dose_plot <- coverage$dose
  }

  plt <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = coverage$date,
        y = coverage$dose_plot,
        fill = "c1"
      ), stat = "identity", alpha = 0.6
    ) +
    ggplot2::geom_line(ggplot2::aes(x = coverage$date,
        y = coverage$coverage * max(coverage$dose_plot),
        color = "c2", group = 1
      ), linewidth = 1.3, linetype = 2
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste0("Doses per ", unit)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      sec.axis = ggplot2::sec_axis(~. / max(coverage$dose_plot),
        labels = scales::percent,
        name = "Percentage of coverage"
      )
    ) +
    ggplot2::scale_fill_manual(name = "",
      values = colors,
      labels = lbls
    ) +
    ggplot2::scale_color_manual(name = "",
      values = colors,
      labels = lbls
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
                   axis.text = ggplot2::element_text(size = 13),
                   axis.title = ggplot2::element_text(size = 15),
                   legend.title = ggplot2::element_text(size = 15),
                   legend.text = ggplot2::element_text(size = 13))
  return(plt)
}

plt1 <- plot_coverage(data = cohortdata,
  vaccine_date_col = "vaccine_date_1",
  unit = "day",
  date_interval = FALSE,
  cumulative = FALSE
)