data("cohortdata")
head(cohortdata)
source("R/coh_data_wrangling.R")

start_cohort <- as.Date("2044-01-01")
end_cohort <- as.Date("2044-12-31")


cohortdata$immunization_death <-
  get_immunization_date(
    data = cohortdata,
    outcome_date_col = "death_date",
    outcome_delay = 0,
    immunization_delay = 14,
    vacc_date_col = c("vaccine_date_1", "vaccine_date_2"),
    end_cohort = end_cohort,
    take_first = FALSE
  )

cohortdata$vaccine_status <- set_status(
  data = cohortdata,
  col_names = "immunization_death",
  status = c("v", "u")
)

cohortdata$death_status <- set_status(
  data = cohortdata,
  col_names = "death_date"
)

cohortdata$time_to_death <- get_time_to_event(
  data = cohortdata,
  outcome_date_col = "death_date",
  start_cohort = start_cohort,
  end_cohort = end_cohort,
  start_from_immunization = FALSE
)

data <- cohortdata
time_to_event <- "time_to_death"
outcome_status <- "death_status"
vaccine_status <- "vaccine_status"


extract_surv_model <- function(model) {
  days <- end_cohort - start_cohort
  tte <- seq(0, as.numeric(days) - 1, by = 1)
  res <- summary(model, times = tte, scale = 1)
  cols <- lapply(c(2:6, 8:16), function(x) res[x])
  tbl <- do.call(data.frame, cols)
  tbl$date <- tbl$time + start_cohort
  return(tbl)
}


status <- c("v", "u")
color <- c("steelblue", "darkred")

#Extract data from survival element
results <- extract_surv_model(km_model)
#Filter data by status
conn1 <- paste0("data[[vaccine_status]]=", status[1])
results_1 <- results[results$strata == conn1, ]

conn2 <- paste0("data[[vaccine_status]]=", status[2])
results_2 <- results[results$strata == conn2, ]

plt <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = results_1,
    ggplot2::aes(x = time,
      ymin = lower, ymax = upper
    ),
    fill = color[1], alpha = 0.2
  ) +
  ggplot2::geom_line(data = results_1,
    ggplot2::aes(x = time, y = surv),
    color = color[1]
  ) +
  ggplot2::geom_ribbon(data = results_2,
    ggplot2::aes(x = time,
      ymin = lower, ymax = upper
    ),
    fill = color[2], alpha = 0.2
  ) +
  ggplot2::geom_line(data = results_2,
    ggplot2::aes(x = time,
      y = surv
    ),
    color = color[2]
  ) +
  ggplot2::theme_classic() +
  ggplot2::ylab("Survival probability") +
  ggplot2::xlab("Time to event (Days)") +
  ggplot2::labs(colour = "Vaccine Status")