library(survival)
#' Function to estimate the vaccine effectiveness based on the vaccination 
#' status.
#' The function relies on the implementation of the Cox model for proportional
#' hazards on survival package. It returns a dataframe with the summary of the
#' estimation that includes the value of the hazard ratio, the vaccine
#' effectiveness, and the result of the test for the Proportional Hazards 
#' hypothesis. If the test is rejected, it is recomended to try other 
#' statistical strategies, e.g. stratifying the dataset by confounders or 
#' including time-dependent variables.
#' @param data dataset with at least one column to generate the status
#' @param outcome_status_col name of the column containin status of the 
#' event (most be a binary column)
#' @param time_to_event_col name of the column containing the time-to-event 
#' @param status_vacc_col name of the column containing the vaccination 
#' status
#' @param p_thr p-value to test Proportional Hazards Hypothesis
#' @return age_group
#' @examples
#' \dontrun{
#' data("cohortdata")
#' 
#' cohortdata$immunization.death <- get_immunization_date(cohortdata, 
#' "death.date", 0, 14,
#' c("vaccine.date.1", "vaccine.date.2"), 
#' "2021-12-31", take_first = FALSE)
#' "2021-12-31",
#' take_first = FALSE)
#' 
#' cohortdata$vaccine.status <- set_status(cohortdata, 
#' c("immunization.death"),
#' status = c("v", "u"))
#' 
#' cohortdata$death.status <- set_status(cohortdata,
#' c("death.date"))
#' cohortdata$time.to.death <- get_time_to_event(cohortdata, "death.date",
#' "2021-01-01", "2021-12-31",
#' FALSE) 
#' 
#' coh_eff_noconf(cohortdata,
#' "death.status",
#' "time.to.death",
#' "vaccine.status")
#' }
#' @export
coh_eff_noconf <- function(data, outcome_status_col,
                        time_to_event_col,
                        status_vacc_col,
                        p_thr = 0.05) {
    cx <- coxph(Surv(data[[time_to_event_col]], data[[outcome_status_col]])
                    ~ data[[status_vacc_col]])
    test <- cox.zph(cx)    
    hr <- c(round(exp(coef(cx)), 4)[1])
    ci025 <- c(round(exp(confint(cx)), 4)[1])
    ci975 <- c(round(exp(confint(cx)), 4)[2])
    p <- test$table[5]
    p_value <- c(format(p, digits = 3))
    if (p < p_thr) {
        ph <- c("reject")
    } else {
        ph <- c("accept")
    }

    df_summ <- data.frame(
                HR = hr,
                HR.low = ci025,
                HR.high = ci975,
                V.eff = 1 - hr,
                V.eff.low = 1 - ci975,
                V.eff.high = 1 - ci025,
                PH = ph,
                p.value = p_value
                )
    return(df_summ)
}