source("R/vaccineff_package.R")
source("R/all_data_prep.R")
source("R/coh_data_wrangling.R")
cohortdata <- readRDS("data/cohortdata.rds")
head(cohortdata)
str(cohortdata)

cohortdata$age.group <- get_age_group(cohortdata, "age", 80, 9)
table(cohortdata$age.group)

cohortdata$vaccine.status <- set_status(cohortdata, 
                                        c("vaccine.date.1", "vaccine.date.2"), 
                                        status = c("v", "u"))
table(cohortdata$vaccine.status)

source("R/coh_data_wrangling.R")
cohortdata$immunization.death <- get_immunization_date(cohortdata, "death.date", 0, 14,
                                                        c("vaccine.date.1", "vaccine.date.2"), 
                                                        "2021-12-31", take_first = FALSE)
head(cohortdata)
head(cohortdata %>% filter(!(is.na(death.date))))
head(cohortdata %>% filter(is.na(death.date)))

source("R/coh_data_wrangling.R")
cohortdata$time.to.death <- get_time_to_event(cohortdata, "death.date",
                                            "2021-01-01", "2021-12-31", 
                                            TRUE, "immunization.death") 
head(cohortdata)
head(cohortdata %>% filter(!(is.na(death.date))))

source("R/coh_data_wrangling.R")
cohortdata$immunization.dose <- get_immunization_dose(cohortdata, "immunization.death", 
                                                        c("vaccine.date.1", "vaccine.date.2"),
                                                        immunization_delay = 14)
head(cohortdata)

source("R/coh_data_wrangling.R")
cohortdata$immunization.vaccine <- get_immunization_vaccine(cohortdata, "immunization.death", 
                                                            c("vaccine.date.1", "vaccine.date.2"),
                                                            c("vaccine.1", "vaccine.2"),
                                                            immunization_delay = 14)

head(cohortdata)

write.csv(cohortdata, "dolab/temp.csv", row.names = FALSE)