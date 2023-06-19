source("R/vaccineff_package.R")
source("R/all_data_prep.R")
cohortdata <- readRDS("data/cohortdata.rds")
head(cohortdata)
str(cohortdata)
cohortdata$vaccine.date.1 <- as.Date(cohortdata$vaccine.date.1)
cohortdata$vaccine.date.2 <- as.Date(cohortdata$vaccine.date.2)
cohortdata$death.date <- as.Date(cohortdata$death.date)
cohortdata$age.group <- get_age_group(cohortdata, "age", 80, 9)
table(cohortdata$age.group)
cohortdata$vaccine.status <- set_status(cohortdata, 
                                        c("vaccine.date.1", "vaccine.date.2"), 
                                        status = c("v", "u"))
table(cohortdata$vaccine.status)

source("R/coh_data_wrangling.R")
cohortdata$immunization.death <- get_immunization_date(cohortdata, "death.date", 1, 1,
                                                        c("vaccine.date.1", "vaccine.date.2"), 
                                                        as.Date("2021-12-31"), take_first = FALSE)

head(cohortdata)

head(cohortdata %>% filter(!(is.na(death.date))))
head(cohortdata %>% filter(is.na(death.date)))