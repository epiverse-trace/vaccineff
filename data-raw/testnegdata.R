## code to prepare `testneg` dataset

library(dplyr)

# Link to data
data_url <- "https://datadryad.org/stash/downloads/file_stream/24421" # nolint

# Reading the data in using the url
ILI_vacc_data <- read.csv(data_url)

# Selecting columns of interest

ILI_vacc_data <- ILI_vacc_data %>%
  select(month, year,
    vacc_status = Vaccine12mo_yn,
    pcr_result = RealTimePCRforInfluenzaABAFRIMS,
    rapid_result = Sec4_Ifrapid, treatment = Sec2_Medic_01_specify,
    sex = Gender, age = Age, collection_site = Collection_site
  )

# Date format

ILI_vacc_data$collection_date <- as.Date(paste(ILI_vacc_data$year,
  sprintf("%02d", ILI_vacc_data$month),
  1,
  sep = "-"
))

ILI_vacc_data <- ILI_vacc_data[,-c(1:2)]

# Changing sex names

ILI_vacc_data$sex <- ifelse(ILI_vacc_data$sex == "Male", "M", "F")

# Replacing blanks with NAs

ILI_vacc_data$treatment <- ifelse(ILI_vacc_data$treatment == " ",
  NA, ILI_vacc_data$treatment
)

# Invalid results to NA

ILI_vacc_data$rapid_result <- ifelse(ILI_vacc_data$rapid_result ==
                                       "Invalid result", NA,
                                     ILI_vacc_data$rapid_result)

# Reordering columns

testnegdata <- ILI_vacc_data %>%
  relocate(
    collection_date,
    collection_site,
    sex, age,
    treatment,
    pcr_result,
    rapid_result,
    vacc_status)


usethis::use_data(testnegdata, overwrite = TRUE)
