# XX_create_dataset_for_predictions.R
# Francisco Rios Casas
# April 11 2023

# Purpose: this file will organize data for Alex's analyses
# the final dataset should have the following variables:
# year, location, number children (unweighted), number of children vaccinated (unweighted),
# number of children (weighted total), number of children (unweighted total)

# load set up file
source("./01_set_up_file.R")

# load unweighted data first
data <- read_rds(file=paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_2021.rds"))

# subset columns of interest
data_test <- data %>% select(STATE, HPVI_ANY)

# add year
data_test$YEAR <- 2021

# county number of children vaccinated and number of children
counts <- data_test %>% count(STATE)

vaccine_status <- data_test %>% count(STATE, HPVI_ANY)

# re-shape vaccine_status data