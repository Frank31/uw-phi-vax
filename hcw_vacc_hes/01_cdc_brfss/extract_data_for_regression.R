# 0X_prep_data_for_regression_model.R
# Francisco Rios Casas
# March 9, 2023

# Purpose: Visualize local-level trends in vaccination coverage

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# prep density of population
density_data <- read_xlsx(path=paste0(data_folder, "raw_data/census_bureau/population-density-data-table.xlsx"))

# re-name columns of interest
names(density_data)[1] <- "area"
names(density_data)[2] <- "res_pop_2020"
names(density_data)[3] <- "pop_density_2020"

# subset columns of interest
density_data <- density_data[,1:3]

# remove rows that are missing key information
density_data <- density_data %>% filter(!is.na(pop_density_2020)) %>%
  filter(!is.na(area)) %>%
  filter(area!="United States1")

# save the prepped data
saveRDS(density_data, file = paste0(data_folder, "prepped_data/census_bureau/prepped_population_density.rds" ))