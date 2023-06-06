# 0X_prep_data_for_regression_model.R
# Francisco Rios Casas
# March 9, 2023

# Purpose: Visualize local-level trends in vaccination coverage

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load the raw data
raw_data <- read_xlsx(paste0(data_folder, "raw_data/bea/lagdp1222.xlsx"))

# rename columns of interest
names(raw_data)[1] <- "location"
names(raw_data)[2] <- "2018"
names(raw_data)[3] <- "2019"
names(raw_data)[4] <- "2020"
names(raw_data)[5] <- "2021"
names(raw_data)[6] <- "rank_in_state"

# filter out only state-level data
raw_data <- raw_data %>% filter(rank_in_state=="--")

# subset columns of interest
raw_data <- raw_data %>% select(location, `2018`, `2019`, `2020`, `2021`)

# drop unnecessary rows
prepped_data <- raw_data %>% filter(`2021`!="(NA)") %>%
  filter(location!="United States") %>%
  # rename the columns 
  rename(gdp_2018 = `2018`,
         gdp_2019 = `2019`,
         gdp_2020 = `2020`,
         gdp_2021 = `2021`)

# save the file
saveRDS(prepped_data, file = paste0(data_folder, "prepped_data/bea/gdp_data.rds"))

