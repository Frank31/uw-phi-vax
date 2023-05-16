# 04_extract_county_covid_data.R
# Francisco Rios Casas
# April 27, 2023

# Purpose: Loop that extracts all variables necessary for analysis at the county level for select states

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# Use API to extract the data
library(RSocrata)
library(httr)
library(jsonlite)

# load excel file with list of states to extract
state_list <- read_xlsx(paste0(data_folder, "documentation/cms_healthcare_worker/potential_states_of_interest.xlsx")) %>%
  filter(state %in% c("GA", "MA", "CO"))

todays_date <- "27April2023"

# use loop to extract covid data for three states
for (i in 1:length(state_list$state)){
  
  # uncomment to troubleshoot
  # i <- 1
  
  # set parameters of interest
  date <- "2023-02-22T00:00:00.000"
  state <- state_list$state[i]
  state_name <- state_list$state_name[i]
  
  base_url     <- "https://data.cdc.gov/resource/8xkx-amqh.json?"
  date_filter  <- "date="
  state_filter <- "recip_state="
  
  # print a statement so you know progress
  print(paste0("Now connecting to API for ", state_name, "."))
  
  # create a final URL
  final_url <- paste0(base_url, date_filter, date, "&", state_filter, state)
  # token <- "jwmlVFPrNopUiPRI3YG99u4Gx"
  
  # create a path to the data
  res = GET(final_url)
  
  # convert to data structure
  data = fromJSON(rawToChar(res$content))
  
  # subset columns of interest
  subset <- data %>% select(date, recip_county, recip_state,
                            series_complete_18pluspop_pct)
  
  # save the subset data in the prepped data folder
  write_rds(subset, paste0(data_folder, "prepped_data/cdc_covid_vax_tracker/", state, "_county_level_data_", todays_date, ".RDS"))
  write.csv(subset, paste0(data_folder, "prepped_data/cdc_covid_vax_tracker/", state, "_county_level_data_", todays_date, ".csv"), row.names = FALSE)
  
  # print statement to give update
  print(paste0(state_name, " data saved in prepped data folder. "))
}