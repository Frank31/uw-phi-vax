# 02_extract_healthcare_worker_covid_data.R
# Francisco Rios Casas
# March 2, 2023

# Purpose: create code to interact with medicare api; extract covid vaccination 
# among healthcare workers

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load packages 
library(httr)
library(jsonlite)

# load excel file with list of states to extract
state_list <- read_xlsx(paste0(data_folder, "documentation/cms_healthcare_worker/potential_states_of_interest.xlsx"))

for (i in 1:length(state_list$state)){
  
  # uncomment to troubleshoot
  # i <- 1
  
  # set parameters of interest
  date <- "2023-02-26"
  state <- state_list$state[i]
  state_name <- state_list$state_name[i]
  
  # print a statement so you know progress
  print(paste0("Now connecting to API for ", state_name, "."))
  
  # create url
  base_url     <- "https://data.cms.gov/data-api/v1/dataset/137f90cb-ac53-4b3d-8358-e65cf64e03d3/data?"
  date_filter  <- "filter[week_ending]="
  state_filter <- "filter[provider_state]="
  
  final_url <- paste0(base_url, date_filter, date, "&", state_filter, state)
  
  # create a path to the data
  res = GET(final_url)
  
  # convert to data structure
  data = fromJSON(rawToChar(res$content))
  
  # subset columns of interest
  subset <- data %>% select(week_ending, federal_provider_number, provider_name, provider_address, provider_city, provider_state, provider_zip_code, county, 
                            percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time,
                            percentage_of_current_healthcare_personnel_who_received_a_completed_or_partial_covid_19_vaccination_at_any_time,
                            recent_percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time,
                            )
  
  # save the subset data in the prepped data folder
  write_rds(subset, paste0(data_folder, "prepped_data/cms_healthcare_worker/", state, "_nursing_home_data_", date, ".RDS"))
  write.csv(subset, paste0(data_folder, "prepped_data/cms_healthcare_worker/", state, "_nursing_home_data_", date, ".csv"), row.names = FALSE)
  
  # print statement to give update
  print(paste0(state_name, " data saved in prepped data folder. "))
}