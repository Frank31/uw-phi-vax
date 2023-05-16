# 02_prep_healthcare_worker_covid_data.R
# Francisco Rios Casas
# April 4, 2023

# Purpose: visualize the patterns of data availability and trends in select states 
# among healthcare workers in nursing facilities

# set the working directory at the root of this repository


# Source the set-up file
source("./01_set_up_file.R")

# load excel file with list of states to extract
state_list <- read_xlsx(paste0(data_folder, "documentation/cms_healthcare_worker/final_states_of_interest.xlsx"))

# load the data files and then merge all of the data together

for (i in 1:length(state_list$state)){
  
  # uncomment to troubleshoot
  # i <- 1
  
  # set parameters of interest
  date <- "2023-02-26"
  state <- state_list$state[i]
  state_name <- state_list$state_name[i]
  
  # add print statement
  print(paste0("Now loading the data for ", state_name))
  
  tmpData <- readRDS(paste0(data_folder, "prepped_data/cms_healthcare_worker/", state, "_nursing_home_data_", date, ".RDS"))
  
  # add print statement
  print(paste0("Now appending dataset id: ", i))
  if (i == 1){
    extracted_estimates <- tmpData
  } else {
    extracted_estimates <- plyr::rbind.fill(extracted_estimates, tmpData)
  }
}

# save the combined dataset
saveRDS(extracted_estimates, file = paste0(data_folder, "prepped_data/cms_healthcare_worker/_merged_shortlist_nursing_home_data_", date, ".RDS"))
