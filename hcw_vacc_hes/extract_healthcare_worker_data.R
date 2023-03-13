# _extract the healthcare worker data.R
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

# create the path to our data
res = GET("https://data.cms.gov/data-api/v1/dataset/137f90cb-ac53-4b3d-8358-e65cf64e03d3/data?filter[week_ending]=2023-02-26&filter[provider_zip_code]=79912")

# convert to data strcuture
data = fromJSON(rawToChar(res$content))

# subset the columns we will use in our analyses
subset <- data %>% select(week_ending, federal_provider_number, provider_name, provider_address, provider_city, provider_state, provider_zip_code, county, 
  recent_percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time)

# save the subset data in the prepped data folder
# load the raw data
# data <- read.csv(paste0(data_folder, "/raw_data/medicare/COVID-19 Nursing Home Data 02.19.2023.csv"))

# filter out the variables of interest
# data_subset <- data %>% select()
# res = GET("http://api.open-notify.org/iss-pass.json",
#     query = list(lat = 40.7, lon = -74))


# data = fromJSON(rawToChar(res$content))

# # create a GET request
# res = GET("https://data.cms.gov/data-api/v1/dataset/89163e1e-827d-4da2-be8b-d9a60dfe4605/data",
#           query = list(week_ending = "2023-02-19"))

# rawToChar(res$content)
# data = fromJSON(rawToChar(res$content)) %>%
#   filter(week_ending == "2023-02-26")
# names(data)


# # covert character vector that contains the JSON structure
# res = GET("https://data.cms.gov/data-api/v1/dataset/137f90cb-ac53-4b3d-8358-e65cf64e03d3/data",
#           query = list(provider_city="El Paso"))
# 
# 
# # concvert to dataset
# data = fromJSON(rawToChar(res$content))

# select the columns I am interested in 