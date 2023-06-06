# 03_extract_covid_vaccination_trends.R
# Francisco Rios Casas
# March 16, 2023

# Purpose: create code to interact with medicare api; extract covid vaccination 
# among healthcare workers

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load packages 
library(httr)
library(jsonlite)
library(RSocrata)

# specify the token to access the data
token <- ""

# create the path to our data
url2020 <- "https://data.cdc.gov/resource/rh2h-3yt2.json?Date=2020-12-13"
covidDataFrame2020 <- read.socrata(url=url2020, 
                               app_token = token)

url2021 <- "https://data.cdc.gov/resource/rh2h-3yt2.json?Date=2021-03-08"
covidDataFrame2021 <- read.socrata(url=url2021, 
                                 app_token = token)

url2022 <- "https://data.cdc.gov/resource/rh2h-3yt2.json?Date=2022-03-08"
covidDataFrame2021 <- read.socrata(url=url2022, 
                                 app_token = token)

url2023 <- "https://data.cdc.gov/resource/rh2h-3yt2.json?Date=2023-03-08"
covidDataFrame2023 <- read.socrata(url=url2023, 
                                 app_token = token)

# write a sequence of all the years that 
years <- seq(2020, 2023)
data_matrix <- matrix(nrow = 4, byrow = FALSE)

# # save raw datasets
# for (i in length(years)){
#   year <- years[i]
#   saveRDS(covidData)
# }

saveRDS()