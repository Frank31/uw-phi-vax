# 03_get_coverage_estimates.R
# Francisco Rios Casas
# February 24 2023

# Purpose: estimate flu shot coverage in each state

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# states_with_data <- unique(BRFSS$`_STATE`)
years_with_data    <- seq(2016,2021)

# Loop through each dataset
for (i in 1:length(years_with_data)){

  # set the initial parameters of data to be prepped
  year      <- years_with_data[i]
  file_name <- paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_",year,".rds")
  
  print(paste0(i, " Now prepping ", year[i], " BRFSS Data"))
  
  # read in the file
  BRFSS <- read_rds(file_name)
  
  # create a vector of the states present in the dataset
  states_with_data <- unique(BRFSS$`_STATE`)
  
  # loop through each state to calculate the vaccination estimates
  for (i in 1:length(states_with_data)){
    
    # set the state to be analyzed
    state <- states_with_data[i]
    
    print(paste(i, " Now prepping State FIPS Code: ", state))
    
    # subset to the state of interest
    BRFSS_subset <- BRFSS %>% filter(`_STATE`== state)
    
    # Set options for allowing a single observation per stratum
    options(survey.lonely.psu = "adjust")
    
    # Create survey design
    brfssdsgn <- svydesign(
      id=~1,
      strata = ~`_STSTR`,
      weights = ~`_LLCPWT`,
      data = BRFSS_subset)
    
    # store the flu vaccine coverage within an object
    estimates <- svymean(~factor(FLUSHOT7),
                         brfssdsgn,
                         na.rm = TRUE)
    
    # data <- do.call(rbind, lapply(list(vacc2021), as.data.frame, row.names = FALSE))
    data <- as.data.frame(estimates)
    tmpData <- tibble::rownames_to_column(data, "factor")
    
    # add in necessary columns
    tmpData$STATE <- state
    tmpData$YEAR <- year
    
    # bind all the files together for 
    if (i == 1){
      extracted_estimates <- tmpData
    } else {
      extracted_estimates <- plyr::rbind.fill(extracted_estimates, tmpData)
    }
    
    # save the extracted data in the prepped data folder
    saveRDS(extracted_estimates, file = paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_",year,".rds"))
  }
}

# re-load all of the data that was just prepped
data16 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2016.rds"))
data17 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2017.rds"))
data18 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2018.rds"))
data19 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2019.rds"))
data20 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2020.rds"))
data21 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_2021.rds"))

# bind all the extracted estimates together
full_data <- do.call("rbind", list(data16, data17, data18, data19, data20, data21))

# save the full dataset
saveRDS(full_data, paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_all_years.rds"))

# for each year estimate the flu shot coverage rates
# BRFSS <- read_rds(paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_2021.rds"))

# load the estimation areas table

# rbind()
# if (i == 1){
#   bound_estimates <- extracted_estimates
# } else {
#   bound_estimates <- plyr::rbind.fill(bound_estimates, extracted_estimates)
# }

# for (i in 1:length(states)) {
#   state      <- states_with_data[i]
#   year       <- years_with_data[i]
#   file_name  <- 
#   
#   
#   
#   # subset to the state of interest
#   BRFSS_subset <- BRFSS %>% filter(`_STATE`==states[i])
#   
#   # Set options for allowing a single observation per stratum
#   options(survey.lonely.psu = "adjust")
#   
#   # Create survey design
#   brfssdsgn <- svydesign(
#     id=~1,
#     strata = ~`_STSTR`,
#     weights = ~`_LLCPWT`,
#     data = BRFSS_subset)
#   
#   # store the flu vaccine coverage within an object
#   estimates <- svymean(~factor(FLUSHOT7),
#                       brfssdsgn,
#                       na.rm = TRUE)
#   
#   # data <- do.call(rbind, lapply(list(vacc2021), as.data.frame, row.names = FALSE))
#   test <- as.data.frame(estimates)
#   test <- tibble::rownames_to_column(test, "factor")
#   
#   # add in necessary columns
#   test$STATE <- states[i]
#   test$YEAR <- year
#   
#   # bind all the files together for 
# 
#   
#   
# }
# # subset to Louisiana(22)
# BRFSS <- BRFSS[BRFSS$`_STATE` == "22", ]
# 
# # Set options for allowing a single observation per stratum
# options(survey.lonely.psu = "adjust")
# 
# # Create survey design
# brfssdsgn <- svydesign(
#   id=~1,
#   strata = ~`_STSTR`,
#   weights = ~`_LLCPWT`,
#   data = BRFSS)
# 
# # calculate average vaccination coverage
# # svymean(~FLUSHOT7,
# #         brfssdsgn,
# #         na.rm = TRUE)
# 
# # calculate percent in each flushot category
# vacc2021 <- svymean(~factor(FLUSHOT7),
#                     brfssdsgn,
#                     na.rm = TRUE)
# 
# # -------
# # Example code
# # -------
# 
# # Read in BRFSS data
# load("\\BRFSS\\BRFSS.rdata")
# 
# # Subset the data for Louisiana(22)
# BRFSS <- BRFSS[BRFSS$state == 22, ]
# # Set options for allowing a single observation per stratum
# options(survey.lonely.psu = "adjust")
# # Create survey design
# brfssdsgn <- svydesign(
#   id=~1,
#   strata = ~ststr,
#   weights = ~llcpwt,
#   data = BRFSS)
# # calculate average number of physical healthy days
# svymean(~physhlth,
#         brfssdsgn,
#         na.rm = TRUE)
# # calculate percent in each arthritis category
# svymean(~factor(havarth5),
#         brfssdsgn,
#         na.rm = TRUE)