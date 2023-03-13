# 0X_get_coverage_estimates_brfss_smart.R
# Francisco Rios Casas
# March 2, 2023

# Purpose: estimate flu shot coverage in each metro/micro area

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# create vector with all the years of data
years_with_data    <- seq(2016,2021)

# Loop through each dataset
for (i in 1:length(years_with_data)){
  
  # set the initial parameters of data to be prepped
  year      <- years_with_data[i]
  file_name <- paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_data_",year,".rds")
  
  print(paste0(i, " Now prepping ", year, " BRFSS SMART Data"))
  
  # read in the file
  BRFSS <- read_rds(file_name)
  
  # create a vector of the states present in the dataset
  areas_with_data <- unique(BRFSS$`_MMSA`)
  
  # loop through each state to calculate the vaccination estimates
  for (i in 1:length(areas_with_data)){
    
    # set the state to be analyzed
    area <- areas_with_data[i]
    
    print(paste(i, " Now prepping Area Statistical Code: ", area))
    
    # subset to the state of interest
    BRFSS_subset <- BRFSS %>% filter(`_MMSA`== area)
    
    # Set options for allowing a single observation per stratum
    options(survey.lonely.psu = "adjust")
    
    # Create survey design
    brfssdsgn <- svydesign(
      id=~1,
      strata = ~`_STSTR`,
      weights = ~`_MMSAWT`,
      data = BRFSS_subset)
    
    # store the flu vaccine coverage within an object
    estimates <- svymean(~factor(FLUSHOT7),
                         brfssdsgn,
                         na.rm = TRUE)
    
    # data <- do.call(rbind, lapply(list(vacc2021), as.data.frame, row.names = FALSE))
    data <- as.data.frame(estimates)
    tmpData <- tibble::rownames_to_column(data, "factor")
    
    # add in necessary columns
    tmpData$AREA <- area
    tmpData$YEAR <- year
    
    # bind all the files together for 
    if (i == 1){
      extracted_estimates <- tmpData
    } else {
      extracted_estimates <- plyr::rbind.fill(extracted_estimates, tmpData)
    }
    
    # save the extracted data in the prepped data folder
    saveRDS(extracted_estimates, file = paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_",year,".rds"))
  }
}

# re-load all of the data that was just prepped
data16 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2016.rds"))
data17 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2017.rds"))
data18 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2018.rds"))
data19 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2019.rds"))
data20 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2020.rds"))
data21 <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_2021.rds"))

# bind all the extracted estimates together
full_data <- do.call("rbind", list(data16, data17, data18, data19, data20, data21))

# save the full dataset
saveRDS(full_data, paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_all_years.rds"))
