# 06_extract_brfss_smart_data.R
# Francisco Rios Casas
# March 1 2023

# Purpose: Estimate the state's coverage using region, population size, and wealth of the state

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# filter the list of data to prep
file_list <- file_list %>% filter(data_source=="cdc_brfss_smart")

# loop through each file to extract the right variables
for (i in 1:nrow(file_list)){
  
  # set unique parameters
  year <- file_list$year[i]
  
  # layout_file <- paste0("variable_layout_", year, ".csv")
  raw_file <- file_list$file_name[i]
  
  # Print statement so you know which file it broke on
  print(paste0(i, " Now Prepping: ", year," CDC BRFSS SMART Data "))
  
  # load the dataset with the BRFSS responses
  responses <- read_xpt(
    file = paste0(data_folder, "raw_data/cdc_brfss_smart/", raw_file)
  )
  
  # each year requires some re-coding of variables
  # GOING TO TRY TO USE THE SAME RE-CODE AS LARGER BRFSS
  
  if (year == 2021) {
    
    # re-code income to be similar to the previous years
    data_subset <- responses %>%
      # income variable in 2021 was modified
      mutate(INCOME2 = case_when(
        INCOME3 == 9  ~ 8,
        INCOME3 == 9  ~ 8,
        INCOME3 == 11 ~ 8,
        TRUE ~ INCOME3 ))
  } else if (year == 2020){
    
    data_subset <- responses %>%
      # CHCCOPD3 variable was re coded
      mutate(CHCCOPD3 = CHCCOPD2)
  } else if (year == 2019){
    
    data_subset <- responses %>%
      # CHCCOPD3 variable was re coded
      mutate(CHCCOPD3 = CHCCOPD2)
  } else if (year == 2018){
    
    data_subset <- responses %>%
      mutate(FLUSHOT7 = FLUSHOT6) %>%
      mutate(CHCKDNY2 = CHCKDNY1) %>%
      mutate(CHCCOPD3 = CHCCOPD1) %>%
      mutate(BIRTHSEX = SEX1) %>%
      mutate(ADDEPEV3 = ADDEPEV2) %>%
      mutate(DIABETE4 = DIABETE3) %>%
      mutate(CHCKDNY2 = CHCKDNY1) %>%
      mutate(`_SEX`   = SEX1)
  } else if (year == 2017){
    
    data_subset <- responses %>%
      mutate(FLUSHOT7 = FLUSHOT6) %>%
      mutate(CHCKDNY2 = CHCKIDNY) %>%
      mutate(CHCCOPD3 = CHCCOPD1) %>%
      mutate(BIRTHSEX = SEX) %>% 
      mutate(ADDEPEV3 = ADDEPEV2) %>%
      mutate(DIABETE4 = DIABETE3) %>%
      mutate(CHCKDNY2 = CHCKIDNY) %>%
      mutate(`_SEX`   = SEX)
  } else if (year == 2016){
    
    data_subset <- responses %>% 
      mutate(FLUSHOT7 = FLUSHOT6) %>%
      mutate(CHCCOPD3 = CHCCOPD1) %>%
      mutate(BIRTHSEX = SEX) %>%
      mutate(ADDEPEV3 = ADDEPEV2) %>%
      mutate(DIABETE4 = DIABETE3) %>%
      mutate(CHCKDNY2 = CHCKIDNY) %>%
      mutate(`_SEX`   = SEX)
  }
  
  # subset the final columns of interest
  data_subset <- data_subset %>% 
    select(
      
      # domain
      `_MMSA`,
      
      # unique identifiers for the metro/micropolitan statistical area
      MMSANAME,
      
      # vaccine variable
      FLUSHOT7, `_MMSA`,
      
      # demographic variables
      EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_PRACE1`,
      `_SEX`,
      
      # chronic health disease conditions
      CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
      CHCSCNCR, CHCOCNCR, CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,
      
      # sample weights
      `_MMSAWT`,
      
      # Sample Design Stratification Variable
      `_STSTR`)
  
  # save dataset stratified by year as CSV and as RDS File
  write.csv(data_subset, file = paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_data_", year, ".csv"), row.names = FALSE)
  saveRDS(data_subset,   file = paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_data_", year, ".RDS"))
}
