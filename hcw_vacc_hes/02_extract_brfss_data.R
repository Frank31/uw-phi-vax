# 02_extract_brfss_data.R
# Francisco Rios Casas
# February 16 2023

# Purpose: Loop that extracts all variables necessary for analysis

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# filter the list of data to prep
file_list <- file_list %>% filter(data_source=="cdc_brfss")

for (i in 1:nrow(file_list) ){
  
  # set unique parameters
  year <- file_list$year[i]
  layout_file <- paste0("variable_layout_", year, ".csv")
  raw_file <- file_list$file_name[i]
  
  # Print statement so you know which file it broke on
  print(paste0(i, " Now Prepping:  CDC BRFSS ", year))
  
  # load the file with the column information
  columns <- read.csv(paste0(data_folder, "documentation/brfss/variable_layouts/", layout_file))
  
  columns$File_Width <- sapply(1:nrow(columns), function(y) ifelse(y < nrow(columns), 
                                                                   columns$Starting_Column[y + 1] - columns$Starting_Column[y], 1))
  
  columns <- columns[columns$File_Width > 0,]
  
  # load the dataset with the BRFSS responses
  responses <- read_fwf(paste0(data_folder, "raw_data/cdc/", raw_file),
                        col_positions = fwf_widths(columns$File_Width, 
                                                   col_names = columns$Variable_Name),
                        show_col_types = FALSE)
  
  # each year requires some re-coding of variables
  
  if (year == 2021) {
    # re-code income to be similar to the previous years
    data_subset <- responses %>%
      # income variable in 2021 was modified
      mutate(INCOME2 = case_when(
        INCOME3 == "09" ~ "08",
        INCOME3 == "10" ~ "08",
        INCOME3 == "11" ~ "08",
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
      mutate(CHCKDNY2 = CHCKDNY1)
  } else if (year == 2017){
    
    data_subset <- responses %>%
      mutate(FLUSHOT7 = FLUSHOT6) %>%
      mutate(CHCKDNY2 = CHCKIDNY) %>%
      mutate(CHCCOPD3 = CHCCOPD1) %>%
      mutate(BIRTHSEX = SEX) %>% 
      mutate(ADDEPEV3 = ADDEPEV2) %>%
      mutate(DIABETE4 = DIABETE3) %>%
      mutate(CHCKDNY2 = CHCKIDNY)
  } else if (year == 2016){
    
    data_subset <- responses %>% 
      mutate(FLUSHOT7 = FLUSHOT6) %>%
      mutate(CHCCOPD3 = CHCCOPD1) %>%
      mutate(BIRTHSEX = SEX) %>%
      mutate(ADDEPEV3 = ADDEPEV2) %>%
      mutate(DIABETE4 = DIABETE3) %>%
      mutate(CHCKDNY2 = CHCKIDNY)
  }
  
  # subset the final columns of interest
  data_subset <- data_subset %>% 
    select(
    
      # unique identifier for year and state
      `_PSU`,

      # location and date of interview
      `_STATE`, IMONTH, IDAY, IYEAR,

      # vaccine variable
      FLUSHOT7,

      # demographic variables
      EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_PRACE1`,
      BIRTHSEX,

      # chronic health disease conditions
      CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
      CHCSCNCR, CHCOCNCR, CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,

      # sample weights
      `_LLCPWT`, `_LLCPWT2`,
      
      # Sample Design Stratification Variable
      `_STSTR`)
  
  # save dataset stratified by year as CSV and as RDS File
  write.csv(data_subset, file = paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_", year, ".csv"), row.names = FALSE)
  saveRDS(data_subset,   file = paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_", year, ".RDS"))
  
  # bind all years together
  if(i==1) {
    extracted_brfss_data <- data_subset
    } else {
      extracted_brfss_data <- plyr::rbind.fill(extracted_brfss_data, data_subset)
    }
  
  
}

# save the bound dataset in one combined file in the prepped data folder
write.csv(extracted_brfss_data, file = paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_all_years.csv"), row.names = FALSE)
saveRDS(extracted_brfss_data,   file = paste0(data_folder, "prepped_data/cdc_brfss/brfss_data_all_years.RDS"))

# # subset columns of interest according to the year of the data #####
# if (year==2021) {
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT7,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME3, `_IMPRACE`,
#       SEXVAR, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD3, ADDEPEV3, CHCKDNY2, DIABETE4,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     ) 
#   
#   # recode some of the variables
#   # subset the variables of interest
# } #####
# else if (year == 2020) {
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT7,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_IMPRACE`,
#       SEXVAR, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD2, ADDEPEV3, CHCKDNY2, DIABETE4,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     )
# } else if (year == 2019){
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT7,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_IMPRACE`,
#       SEXVAR, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD2, ADDEPEV3, CHCKDNY2, DIABETE4,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     )
# }  else if (year == 2018) {
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT6,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_IMPRACE`,
#       SEX1, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD1, ADDEPEV2, CHCKDNY1, DIABETE3,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     )
# } else if (year == 2017) {
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT6,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_IMPRACE`,
#       SEX, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD1, ADDEPEV2, CHCKIDNY, DIABETE3,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     )
# } else if (year == 2016){
#   data_subset <- responses %>% 
#     select(
#       # unique identifier for year and state
#       `_PSU`,
#       
#       # location and date of interview
#       `_STATE`, IMONTH, IDAY, IYEAR,
#       
#       # vaccine variable
#       FLUSHOT6,
#       
#       # demographic variables
#       EDUCA, RENTHOM1, VETERAN3, EMPLOY1, INCOME2, `_PRACE1`,
#       SEX, 
#       
#       # chronic health disease conditions
#       CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ASTHNOW,
#       CHCSCNCR, CHCOCNCR, CHCCOPD1, ADDEPEV2, CHCKIDNY, DIABETE3,
#       
#       # sample weights
#       `_LLCPWT`, `_LLCPWT2`
#     )
# }
# 