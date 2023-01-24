# Author: Francisco Rios 
# Purpose: Prep all data necessary for Aim 3 Analysis
# Date: Last modified January 18 2023
# Updated to load data that was specifically for disease burden among children

rm(list=ls())

# set up files
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_3/01_set_up_R.R"))

# Read in list of files to prep
file_list <- data.table(read_excel(paste0(file_folder, "data/list_of_data_used.xlsx")))

# Filter file list
file_list <- file_list %>% filter(data_type=="counterfactual_outcomes" & data_source=="gbd_dalys_children")

# extract DALYs and format for analysis file

for (i in 1:nrow(file_list)){
  
  # set up file path
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/', file_list$disease[i], '/', file_list$file_name[i])
  
  # read csv file
  tmpData <- read.csv(file = file_dir)
  
  # subset columns of interest
  tmpData <- tmpData %>% select(location_id, location_name, cause_name, metric_name, year, val)
  
  # rename values in columns
  tmpData$cause_name <- tolower(tmpData$cause_name)
  tmpData$metric_name <- tolower(tmpData$metric_name)
  
  # reshape the data 
  tmpData <- pivot_wider(tmpData,
                         id_cols=c("location_id", "location_name", "year", "cause_name"),
                         names_from = c(metric_name),
                         values_from = val)
  
  # # rename Daly columns
  # tmpData <- tmpData %>% rename()
  # if (file_list$disease[i]=="Diphtheria"){
  #   tmpData <- tmpData %>% rename("dalys_diphtheria_number"="diphtheria_number",
  #                                 "dalys_diphtheria_measles_percent"="diphtheria_percent",
  #                                 "dalys_diphtheria_measles_rate"="diphtheria_rate")
  #   } else if (file_list$disease[i]=="Measles"){
  #   tmpData <- tmpData %>% rename("dalys_measles_number"="measles_number",
  #                                 "dalys_measles_percent"="measles_percent",
  #                                 "dalys_measles_rate"="measles_rate")
  # }
  
  # Bind data together 
  if(i==1){
    extracted_daly_data <- tmpData
  } else {
    extracted_daly_data <- plyr::rbind.fill(extracted_daly_data, tmpData)
  }
}

# reshape the data to be wide shape
test <- pivot_wider(extracted_daly_data,
                       id_cols=c("location_id", "location_name", "year"),
                       names_from = c(cause_name),
                       values_from = c(number, percent, rate),
                       names_glue = "dalys_{cause_name}_{.value}")

# save in one combined file in prepped data folder
saveRDS(test, file=paste0(prepped_data_dir, "aim_3/01_prepped_dalys_under4.RDS"))
