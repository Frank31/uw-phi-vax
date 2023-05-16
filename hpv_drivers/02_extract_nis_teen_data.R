# 02_extract_nis_teen_data.R
# Francisco Rios Casas
# March 13 2023

# Purpose: Loop that extracts all variables necessary for analysis

# Note: Set the working directory at the root of this repository
source("./01_set_up_file.R")

# filter the list of prep scripts
file_list <- file_list %>% filter(data_source=="cdc_nis_teen")

# create loop that extracts variables of interest
for (i in 1:length(file_list)){
  # uncomment to troubleshoot
  # i <- 2
  
  # set unique parameters
  year <- file_list$year[i]
  prep_file <- file_list$file_name[i]
  
  # Print statement so you know which file it broke on
  print(paste0(i, " Now Prepping: ", year," CDC NIS Teen Data using ", prep_file))
  
  # source each of the prep files - this will extract and save all the variables
  source(paste0("./nis_prep_scripts/", prep_file, ".R"))
  
}
