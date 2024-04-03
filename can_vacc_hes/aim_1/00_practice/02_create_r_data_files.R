# Author: Frances Gellert (by Francisco Rios Casas)
# PURPOSE:  Run each of the R scripts which creates a permanent
# R Dataset
# Date: Aug 02 2023


# run loop which preps each script in turn
for(i in 1:14){
  
  # source set up script
  source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))
  
  # create file list indicating files to use
  province_file_list <- read_xlsx(paste0(team_drive, "Data/Documentation/list_of_data_used.xlsx")) %>% filter(data_type=="province_level_vaccination_rates")

  # set up file path where extracted R dataset is to be saved
  PUF <- paste0(raw_data_dir, "public_health_ontario/r_files")
  
  # set up location to ascii file that will be prepped
  flatfile <- paste0(raw_data_dir, "public_health_ontario/ascii_files/", province_file_list$file_name[i])
  
  # set up name of prep script that needs to be called
  prep_script <- paste0(code_dir, "aim_1/r_input_scripts/", province_file_list$r_input_script[i])
  
  ### RUN THE PREP FUNCTION HERE ###
  source(prep_script)
  
  # Print message
  print(paste0(i, " ", province_file_list$data_type[i], " ", province_file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  
  # clear local environment
  rm(list=ls())
  
}
