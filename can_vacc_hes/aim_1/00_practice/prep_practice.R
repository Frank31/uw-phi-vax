# Author: Francisco Rios Casas
# Purpose: Prep BCCDC free COVID-19 data updated July 06 2023
# Date: August 2 2023

  # uncomment below to trouble-shoot
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
#clear everything
rm(list=ls())

# Load/prep data
library(readxl)
Copy_of_BCCDC_COVID19_LHA_Data <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/practice/Copy of BCCDC_COVID19_LHA_Data.xlsx", 
                                             +     sheet = "LHA")
 
# rename dataset 
practice_dataset <- Copy_of_BCCDC_COVID19_LHA_Data

# remove Copy_of_BCCDC_COVID19_LHA_Data & view practice_dataset
rm(Copy_of_BCCDC_COVID19_LHA_Data)
View(practice_dataset)

# rename columns
names(practice_dataset)[1] <- "health_authority_id"
names(practice_dataset)[2] <- "health_authority"
names(practice_dataset)[3] <- "cov_all_ages_1"
names(practice_dataset)[4] <- "cov_age_0_4_1"
names(practice_dataset)[5] <- "cov_age_5_11_1"
  
  # remove rows without any population data 
  drop_rows <- practice_dataset %>% filter(is.na(population))
  drop_rows <- as.data.table(drop_rows)
  drop_rows[, proportion:=as.numeric(`Series Complete`)]
  na_proportion = drop_rows[, sum(proportion, na.rm = TRUE)]
  
  if (na_proportion!=0){
    stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  } else {
    county_dataset <- county_dataset %>% filter(!is.na(population))
  }
  
  # drop total rows
  county_dataset <- county_dataset %>% filter(county!="State Total")
  
  # reshape the data
  county_dataset <- pivot_longer(county_dataset, !c(county, number_of_programs, population), names_to="vaccine_name", values_to="proportion")
  
  year_of_data <- file_list$year[i]
  county_dataset$year <- year_of_data
  
  # if (na_proportion!=0){
  #   stop("Some rows with NA for location_name still have proportion data--review drop conditions before dropping NAs in key variables")
  # } else {
  #   county_dataset <- county_dataset %>% filter(!is.na(county))
  # }
  
  # filter out non-vaccine data
  county_dataset <- county_dataset %>% filter(vaccine_name%in%c("4 DTaP", "3 Polio", "1 MMR", 
                                                                "3 Hib", "3 Hep B", "1 Varicella", "Series Complete"))
  
  # only keep columns of interest
  county_dataset <- county_dataset %>% select(county, year, population, vaccine_name, proportion)
  return(county_dataset)
  
