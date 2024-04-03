## Analyze CMA Census Data
# Author: Frances Gellert
# Date: Sept 13 2023

# clear all
rm(list=ls())

# source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))
 
# load data
library(readxl)
census_CMA <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/2021_census/census_CMA.xlsx")

#open/view data
View(census_CMA)  


#select relevant columns
mydata <- subset(census_CMA, select = c(""))

#flip rows and columns
transposed_census_CMA <- t(census_CMA)
view(transposed_census_CMA)

#make row 1 the variable names
variable_names <- as.character(transposed_census_CMA[2, ])



ggplot(count_data, aes(x = "Vacnouver CMA total", y = Unique_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Unique Values Count in Each Column", x = "Column", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))