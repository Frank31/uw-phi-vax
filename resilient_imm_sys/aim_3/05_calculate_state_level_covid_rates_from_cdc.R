# Code to prep state-level covid vaccine rates
# Date: November 3 2022
# Francisco Rios Casas

# clear workspace
rm(list=ls())

# source set-up file
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_3/01_set_up_R.R")

raw_data_dir <- "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/raw_data/covid_county_vaccination_rates"

# load cdc data on vaccine rates in each county
az_headers <- read.csv(file = paste0(raw_data_dir, "/arizona/county_level_vaccination_data_for_arizona.csv"), skip = 2, nrows = 1)
arizona <- read.csv(file = paste0(raw_data_dir, "/arizona/county_level_vaccination_data_for_arizona.csv"), skip = 3, header = F)
colnames(arizona) <- names(az_headers)

wa_headers <- read.csv(file = paste0(raw_data_dir, "/washington/county_level_vaccination_data_for_washington.csv"), skip = 2, nrows = 1)
washington <- read.csv(file = paste0(raw_data_dir, "/washington/county_level_vaccination_data_for_washington.csv"), skip = 3, header = F)
colnames(washington) <- names(wa_headers)

nc_headers <- read.csv(file = paste0(raw_data_dir, "/north_carolina/county_level_vaccination_data_for_north_carolina.csv"), skip = 2, nrows = 1)
north_carolina <- read.csv(file = paste0(raw_data_dir, "/north_carolina/county_level_vaccination_data_for_north_carolina.csv"), skip = 3, header = F)
colnames(north_carolina) <- names(nc_headers)


# clean up potential NAs in column of interest
arizona$total_vax <- as.numeric(arizona$Percent.of.Total.Pop.Fully.Vaccinated...Resident)
washington$total_vax <- as.numeric(washington$Percent.of.Total.Pop.Fully.Vaccinated...Resident)
north_carolina$total_vax <- as.numeric(north_carolina$Percent.of.Total.Pop.Fully.Vaccinated...Resident)

# calculate the average in each state
az <- mean(arizona$total_vax, na.rm = TRUE)
wa <- mean(washington$total_vax, na.rm = TRUE)
nc <- mean(north_carolina$total_vax, na.rm = TRUE)

