# 01_set_up_file.R
# Francisco Rios Casas
# March 13 2023

# Purpose: This file will load any necessary libraries and filepaths to carry out
# the analysis

# Load packages
library(tidyverse)
library(readxl)
library(survey)
library(haven)
library(Hmisc) 
library(data.table)
library(aod)

# set seed
set.seed(500)

# Set key file paths
data_folder <- "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Data/quantitative/"

# load the file list
file_list <- read_xlsx(paste0(data_folder, "_documentation/list_of_data_used.xlsx"))

# Print Complete Statement
print("Sourced necessary packages for NIS - Teen Data")
