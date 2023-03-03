# 01_set_up_file.R
# Francisco Rios Casas
# February 16 2023

# Purpose: This file will load any necessary libraries and filepaths to carry out
# the analysis

# Load packages
library(tidyverse)
library(readxl)
library(survey)
library(haven)

# set seed
set.seed(500)

# Set key file paths
data_folder <- "C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/data/"
# data_docs <- "C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/data/documentation"

# load the file list
file_list <- read_xlsx(paste0(data_folder, "documentation/list_of_data_used.xlsx"))
