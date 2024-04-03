## Prep 2011, 2016, 2021 ON-Marg Data
# Author: Frances Gellert
# Purpose: This script will create complete ON-Marg dataset for 2011, 2016, and 2021
# Date: Dec 29, 2023

# clear all
rm(list=ls())

# source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))

## Load data ----
library(readxl)
onmarg11 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/social_indices/on-marg/index-on-marg-2011.xlsx", sheet = "PHUUID_2011")
onmarg16 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/social_indices/on-marg/index-on-marg-2016.xlsx", sheet = "2016_PHUUID")
onmarg21 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/social_indices/on-marg/index-on-marg-2021.xlsx", sheet = "2021_HUID")
#   Effective May 1, 2018, Oxford County Public Health and Elgin-St. Thomas Health Unit have merged to become Southwestern Public Health (as seen in onmarg21).
#   Perth District Health Unit dropped, reflected starting 2019-20 school year (as seen in onmarg21)

