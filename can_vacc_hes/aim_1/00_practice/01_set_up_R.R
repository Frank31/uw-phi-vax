# Author: Frances Gellert (by Francisco Rios Casas)
# Purpose: Set up R for prepping UW PHI Vaccination Data - Merck 5 Aim 1
# Date: Aug 02 2023

# Load required packages -----
library(data.table)
library(ggplot2)
library(readxl)
library(tidyverse)
library(utilities)
library(Hmisc) 
library(ggrepel)
library(scales)
library(RColorBrewer) 

# Define important variables -----
set.seed(500)

# set shared team Google drive and code repo dynamically
if (Sys.info()[2]=='10 x64'){
  team_drive  <- 'C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/'
  code_dir <- 'C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/'
} else if (Sys.info()[2]=='Server x64'){
  stop("The CSDE server has not been set up for analyses.")
  # team_drive  <- 'G:/Shared with Me/Merck Vaccine Improvement Index Project/'
  # code_dir <- 'H:/uw-phi-vax/global_vac_index/'
} else {
  stop("no other computer systems have been set up for analyses")
  # team_drive  <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1P7ITMVB9x01fuYfHW8-uWogw4SpbuvwO/Merck Vaccine Improvement Index Project/'
  # code_dir <- '~/Documents/uw-phi-vax/'
}

setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(team_drive,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(team_drive,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(team_drive,"Data/documentation/codebooks/aim_2/") # location of codebooks for interpreting data
resDir <- paste0(team_drive, "Results/") # location of  any result outputs
visDir <- paste0(resDir, "graphics/") # location where visualizations are saved

