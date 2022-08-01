# Author: Francisco Rios 
# Purpose: set up file for aim 3 analyses
# Date: Last modified July 14, 2022

# Load required packages -----
library(data.table)
library(MASS)
library(ggplot2)
library(readxl)
library(tidyverse)

# Define important variables -----
set.seed(500)

# set file folder and code folder for my computer pathways
file_folder <- '/Users/ziva/Library/CloudStorage/OneDrive-UW/General/'

code_dir <- '/Users/ziva/R Projects/uw-phi-vax/global_vac_index'

setwd(code_dir) # set the working directory to wherever code is stored
raw_data_dir <- paste0(file_folder,"Data/raw_data/") # location of raw data
prepped_data_dir <- paste0(file_folder,"Data/prepped_data/") # location of prepped data
codebook_directory <- paste0(file_folder,"Data/documentation/codebooks/aim_2/") # location of codebooks for interpreting data
resDir <- paste0(file_folder, "Results/") # location of  any result outputs
visDir <- paste0(file_folder,"Visualizations/") # location where visualizations are saved
