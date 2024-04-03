# Author: Frances Gellert
# Purpose: Prep Ontario 2017-18 immunization data
# Date: Aug 03 2023

#clear everything
rm(list=ls())

# load required packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(nycflights13)
library(fivethirtyeight)

library(data.table)
library(readxl)
library(tidyverse)
library(utilities)
library(Hmisc) 
library(ggrepel)
library(scales)
library(RColorBrewer) 


# source set up script
# (this is where i would source my set_up_R script)

# load raw data
library(readxl)
ont18 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_level_vacc_rates/ontario/immunization-coverage-appendix-tables-2017-18.xlsx", sheet = "A1.PHUCov7")
View(ont18)

# assign the first row as column names
colnames(ont18) <- ont18[1, ]

# remove the first row (it is now the column names)
ont18 <- ont18[-1, ]

# remove the last two rows (they are not public health units) - HAVE TO RUN ONE AT A TIME
ont18 <- ont18[-37, ]
ont18 <- ont18[-37, ]

# drop empty rows (N/A)

# subset columns (in each dataset)
#ont18 <- ont18 %>% select(Mea, Mumps)

# rename columns
names(ont18)[1] <- "phu"
names(ont18)[2] <- "measles"
names(ont18)[3] <- "mumps"
names(ont18)[4] <- "rubella"
names(ont18)[5] <- "diphtheria"
names(ont18)[6] <- "tetanus"
names(ont18)[7] <- "polio"
names(ont18)[8] <- "pertussis"
names(ont18)[9] <- "haemophilus"
names(ont18)[10] <- "pneumococcal"
names(ont18)[11] <- "mcc"
names(ont18)[12] <- "varicella"

# JK INSTEAD TIDY THE DATA https://moderndive.netlify.app/4-tidy
# reshape the data
ont18_tidy <- pivot_longer(ont18, ic())
