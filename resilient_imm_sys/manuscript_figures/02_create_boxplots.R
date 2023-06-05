# 02_create_boxplot_graph.R
# May 25 2023
# Purpose: we want a few graphics of vaccination trends over the years

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

# important file directories
team_drive  <- 'C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/'

# October 11, 2022

# set up 
rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_state_level_analyses/01_set_up_R.R"))

# Load data
data <-  read.csv(file = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/12_prepped_data_for_final_report.csv")

# Create boxplot showing what the distribution of the difference between white and non-white children
eii_data <- data %>% filter(RACEETHK_R != "White" & category!="Outlier")

# refactor the INCOME categories
eii_data$INCPOV1 <- factor(eii_data$INCPOV1, levels = c("High", "Medium", "Low", "Unknown"))

# create box plot showing the difference between 
h <- ggplot(eii_data, aes(x = INCPOV1, y = eii)) +
  geom_boxplot() + 
  theme_minimal(base_size = 20) +
  facet_grid(cols = vars(RACEETHK_R), vars(VACCINE) )+
  labs(title = paste('Difference between change in white children and children of other racial/ethnic backgrounds'), 
       y = 'Difference', 
       x = 'Income Group', 
       subtitle = paste0('Zero indicates no difference between the two groups. \n Positive value indicates white children saw a worse decrease.'))
  

png(filename = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/manuscript/02_differences_box_plot.png",
    width = 16, 
    height = 8.5, 
    units = "in",
    res = 600)
print(h)
dev.off()
