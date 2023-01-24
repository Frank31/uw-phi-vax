# 06_state_resuls_for_report.R

# Francisco Rios Casas
# Create graphics to show specific states in graphics
# October 11, 2022

# set up 
rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_state_level_analyses/01_set_up_R.R"))

# Load data
data <-  read.csv(file = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/12_prepped_data_for_final_report.csv")

# subset to only states of interest
dt_subset <- data %>% filter(ESTIAP %in% c("NC", "AZ", "WA"))

# calculate the percent coverage for each state
by_state <- dt_subset %>% group_by(ESTIAP, VACCINE)

by_state <- by_state %>% 
  summarize_at(c("X2019"), mean)

# re-shape to be wide format
table <- by_state %>% pivot_wider(names_from = "VACCINE", values_from = "X2019")

# save the table on local directory
write.csv(table, file="C:/Users/frc2/Desktop/table_x.csv")

# describe the average change for each vaccine
# by_race_inc <- plot_data %>% group_by(RACEETHK_R, INCPOV1, VACCINE)
# 
# by_rac_inc <- by_race_inc  %>%
#   summarize_at(c("change"), mean)