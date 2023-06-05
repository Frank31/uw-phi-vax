# 01_create_trend_graph.R
# February 7 2023
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

# load the estimates of vaccine coverage
# data <- readRDS("C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/02_estimated_vaccine_coverage.RDS")

# First: Unadjusted results: Create trend line of coverage over each year 
# and for full vaccination--that variable might not be extracted #####

# calculate the average coverage across all states and for each racial/ethnic group

# calculate new variables of difference in race and ethnic disparities for each year

# First Part: unadjusted estimates

# load the unadjusted estimates
data <- readRDS(paste0(team_drive, "Data/prepped_data/01_complete_nis_data.RDS"))

# create indicator of fully vaccinated with all three doses
data <- data %>% mutate(full_vac = case_when(
  dtp_vac==1 & mmr_vac==1 & hep_vac==1 ~ 1,
  TRUE ~ 0))

# summarize fully vaccinated according to year
# by_race_inc <- plot_data %>% group_by(RACEETHK_R, INCPOV1, VACCINE)

# test <- data %>% select(YEAR, full_vac)
# 
# by_year <- test %>% group_by(YEAR)
# 
# by_year <- by_year %>%
#   summarize_at(c("full_vac"), mean)

# all vaccines
test <- data %>% select(YEAR, full_vac, dtp_vac, mmr_vac, hep_vac)

# summarize values by year
by_year <- test %>% group_by(YEAR)

# calculate average coverage by vaccine type
by_year <- by_year %>%
  summarize_at(c("full_vac", "dtp_vac", "mmr_vac", "hep_vac"), mean)

# by_year <- by_year %>% 
#   summarize_at(c("prop_full_vac"), mean)

# by_rac_inc <- by_race_inc  %>%
#   summarize_at(c("change"), mean)

# reshape data for plotting
plot_data <- pivot_longer(by_year, !c("YEAR"), names_to = c("vaccine_type"))

# Factor the vaccine variables to make plotting 
plot_data$vaccine_type <- factor(plot_data$vaccine_type, 
                                 levels = c("dtp_vac", "hep_vac", "mmr_vac", "full_vac"),
                                 labels = c("DTP", 
                                            "Hepatitis B",
                                            "MMR",
                                            "Fully Vaccinated"))

# drop the year 2020 from the dataset
plot_data <- plot_data %>% filter(YEAR<=2019)

# Plot unadjusted coverage rates
g <- ggplot(plot_data, aes(y=value*100, x=YEAR)) +
  geom_line(aes(color=vaccine_type), show.legend = TRUE) +
  labs(title = "Vaccination Coverage Rates in the United States",
       subtitle = "Unadjusted estimates, 2007 to 2019",
       caption = "Data Source: National Immunization Survey, CDC",
       y = "Percent",
       x = "Year",
       legend="Vaccine Type") +
  guides(color=guide_legend(title="Vaccine Type"))+
  ylim(75, 100) +
  theme_minimal() 

png(filename = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/manuscript/01_trends_us.png",
    width = 11, 
    height = 6, 
    units = "in",
    res = 600)
print(g)
dev.off()

##### Should I apply weights???? #####
# Don't think that would change the results much!!
#   
# 
# ggplot(data %>% filter(ESTIAP==lctns[[g]]), aes(y = PredictedProb, x = YEAR, color = VACCINE)) + 
#   geom_ribbon(aes(ymin = LL,
#                   ymax = UL, fill = VACCINE), alpha = 0.2) +
#   geom_line(size = 1, alpha = .8) + 
#   facet_wrap(~INCOME) +
#   labs(title = paste('Vaccine coverage for', l), y = 'Predicted probability of being vaccinated', x = 'Year',
#        subtitle = "Stratified according to race/ethnicity and family income") +
#   theme_minimal() +
#   facet_grid(vars(INCPOV1), vars(RACEETHK_R))
# 
# # plot the estimates coverage rates