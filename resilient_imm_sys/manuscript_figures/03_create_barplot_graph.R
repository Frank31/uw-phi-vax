# 03_create_barplot_graph.R
# February 7 2023
# Purpose: Create a barplot showing how well states performed compared to each other

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

# create bar plot showing how well states of interest performed

# Load data
data <-  read.csv(file = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/12_prepped_data_for_final_report.csv")

# Create groupings based on how states compared to each other but also on which were most likely to show equitable improvement
barplot_dt <- data %>%
  filter(category!="Outlier") %>%
  filter(category!="Reference") %>%
  group_by(ESTIAP) %>%
  mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low"), freq.out = sum(category=="outlier")) %>%
  ungroup %>%
  group_by(ESTIAP, category) %>%
  tally %>%
  mutate(pct=n/sum(n))

barplot_dt$category <- factor(barplot_dt$category, levels = c("Worse", "Average", "Better"), labels = c("Worse", "Average", "Better"))

barplot_dt <- barplot_dt %>% filter(ESTIAP %in% c("NM", "NC", "MA", "WA", "VA", "IL-REST OF STATE", "AZ", "OR", "TX-DALLAS COUNTY"))


# re-code the ESTIAP Variables that will be plotted to include the full name
barplot_dt <- barplot_dt %>% mutate(ESTIAP = case_when(
  ESTIAP=="AZ" ~ "Arizona",
  ESTIAP=="OR" ~ "Oregon",
  ESTIAP=="TX-DALLAS COUNTY" ~ "Dallas County, Texas",
  ESTIAP=="WA" ~ "Washington",
  ESTIAP=="VA" ~ "Virginia",
  ESTIAP=="IL-REST OF STATE" ~ "Illinois (excluding Chicago)",
  ESTIAP=="NM" ~ "New Mexico",
  ESTIAP=="NC" ~ "North Carolina",
  ESTIAP=="MA" ~ "Massachusetts"
))

barplot_dt$ESTIAP <- factor(barplot_dt$ESTIAP, levels =c("Arizona", "Oregon", "Dallas County, Texas", "Washington", "Virginia", "Illinois (excluding Chicago)", "New Mexico", "North Carolina", "Massachusetts"))

plot2 <- ggplot(barplot_dt, aes(fill=category, x=ESTIAP, y=n, label=round(pct,2))) +
  geom_bar(position="fill", stat="identity") +
  geom_text(size =3, position = position_fill(vjust = .5)) +
  coord_flip() +
  scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
  theme_minimal(base_size = 20) + 
  labs(title=paste0('Comparing achievements in reducing the racial/ethnic vaccination gap'), 
       x="State/area", 
       y="Proportion of African-American, Hispanic, or Asian-American children that improved relative to White children")

image_loc <- "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/manuscript/"

png(filename = paste0(image_loc, "03_relative_perf.png"), width = 15, height =5, units = "in", pointsize = 10, res = 900, )
print(plot2)
dev.off()
