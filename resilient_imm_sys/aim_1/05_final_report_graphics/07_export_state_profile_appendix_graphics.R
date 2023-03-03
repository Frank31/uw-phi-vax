# Name: 01_export_state_profile_appendix_graphs.R
# Date: January 24 2023
# Author: Francisco Rios

# Set Up
library(tidyverse)
library(RColorBrewer)

# source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/03_data_profiles/01_set_up_R.r")
prepped_data_dir <- "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/"

# set the place to save images
res_dir <- "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/final_report/"

###### North Carolina #####

# Save some key variables
s <- "North Carolina" # state
# c <- c("Lenoir County", "Edgecombe County", "Hertford County") # counties
# r <- "north carolina" # region
# sr <- c("lenoir", "edgecombe", "hertford") # subregion

# load state level data
state_data <- readRDS(paste0(prepped_data_dir, "11_merged_data_for_state_profile_docs.RDS")) %>%
  # filter state
  filter(state==s)

# reshape data for plotting trends in vaccination gaps
state_data_gaps <- state_data %>% 
  select(state, YEAR, income, VACCINE, hispa_diff, black_diff, other_diff) %>%
  pivot_longer(cols = c(hispa_diff, black_diff, other_diff),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable_label = case_when(
    variable=="hispa_diff" ~ "Hispanic",
    variable=="black_diff" ~ "Black/African-American",
    variable=="other_diff" ~ "Other or multi-racial"
  )) %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3 ,
    income=="miss" ~ 4,
  ))

# factor income_label
state_data_gaps$income_label <- factor(state_data_gaps$income_label,
                                       labels = c("High income", 
                                                  "Medium income", 
                                                  "Low income", 
                                                  "Missing income "))


# plot trends in vaccination coverage among different racial/ethnic groups
plot1a <- ggplot(state_data_gaps, aes(YEAR, value, group=factor(VACCINE))) + 
  geom_line(aes(color=VACCINE), show.legend = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
  labs(title=paste('Difference in Vaccination Rates'), 
       subtitle=paste0(' Reference group is white children of same income background in', " ", s, 
                       "\n Positive slope indicates increased gap between the two time points"),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
  ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
  xlab('Year') +
  ylim(-.06, .06) +
  theme_minimal()

# export the file as a png
png(paste0(res_dir, "03_north_carolina_gap.png"),
    width = 3000,
    height = 1800,
    units = "px",
    res = 300)
plot1a
dev.off()

# second plot showing percent vaccination coverage
state_cov <- state_data %>% select(state, YEAR, income, VACCINE, white, hispa, black, other) %>% 
  pivot_longer(cols = c(white, hispa, black, other),                                                                                          
               names_to = "race",                                                                                           
               values_to = "value") %>%
  mutate(race_label = case_when(
    race=="hispa" ~ "Hispanic",
    race=="black" ~ "Black",
    race=="other" ~ "Other/multiple",
    race=="white" ~ "White"
  ))

state_cov$value <- as.numeric(state_cov$value)

# factor the income level
state_cov <- state_cov %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3,
    income=="miss" ~ 4
  ))

# factor income_label
state_cov$income_label <- factor(state_cov$income_label,
                                 labels = c("High", 
                                            "Medium", 
                                            "Low", 
                                            "Missing"))

# re-scale the percentage to be whole numbers
state_cov$value <- round(100*state_cov$value, 0)

# create raster plot of locations
plot1b <- ggplot(state_cov, aes(income_label, VACCINE)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  # geom_tile(data=median, size=1, fill=NA, colour="black") +
  labs(title=paste('Percent of children fully vaccinated in', s), y = 'Vaccine', x = 'Income level', 
       # subtitle=paste0('in', " ", s),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)", fill='Darker value indicates \n higher vaccination rate') +
  facet_grid(vars(race_label), vars(YEAR)) +
  theme_minimal()

# export the plot as a png
png(paste0(res_dir, "04_north_carolina_coverage.png"),
    width = 2200,
    height = 2600,
    units = "px",
    res = 300)
plot1b
dev.off()

##### Washington #####

# define new variables for Washington state
s <- "Washington" # state

# load state level data
state_data <- readRDS(paste0(prepped_data_dir, "11_merged_data_for_state_profile_docs.RDS")) %>%
  # filter state
  filter(state==s)

# reshape data for plotting trends in vaccination gaps
state_data_gaps <- state_data %>% 
  select(state, YEAR, income, VACCINE, hispa_diff, black_diff, other_diff) %>%
  pivot_longer(cols = c(hispa_diff, black_diff, other_diff),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable_label = case_when(
    variable=="hispa_diff" ~ "Hispanic",
    variable=="black_diff" ~ "Black/African-American",
    variable=="other_diff" ~ "Other or multi-racial"
  )) %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3 ,
    income=="miss" ~ 4,
  ))

# factor income_label
state_data_gaps$income_label <- factor(state_data_gaps$income_label,
                                       labels = c("High income", 
                                                  "Medium income", 
                                                  "Low income", 
                                                  "Missing income "))

plot2a <- ggplot(state_data_gaps, aes(YEAR, value, group=factor(VACCINE))) + 
  geom_line(aes(color=VACCINE), show.legend = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
  labs(title=paste('Difference in Vaccination Rates'), 
       subtitle=paste0(' Reference group is white children of same income background in', " ", s, 
                       "\n Positive slope indicates increased gap between the two time points"),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
  ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
  xlab('Year') +
  ylim(-.06, .06) +
  theme_minimal()

# export the file as a png
png(paste0(res_dir, "05_washington_gap.png"),
    width = 3000,
    height = 1800,
    units = "px",
    res = 300)
plot2a
dev.off()

# make the second washington graphic
# second plot showing percent vaccination coverage
state_cov <- state_data %>% select(state, YEAR, income, VACCINE, white, hispa, black, other) %>% 
  pivot_longer(cols = c(white, hispa, black, other),                                                                                          
               names_to = "race",                                                                                           
               values_to = "value") %>%
  mutate(race_label = case_when(
    race=="hispa" ~ "Hispanic",
    race=="black" ~ "Black",
    race=="other" ~ "Other/multiple",
    race=="white" ~ "White"
  ))

state_cov$value <- as.numeric(state_cov$value)

# factor the income level
state_cov <- state_cov %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3,
    income=="miss" ~ 4
  ))

# factor income_label
state_cov$income_label <- factor(state_cov$income_label,
                                 labels = c("High", 
                                            "Medium", 
                                            "Low", 
                                            "Missing"))

# re-scale the percentage to be whole numbers
state_cov$value <- round(100*state_cov$value, 0)

# create raster plot of locations
plot2b <- ggplot(state_cov, aes(income_label, VACCINE)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  # geom_tile(data=median, size=1, fill=NA, colour="black") +
  labs(title=paste('Percent of children fully vaccinated in', s), y = 'Vaccine', x = 'Income level', 
       # subtitle=paste0('in', " ", s),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)", fill='Darker value indicates \n higher vaccination rate') +
  facet_grid(vars(race_label), vars(YEAR)) +
  theme_minimal()

# export the plot as a png
png(paste0(res_dir, "06_washington_coverage.png"),
    width = 2200,
    height = 2600,
    units = "px",
    res = 300)
plot2b
dev.off()


##### Arizona plots #####

# Save some key variables
s <- "Arizona" # state

# load state level data
state_data <- readRDS(paste0(prepped_data_dir, "11_merged_data_for_state_profile_docs.RDS")) %>%
  # filter state
  filter(state==s)

# reshape data for plotting trends in vaccination gaps
state_data_gaps <- state_data %>% 
  select(state, YEAR, income, VACCINE, hispa_diff, black_diff, other_diff) %>%
  pivot_longer(cols = c(hispa_diff, black_diff, other_diff),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable_label = case_when(
    variable=="hispa_diff" ~ "Hispanic",
    variable=="black_diff" ~ "Black/African-American",
    variable=="other_diff" ~ "Other or multi-racial"
  )) %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3 ,
    income=="miss" ~ 4,
  ))

# factor income_label
state_data_gaps$income_label <- factor(state_data_gaps$income_label,
                                       labels = c("High income", 
                                                  "Medium income", 
                                                  "Low income", 
                                                  "Missing income "))


# plot trends in vaccination coverage among different racial/ethnic groups
plot3a <- ggplot(state_data_gaps, aes(YEAR, value, group=factor(VACCINE))) + 
  geom_line(aes(color=VACCINE), show.legend = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
  labs(title=paste('Difference in Vaccination Rates'), 
       subtitle=paste0(' Reference group is white children of same income background in', " ", s, 
                       "\n Positive slope indicates increased gap between the two time points"),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
  ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
  xlab('Year') +
  ylim(-.06, .06) +
  theme_minimal()

# export the file as a png
png(paste0(res_dir, "07_arizona_gap.png"),
    width = 3000,
    height = 1800,
    units = "px",
    res = 300)
plot3a
dev.off()

# second plot showing percent vaccination coverage
state_cov <- state_data %>% select(state, YEAR, income, VACCINE, white, hispa, black, other) %>% 
  pivot_longer(cols = c(white, hispa, black, other),                                                                                          
               names_to = "race",                                                                                           
               values_to = "value") %>%
  mutate(race_label = case_when(
    race=="hispa" ~ "Hispanic",
    race=="black" ~ "Black",
    race=="other" ~ "Other/multiple",
    race=="white" ~ "White"
  ))

state_cov$value <- as.numeric(state_cov$value)

# factor the income level
state_cov <- state_cov %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3,
    income=="miss" ~ 4
  ))

# factor income_label
state_cov$income_label <- factor(state_cov$income_label,
                                 labels = c("High", 
                                            "Medium", 
                                            "Low", 
                                            "Missing"))

# re-scale the percentage to be whole numbers
state_cov$value <- round(100*state_cov$value, 0)

# create raster plot of locations
plot3b <- ggplot(state_cov, aes(income_label, VACCINE)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  # geom_tile(data=median, size=1, fill=NA, colour="black") +
  labs(title=paste('Percent of children fully vaccinated in', s), y = 'Vaccine', x = 'Income level', 
       # subtitle=paste0('in', " ", s),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)", fill='Darker value indicates \n higher vaccination rate') +
  facet_grid(vars(race_label), vars(YEAR)) +
  theme_minimal()

# export the plot as a png
png(paste0(res_dir, "08_arizona_coverage.png"),
    width = 2200,
    height = 2600,
    units = "px",
    res = 300)
plot3b
dev.off()








