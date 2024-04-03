## Analyze 2021 BC Census subdivision (CSD) Census Data
# Author: Frances Gellert
# Date: Oct 3 2023

#clear all
rm(list=ls())

#source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))
 
#load data
census_bc_CSD <- read_csv(file = paste0("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/2021_census/census_bc_CSD.csv"))

#only keep columns of interest
census_bc_CSD <- select(census_bc_CSD, `GEO_NAME`, `CHARACTERISTIC_NAME`, `CHARACTERISTIC_NOTE`, `C1_COUNT_TOTAL`, `C10_RATE_TOTAL`)
census_bc_CSD

#group by CHARACTERISTIC_NOTE column
library(zoo)
census_bc_CSD <- census_bc_CSD %>%
  mutate(CHARACTERISTIC_NOTE = na.locf(CHARACTERISTIC_NOTE)) #na.locf function from the zoo package to replace "NA" values with the previous non-NA value in the same column
print(census_bc_CSD)

view(census_bc_CSD)

#RACE ----
#CLEAN
#only keep columns containing "118 and 119" (race variables)
race_bc_CSD <- census_bc_CSD %>%
  filter(CHARACTERISTIC_NOTE %in% c(118, 119))

#remove CHARACTERISTIC_NOTE column
race_bc_CSD <- race_bc_CSD %>%
  select(-CHARACTERISTIC_NOTE) 
race_bc_CSD

#rename columns
new_column_names <- c("CSD", "race", "count", "rate")
colnames(race_bc_CSD) <- new_column_names #set column names to dataframe

#MANIPULATE - WIDE
#pivot_wider
wide_race <- race_bc_CSD %>% 
  select(-count) %>%
  pivot_wider(names_from = "race", values_from = "rate") 
wide_race

#identify 'NA' values
rows_with_na <- wide_race %>%
  filter(map_lgl(`Total visible minority population`, ~ any(is.na(.x))))
rows_with_na

#create new dataset without 'NA' values
race2 <- wide_race %>%  
  anti_join(rows_with_na)
view(race2)

#convert 'list' variables to 'numerical' values 
race2 <- race2 %>%
  filter(CSD != "CSD") %>%
  unnest(cols = -CSD)  #use the unnest() function from the tidyr package to flatten the list columns into separate rows
race2

#rename row 107 with symbol - "Q'alatk*7em, Indian reserve (IRI)" to "Q'alatku7em, Indian reserve (IRI)"
race2 <- race2 %>%
  mutate(CSD = ifelse(row_number() == 107, "Q'alatku7em, Indian reserve (IRI)", CSD))
view(race2) 

#MANIPULATE - LONG
#pivot_longer
long_race <- race2 %>%
  pivot_longer(cols = -CSD, names_to = "race", values_to = "rate")
long_race

#VISUALIZE
#only total minority population
total_minority <- long_race %>%
  filter(race == "Total visible minority population")
total_minority 

ggplot(total_minority, aes(x = rate)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Minority Rate by CSD in B.C.", x = "Total Minority Rate", y = "Number of CSDs") +
  theme_minimal()

#NOZERO RACE ----
#create a data frame containing only the CSDs where at least one racial group column has a value that is not equal to 0.
race_nozero <- race2 %>%
  filter_at(vars(-CSD), any_vars(. != 0))
view(race_nozero)

#VISUALIZE for race_nozero
#pivot_longer
long_race_nozero <- race_nozero %>% #lengthen race variable
  pivot_longer(cols = -CSD, names_to = "race", values_to = "rate")
long_race_nozero

#histogram of number of CSDs by total minority population rate
total_minority_nozero <- long_race_nozero %>%
  filter(race == "Total visible minority population")

ggplot(total_minority_nozero, aes(x = rate)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Minority Rate by CSD in B.C.", x = "Total Minority Rate", y = "Number of CSDs") +
  theme_minimal()


#bar chart with lines dividing the percentages
#####THIS IS WHAT I WANT!!!!
ggplot(long_race_nozero, aes(x = CSD, y = rate, fill = race)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Stacked Bar Chart of Total Minority Rate by Census Subdivision",
    x = "Census Subdivision",
    y = "Total Minority Rate"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()


#other visualization options
#remove total visible minority population
race_nozero2 <- race_nozero %>% select(-2)
race_nozero2

long_race_nozero2 <- race_nozero2 %>%
  pivot_longer(cols = -CSD, names_to = "race", values_to = "rate")
long_race_nozero2
summary(long_race_nozero2)

ggplot(long_race_nozero2, aes(x = rate, fill = race)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  facet_wrap(~race, scales = "free_y", nrow = 4) +  # Adjust nrow as needed
  labs(
    title = "Distribution of Racial Group Percentages by Census Subdivision",
    x = "Percentage",
    y = "Frequency"
  ) +
  theme_minimal() +
  xlim(0, 55)
