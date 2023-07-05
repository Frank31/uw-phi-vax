# 03_visualize_healthcare_worker_covid_data_patterns.R
# Francisco Rios Casas
# March 17, 2023

# Purpose: visualize the patterns of data availability and trends in counties 
# within states in the shortlist of states
# among healthcare workers in nursing facilities

# set the working directory at the root of this repository

# things we are interested in understanding at this point:
# 1. average state vaccination rates, and range among counties
# 2. create dataset showing how counties fall on the spectrum, 
# as well as number of facilities per county, and the county population size
# think about the best way to visualize that information
# the last one might require merging with census data

# Source the set-up file
source("./01_set_up_file.R")

# load the merged dataset
data <- readRDS(paste0(data_folder, "prepped_data/cms_healthcare_worker/_merged_shortlist_nursing_home_data_2023-02-26.RDS"))

# save numeric variables as numeric
data$percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time <- as.numeric(data$percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time)

##############################
# Part 1: calculate descriptive statistics at the state level
##############################

state_averages <- data %>%
  group_by(provider_state) %>%
  filter(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time!=0) %>%
  summarize(mean_hcw_coverage_state = mean(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, na.rm = TRUE),
            min_hcw_coverage_state  =  min(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, na.rm = TRUE),
            max_hcw_coverage_state  =  max(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, na.rm = TRUE),
            first_hcw_cov_quant_state   =  quantile(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, 0.25),
            third_hcw_cov_quant_state   =  quantile(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, 0.75))

# manually double-check the percentile cut-offs for the dataset ARE BEING CORRECTLY CALCULATED
# TEST STATE = MASSACHUSETTS
test <- data %>% filter(provider_state=="MA")
quantile(test$percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, 0.25, na.rm = TRUE)
quantile(test$percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, 0.75, na.rm = TRUE)

# merge state category labels
list_of_states <- read_xlsx(path = paste0(data_folder, "documentation/cms_healthcare_worker/final_states_of_interest.xlsx"))
state_averages <- state_averages %>% right_join(list_of_states, by = c("provider_state"="state"))

# clarify columns names (what the categories really mean)
state_averages <- state_averages %>% rename(flu_covid_category=category)

# export the table as a csv
write.csv(state_averages, file = "C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/05_state_vaccine_information.csv")

##############################
# Part 2: calculate descriptive statistics for counties. 
# Table should include variables such as: State, County, number of facilities, 
# Average coverage in county, How county compares to others within the state, 
# county population per US Census Bureau
#
# probably add county-level covid performance in general population
#
##############################

# number of facilities per county
dt <- data %>% 
  group_by(provider_state, county) %>%
  count(county)
  
# average coverage among healthcare workers per county
dt2 <- data %>%
  group_by(county, provider_state) %>%
  summarize(county_mean_hcw_coverage = mean(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time, na.rm = TRUE))

# merge state averages to county estimates 
dt3 <- dt2 %>% 
  group_by(county, provider_state) %>%
  inner_join(state_averages, by = "provider_state") %>%
  # calculate how counties are performing relative to each other
  mutate(county_performance = case_when(
    county_mean_hcw_coverage >= first_hcw_cov_quant_state & county_mean_hcw_coverage <= third_hcw_cov_quant_state ~ "average",
    county_mean_hcw_coverage > third_hcw_cov_quant_state ~ "better",
    county_mean_hcw_coverage < first_hcw_cov_quant_state ~ "worse"
  ))

# create correctly formatted county name variable
dt3$county_name_corrected <- paste0(dt3$county, " County")

# St Louis City MO does not match the county designation in the census data
dt3 <- dt3 %>% mutate(county_name_corrected=recode(county_name_corrected, 'St. Louis city'='St. Louis City County'))

# load census bureau data
census_data <- read.csv(file = paste0(data_folder, "raw_data/census_bureau/cc-est2021-agesex-all.csv")) %>%
  # filter to states of interest
  filter(STNAME %in% c("Colorado", "Georgia", "Massachusetts", "Missouri", "Ohio", "Pennsylvania", "Vermont")) %>%
  # filter year code to be 3 7/1/2021 population estimate
  filter(YEAR==3) %>%
  # select columns of interest
  select(STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE, AGE18PLUS_TOT)

# St. Louis city MO does not match the county designation in the healthcare worker data
census_data <- census_data %>% mutate(CTYNAME=recode(CTYNAME, "St. Louis city"="St. Louis City County"))

# make sure county names match for all states in the census dataset
census_concat   <- paste0(census_data$CTYNAME)
hcw_data_concat <- paste0(dt3$county_name_corrected)

unmapped_locs <- dt3[!hcw_data_concat%in%census_concat,]

if(nrow(unmapped_locs)>0){
  print(unique(unmapped_locs[, c("county_name_corrected", "provider_state")]))
  stop("You have locations in the data that aren't in the final list of counties!")
}

# merge county population estimates to the dataset
dt4 <- dt3 %>% full_join(census_data, by=c("county_name_corrected"="CTYNAME", "state_name"="STNAME")) %>%
  # add number of healthcare facilities in each county
  inner_join(dt, by=c("provider_state", "county"))

# subset columns of interest
final_data <- dt4 %>% select(provider_state, state_name, county, county_mean_hcw_coverage, county_performance, POPESTIMATE, n) %>%
  # rename columns to make more sense
  rename("state_abbreviation"="provider_state",
         "average_hwc_cv19_vacc_in_county"="county_mean_hcw_coverage",
         "county_hcw_coverage_relative_to_counties_in_state"="county_performance",
         "total_population_estimate"="POPESTIMATE",
         "number_of_facilities_per_county"="n")
  
# export as spreadsheet
write.csv(final_data, 
          file = "C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/06_county_hcw_vaccine_information_update12June2023.csv", 
          row.names = FALSE)
