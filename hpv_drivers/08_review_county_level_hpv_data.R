# 08_review_county_level_data.R
# Francisco Rios Casas
# May 1 2023

# Purpose: for each state compare counties to each other on HPV coverage in each state and categorize states based on 
# their performance
# load data --the data is not exactly comparable between states but these are the criteria we are interested in:

# most recent year of data available

# for Florida: vaccination initiation among 11-12 year olds in 2020

# Source the set-up file
source("./01_set_up_file.R")

###########################
# Florida
###########################

# load the data
fl_data <- read_xlsx(path = paste0(data_folder, "raw_data/county_hpv_data/FL-Data-2020.xlsx"))

# re-name columns
names(fl_data) <- c("county", "count", "denom", "rate")

# drop extra rows
fl_data <- fl_data %>% filter(!county %in% c("Florida", "County"))

# convert factors to numeric
fl_data$rate <- as.numeric(fl_data$rate)

# calculate the average, the first quartile, and third quartile
fl_data$first_q <- quantile(fl_data$rate)[2]
fl_data$third_q <- quantile(fl_data$rate)[4]
fl_data$average <- mean(fl_data$rate)

# keep only counties of sufficient sample size
fl_data <- fl_data %>% filter(county %in% c("Miami-Dade", "Broward", "Palm Beach", "Hillsborough", "Orange", "Duval"))

# categorize how counties performed relative to each other
fl_data <- fl_data %>% mutate(
  ranking = case_when(
    rate >= first_q & rate <= third_q ~ "average",
    rate >  third_q ~ "better",
    rate <  first_q ~ "worse"
  )
)

# potential good candidates include: Orange and Duval Counties


###########################
# New York
###########################

# load the data
ny_data <- read_xlsx(paste0(data_folder, "raw_data/county_hpv_data/NYS_PA_exportData.xlsx"))

# rename columns
names(ny_data) <- c("region", "children_vaccinated", "total_children", "percentage")

# drop extra rows
ny_data <- ny_data[-c(1:11),]
ny_data <- ny_data %>% filter(!is.na(children_vaccinated))
ny_data <- ny_data %>% filter(!region %in% c("Long Island", "New York City", "Mid-Hudson", "Capital Region", "Mohawk Valley", "North Country", "Tug Hill Seaway", "Central NY", "Southern Tier", "Finger Lakes",
                                             "Western NY", "New York State (excluding NYC)", "New York State"))

# convert factors to numeric
ny_data$percentage <- as.numeric(ny_data$percentage)

# calculate the average, the first quartile, and third quartile
ny_data$first_q <- quantile(ny_data$percentage, na.rm = TRUE)[2]
ny_data$third_q <- quantile(ny_data$percentage, na.rm = TRUE)[4]
ny_data$average <- mean(ny_data$percentage, na.rm = TRUE)

# keep only columns of sufficient sample size
ny_data <- ny_data %>% filter(region %in% c("Kings", "Queens", "New York", "Suffolk", "Bronx", "Nassau"))

# categorize how counties performed relative to each other
ny_data <- ny_data %>% mutate(
  ranking = case_when(
    percentage >= first_q & percentage <= third_q ~ 'average',
    percentage > third_q ~ 'better',
    percentage < first_q ~ 'worse'
  )
)

# Potential good counties include: Nassau and Bronx

###########################
# Texas
###########################
tx_data <- read_xlsx(paste0(data_folder, "raw_data/county_hpv_data/ImmTrac2 Doses Administered For Web.xlsx"),
                     sheet = 2)

# rename columns
names(tx_data) <- c("county", "year", "month", "age", "vaccine", "number_of_vaccines")

# drop extra rows
tx_data <- tx_data[-1,]

# convert factors to numeric
tx_data$number_of_vaccines <- as.numeric(tx_data$number_of_vaccines)

# create new merge variable
tx_data$CTYNAME <- paste0(tx_data$county, " County")

# subset data of interest
tx_data <- tx_data %>% filter(vaccine == "Adolescent HPV") %>%
  filter(year=="2022") %>%
  group_by(county, CTYNAME, year) %>%
  summarise(summed_vaccines = sum(number_of_vaccines))

# add in population denominators
tx_denominators <- read.csv(file = paste0(data_folder, "raw_data/county_hpv_data/cc-est2021-agesex-48.csv")) %>%
  filter(YEAR==2) %>%
  select(CTYNAME, AGE1014_TOT, AGE1519_TOT, AGE1417_TOT) %>%
  mutate(denominator1 = AGE1014_TOT + AGE1519_TOT)

# merge county denominators into the numerators
final_tx_data <- merge(tx_data, tx_denominators, by=c("CTYNAME"))

# rate number one
# is this consistent with the values from the NIS teen survey????? that might be the best 
final_tx_data$percentage <- final_tx_data$summed_vaccines/final_tx_data$denominator1

final_tx_data <- final_tx_data %>%
  filter(CTYNAME %in% c("Harris County", "Dallas County", "Tarrant County", "Bexar County", "Travis County", "Collin County"))

#############
# export all of the files
#############
final_fl_data <- fl_data %>% 
  mutate(state="Florida") %>%
  select(state, county, rate, ranking) 

final_ny_data <- ny_data %>% 
  mutate(state="New York") %>%
  select(state, region, percentage, ranking) %>% 
  rename(county=region,
         rate=percentage)

# bind the two datasets together
final_output_table <- rbind(final_fl_data, final_ny_data)

# save the output table
write.csv(final_output_table, file=paste0("C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/08_county_results_18may2023.csv"))
