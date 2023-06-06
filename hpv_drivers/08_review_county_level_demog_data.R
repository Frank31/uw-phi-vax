# 09_review_county_level_demog_data
# Francisco Rios Casas
# May 16, 2023

# Purpose: to review county level data on variables such as:
#  - Race, Sex, Language spoken at home, Family income to identify:
# demographically diverse counties and counties with 
# sufficient population with the appropriate age group.
# in order to select appropriate counties for analyses,

# US State Population Tables: 
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html

# Source the set-up file
source("./01_set_up_file.R")

# FLORIDA age groups

# load Florida County Populations by Characteristics
fl_pop_data <- read.csv(file=paste0(data_folder, "raw_data/census_county_demographics/FL/cc-est2021-agesex-12.csv")) %>%
  # select columns of interest
  select(STNAME, CTYNAME, YEAR, POPESTIMATE, AGE1417_TOT, AGE18PLUS_TOT, AGE1519_TOT) %>%
  # filter out the year that the data corresponds to 
  filter(YEAR==1) %>% 
  # calculate new columns of interest
  mutate(proportion_under18 = 1 - (AGE18PLUS_TOT/POPESTIMATE)) %>%
  mutate(proportion_1417 = AGE1417_TOT/POPESTIMATE) %>%
  mutate(proportion_1519 = AGE1519_TOT/POPESTIMATE) %>%
  # select counties of interest in NY
  filter(CTYNAME %in% c("Orange County", "Duval County", "Broward County",
  "Miami-Dade County", "Hillsborough County", "Palm Beach County")) %>%
  # create new county variable
  mutate(county = case_when(
    CTYNAME == "Orange County" ~ "Orange",
    CTYNAME == "Duval County" ~ "Duval",
    CTYNAME == "Broward County" ~ "Broward",
    CTYNAME == "Miami-Dade County" ~ "Miami-Dade",
    CTYNAME == "Hillsborough County" ~ "Hillsborough",
    CTYNAME == "Palm Beach County" ~ "Palm Beach",
  )) %>%
  # Select the variables we need
  select(county, POPESTIMATE, proportion_1417, proportion_1519)


## New York Age Groups
# load Florida County Populations by Characteristics
ny_pop_data <- read.csv(file=paste0(data_folder, "raw_data/census_county_demographics/NY/cc-est2021-agesex-36.csv")) %>%
  # select columns of interest
  select(STNAME, CTYNAME, YEAR, POPESTIMATE, AGE1417_TOT, AGE18PLUS_TOT, AGE1519_TOT) %>%
  # filter out the year that the data corresponds to 
  filter(YEAR==1) %>% 
  # calculate new columns of interest
  mutate(proportion_under18 = 1 - (AGE18PLUS_TOT/POPESTIMATE)) %>%
  mutate(proportion_1417 = AGE1417_TOT/POPESTIMATE) %>%
  mutate(proportion_1519 = AGE1519_TOT/POPESTIMATE) %>%
  # select counties of interest in FL
  filter(CTYNAME %in% c("Nassau County", "Suffolk County", "Bronx County",
                        "Kings County", "New York County", "Queens County")) %>%
  # create new county variable
  mutate(county = case_when(
    CTYNAME == "Nassau County" ~ "Nassau",
    CTYNAME == "Suffolk County" ~ "Suffolk",
    CTYNAME == "Bronx County" ~ "Bronx",
    CTYNAME == "Kings County" ~ "Kings",
    CTYNAME == "New York County" ~ "New York",
    CTYNAME == "Queens County" ~ "Queens",
  )) %>%
  # Select the variables we need
  select(county, POPESTIMATE, proportion_1417, proportion_1519)

# load Florida County Populations by race and ethnicity
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html
fl_race_data <- read.csv(file=paste0(data_folder, "raw_data/census_county_demographics/FL/cc-est2021-alldata-12.csv")) %>%
  # select columns of interest
  select(STATE, CTYNAME, YEAR, AGEGRP, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE,
         IA_MALE, IA_FEMALE, H_MALE, H_FEMALE, AAC_MALE, AAC_FEMALE) %>%
  # filter out the year that the data corresponds to 
  filter(YEAR==1) %>% 
  # calculate new columns of interest
  mutate(proportion_wa = round((WA_MALE + WA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_ba = round((BA_MALE + BA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_ia = round((IA_MALE + IA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_h  = round((H_MALE + H_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_aac = round((AAC_MALE + AAC_FEMALE)/TOT_POP*100)) %>%
  # filter out age groups of interest
  # ten to 14 year olds
  filter(AGEGRP %in% c(3)) %>%
  # select counties of interest in NY
  filter(CTYNAME %in% c("Orange County", "Duval County", "Broward County",
                        "Miami-Dade County", "Hillsborough County", "Palm Beach County")) %>%
  # create new county variable
  mutate(county = case_when(
    CTYNAME == "Orange County" ~ "Orange",
    CTYNAME == "Duval County" ~ "Duval",
    CTYNAME == "Broward County" ~ "Broward",
    CTYNAME == "Miami-Dade County" ~ "Miami-Dade",
    CTYNAME == "Hillsborough County" ~ "Hillsborough",
    CTYNAME == "Palm Beach County" ~ "Palm Beach",
  )) %>%
  # Select the variables we need
  select(county, proportion_wa, proportion_ba, proportion_ia, proportion_h, proportion_aac)

# Load NY county populations by race and ethnicity
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-detail.html
ny_race_data <- read.csv(file=paste0(data_folder, "raw_data/census_county_demographics/NY/cc-est2021-alldata-36.csv")) %>%
  # select columns of interest
  select(STATE, CTYNAME, YEAR, AGEGRP, TOT_POP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE,
         IA_MALE, IA_FEMALE, H_MALE, H_FEMALE, AAC_MALE, AAC_FEMALE) %>%
  # filter out the year that the data corresponds to 
  filter(YEAR==1) %>% 
  # calculate new columns of interest
  mutate(proportion_wa = round((WA_MALE + WA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_ba = round((BA_MALE + BA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_ia = round((IA_MALE + IA_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_h  = round((H_MALE + H_FEMALE)/TOT_POP*100)) %>%
  mutate(proportion_aac = round((AAC_MALE + AAC_FEMALE)/TOT_POP*100)) %>%
  # filter out age groups of interest
  # ten to 14 year olds
  filter(AGEGRP %in% c(3)) %>%
  # select counties of interest in NY
  filter(CTYNAME %in% c("Nassau County", "Suffolk County", "Bronx County",
                        "Kings County", "New York County", "Queens County")) %>%
  # create new county variable
  mutate(county = case_when(
    CTYNAME == "Nassau County" ~ "Nassau",
    CTYNAME == "Suffolk County" ~ "Suffolk",
    CTYNAME == "Bronx County" ~ "Bronx",
    CTYNAME == "Kings County" ~ "Kings",
    CTYNAME == "New York County" ~ "New York",
    CTYNAME == "Queens County" ~ "Queens",
  )) %>%
  # Select the variables we need
  select(county, proportion_wa, proportion_ba, proportion_ia, proportion_h, proportion_aac)

# first merge state datasets together
fl_race_and_pop_data <- fl_pop_data %>% merge(fl_race_data, by="county")
ny_race_and_pop_data <- ny_pop_data %>% merge(ny_race_data, by="county")

# row bind the NY and FL data together
race_and_pop_data <- rbind(fl_race_and_pop_data, ny_race_and_pop_data)

# prep the following data points:
lang_ed_file <- "ACSDP1Y2021.DP02-2023-05-18T205443.xlsx" # languages spoken at home, educational attainment
# poverty_file <- "ACSST1Y2021.S1701-2023-05-18T225038.xlsx" # poverty
income_file <- "ACSST5Y2021.S1901-2023-05-18T213401.xlsx" # median income file

# load each of the files
lang_ed_data <- read_xlsx(path=paste0(data_folder, "raw_data/census_county_demographics/NY-FL/", lang_ed_file), sheet = 2)
# poverty_data <- read_xlsx(path=paste0(data_folder, "raw_data/census_county_demographics/NY-FL/", poverty_file), sheet = 2)
income_data  <- read_xlsx(path=paste0(data_folder, "raw_data/census_county_demographics/NY-FL/",income_file), sheet = 2)

# subset rows of interest
educ_data    <- lang_ed_data[c(70,76),c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48)]
lang_data    <- lang_ed_data[c(130:132), c(1, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48)]
# poverty_data <- poverty_data[]
income_data  <- income_data[c(7,9,10, 13), c(1,2,10, 18, 26, 34, 42, 50, 58, 66, 74, 82, 90)]

# rename the columns according to the counties of interest
names(educ_data) <- c("educ_attainment", "Broward", "Duval", "Hillsborough", "Miami-Dade", 
                      "Orange", "Palm Beach", "Bronx", "Kings", "Nassau", "New York", "Queens", 
                      "Suffolk")

# rename the columns according to the counties of interest
names(lang_data) <- c("languages_spoken", "Broward", "Duval", "Hillsborough", "Miami-Dade", 
                      "Orange", "Palm Beach", "Bronx", "Kings", "Nassau", "New York", "Queens", 
                      "Suffolk")

# rename the columns according to the counties of interest
names(income_data) <- c("income_brackets", "Broward", "Duval", "Hillsborough", "Miami-Dade", 
                        "Orange", "Palm Beach", "Bronx", "Kings", "Nassau", "New York", "Queens", 
                        "Suffolk")

# pivot each dataset longer format
educ_data_long   <- educ_data %>%   pivot_longer(!educ_attainment, names_to = "county", values_to = "percentage")
lang_data_long   <- lang_data %>%   pivot_longer(!languages_spoken, names_to = "county", values_to = "percentage")
income_data_long <- income_data %>% pivot_longer(!income_brackets, names_to = "county", values_to = "percentage") 

# rename variable columns to pivot one more time
educ_data_final <- educ_data_long %>% 
  mutate(educ_att_percent = 
           case_when(educ_attainment == "9th to 12th grade, no diploma" ~ "less_than_high_school",
                     educ_attainment == "High school graduate or higher" ~ "high_school_or_more")) %>%
  select(county, percentage, educ_att_percent) %>%
  pivot_wider(id_cols = "county", names_from = "educ_att_percent", values_from = "percentage")

lang_data_final <- lang_data_long %>%
  mutate(lang_spok_percent = 
           case_when(languages_spoken == "English only" ~ "english_only",
                     languages_spoken == "Language other than English" ~ "other_language",
                     languages_spoken == 'Speak English less than "very well"' ~ "english_less_well")) %>%
  select(county, percentage, lang_spok_percent) %>% 
  pivot_wider(id_cols = "county", names_from = "lang_spok_percent", values_from = "percentage")

income_data_final <- income_data_long %>%
  mutate(income_brack_percent = 
           case_when(income_brackets == "$25,000 to $34,999" ~ ">25to<35",
                     income_brackets == "$50,000 to $74,999" ~ ">50to<75",
                     income_brackets == "$75,000 to $99,999" ~ ">75to<100",
                     income_brackets == "$200,000 or more"   ~ ">200")) %>%
  select(county, percentage, income_brack_percent) %>%
  pivot_wider(id_cols = "county", names_from = "income_brack_percent", values_from = "percentage")

# load the file with county-level vaccination rates
county_rates <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/08_county_results_18may2023.csv")

# merge according to county name
final_sheet <- county_rates %>%
  merge(race_and_pop_data, by = "county") %>% 
  merge(educ_data_final, by = "county") %>%
  merge(lang_data_final, by = "county") %>%
  merge(income_data_final, by = "county") 

# export the file as an xlsx sheet
writexl::write_xlsx(final_sheet, path ="C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/09_county_level_demographics_19may2023.xlsx" )

# write a few summaries of the difference between the counties and which would be
# good ones to pick

