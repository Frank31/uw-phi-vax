# Francisco Rios Casas
# File name: 03_prep_govt_trust_data.R
# October 13 2022

# set up
source("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/third_version/01_set_up_R.R")

# Load data
load(paste0(raw_data_dir, "index_variables/world_values_survey/WVS_TimeSeries_1981_2022_Rdata_v3_0.rdata"))

# subset variables of interest
dt <- WVS_TimeSeries_1981_2022_spss_v3_0 %>% 
  # Country Name, ISO code, Year of survey, Country-wave, respondent ID, Govconfid
  select(COUNTRY_ALPHA, S003, S020, S024, S006, E069_11)

# rename columns
names(dt) <- c("iso_code", "iso_num_code", "year", "country_wave", "id", "govt_trust")

# restructure variables
dt$iso_num_code <- as.character(dt$iso_num_code)

# fix missing digits in ISO Code (variable needs to be stored as string character to avoid dropping leading zeros)
dt <- dt %>%
  mutate(iso_num_code = case_when(
         iso_code == "ALB" ~ "008",
         iso_code == "DZA" ~ "012",
         iso_code == "AND" ~ "020", 
         iso_code == "ARG" ~ "032",
         iso_code == "ARM" ~ "051", 
         iso_code == "AUS" ~ "036",
         iso_code == "AZE" ~ "031",
         iso_code == "BOL" ~ "068",
         iso_code == "BGD" ~ "050",
         iso_code == "BIH" ~ "070",
         iso_code == "BRA" ~ "076",
         TRUE ~ iso_num_code))

# coding of the govt_trust data
# 1.- A great deal
# 2.- Quite a lot
# 3.- Not very much
# 4.- None at all
# -1-.- Don´t know
# -2-.- No answer
# -4-.- Not asked
# -5-.- Missing; Not available

# re-code variables as missing
dt <- dt %>%
  mutate(govt_trust_recode = case_when(
    govt_trust >0 ~ govt_trust))

# create count of each variable for each country and year
dt_count <- dt %>% count(iso_code, iso_num_code, year, govt_trust_recode)

# reshape data by country and year
dt_wide <- dt_count %>% pivot_wider(names_from = govt_trust_recode, values_from = n)

# calculate the total respondents (total), as well as how many responded "A great
# deal" or "Quite a lot" (1 or 2)
dt_calculations <- dt_wide %>%
  rowwise() %>% 
  mutate(trust = sum(c_across(`1`:`2`), na.rm=TRUE),
         total = sum(c_across(`1`:`NA`), na.rm = T))
# calculate percent trust
dt_calculations$gov_trust <- dt_calculations$trust/dt_calculations$total

# drop locations where the question wasn't included
dt_calculations <- dt_calculations %>% 
  mutate(gov_trust = case_when(
    trust>0 ~ gov_trust
  ))

# subset columns of interest
final_data <- dt_calculations %>% select(iso_code, iso_num_code, year, gov_trust)

# Save the gov_trust data and try to finish prepping tomorrow!
saveRDS(final_data, file=paste0(prepped_data_dir, "aim_2/16_interrim_prepped_govt_trust_data.RDS"))

# standardize location name

# save
# exit