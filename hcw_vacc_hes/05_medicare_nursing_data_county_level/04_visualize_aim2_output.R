# 04_visualize_aim2_output.R
# Francisco Rios Casas
# April 27, 2023

# Create an output table with the following columns
# State, county, facility average, average community, 
# facility rank in state, community rank in state

# load set up file
source("./01_set_up_file.R")

# load healthcare worker data
final_data <- read.csv("C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/06_county_hcw_vaccine_information.csv")

# subset initial columns
facility_columns <- final_data %>% 
  filter(state_name %in% c("Colorado", "Georgia", "Massachusetts")) %>%
  filter(county %in% c("Berkshire", "Worcester", "Essex",
                       "Pueblo", "Larimer", "Adams",
                       "Floyd", "Richmond", "Gwinnett"))

# recode merge variable
facility_columns$recip_county <- paste0(facility_columns$county, " County")

# load and merge covid county data
co_data <- readRDS(paste0(data_folder, "prepped_data/cdc_covid_vax_tracker/CO_county_level_data_27April2023.RDS"))
ga_data <- readRDS(paste0(data_folder, "prepped_data/cdc_covid_vax_tracker/GA_county_level_data_27April2023.RDS"))
ma_data <- readRDS(paste0(data_folder, "prepped_data/cdc_covid_vax_tracker/MA_county_level_data_27April2023.RDS"))

# save vaccination values as numeric values
co_data$series_complete_18pluspop_pct <- as.numeric(co_data$series_complete_18pluspop_pct)
ga_data$series_complete_18pluspop_pct <- as.numeric(ga_data$series_complete_18pluspop_pct)
ma_data$series_complete_18pluspop_pct <- as.numeric(ma_data$series_complete_18pluspop_pct)

# calculate the county rankings in each state
co_data$first_quant <- quantile(co_data$series_complete_18pluspop_pct, na.rm = TRUE)[2]
co_data$third_quant <- quantile(co_data$series_complete_18pluspop_pct, na.rm = TRUE)[4]

ga_data$first_quant <- quantile(ga_data$series_complete_18pluspop_pct, na.rm = TRUE)[2]
ga_data$third_quant <- quantile(ga_data$series_complete_18pluspop_pct, na.rm = TRUE)[4]

ma_data$first_quant <- quantile(ma_data$series_complete_18pluspop_pct, na.rm = TRUE)[2]
ma_data$third_quant <- quantile(ma_data$series_complete_18pluspop_pct, na.rm = TRUE)[4]

# calculate county rankings for each state
co_data <- co_data %>% mutate(county_ranking = case_when(
  series_complete_18pluspop_pct >= first_quant & series_complete_18pluspop_pct <= third_quant ~ "average",
  series_complete_18pluspop_pct > third_quant ~ "better",
  series_complete_18pluspop_pct < first_quant ~ "worse"
))

ga_data <- ga_data %>% mutate(county_ranking = case_when(
  series_complete_18pluspop_pct >= first_quant & series_complete_18pluspop_pct <= third_quant ~ "average",
  series_complete_18pluspop_pct > third_quant ~ "better",
  series_complete_18pluspop_pct < first_quant ~ "worse"
))

ma_data <- ma_data %>% mutate(county_ranking = case_when(
  series_complete_18pluspop_pct >= first_quant & series_complete_18pluspop_pct <= third_quant ~ "average",
  series_complete_18pluspop_pct > third_quant ~ "better",
  series_complete_18pluspop_pct < first_quant ~ "worse"
))

# row_bind all of the data together
covid_county_columns <- do.call("rbind", list(co_data, ga_data, ma_data)) %>% 
  filter(recip_county %in% c("Berkshire County", "Worcester County", "Essex County",
                       "Pueblo County", "Larimer County", "Adams County",
                       "Floyd County", "Richmond County", "Gwinnett County"))

# Merge the two datasets
# also merge by state to make sure no duplicate names get added in
final_table <- facility_columns %>% full_join(covid_county_columns, by="recip_county")

# subset columns of interest and re-order
output_table <- final_table %>% select("state_name", "recip_county", 
                                       "average_hwc_cv19_vacc_in_county", 
                                       "series_complete_18pluspop_pct", 
                                       "county_hcw_coverage_relative_to_counties_in_state",
                                       "county_ranking")

# rename the columns that will be in the final output table
final_output_table <- output_table %>% rename('State'='state_name',
                        'County'='recip_county',
                        'Vacc_Rate_Healthcare_Facilities'='average_hwc_cv19_vacc_in_county',
                        'Vacc_Rate_General_Population'='series_complete_18pluspop_pct',
                        'Ranking_Healthcare_Facilities'='county_hcw_coverage_relative_to_counties_in_state',
                        'Ranking_General_Population'='county_ranking')

final_output_table <- arrange(final_output_table, State)

# save the final output table
write.csv(final_output_table, "C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/07_aim_2_study_output.csv",
          row.names = FALSE)
