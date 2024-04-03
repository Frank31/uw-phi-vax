## Prep 2013 - 2022 Ontario Vaccination Data
# Author: Frances Gellert
# Purpose: This script will create complete Ontario dataset for past few years
# Date: Sept 27 2023

# clear all
rm(list=ls())

# source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))

## Load data ----
#LOAD MEDICAL AND NON-MEDICAL EXEMPTIONS
library(readxl)
ont13 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-2013-14-2015-16.xlsx", sheet = "A1.PHUCov7")
ont14 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-2013-14-2015-16.xlsx", sheet = "A2.PHUCov7")
ont15 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-2013-14-2015-16.xlsx", sheet = "A3.PHUCov7")
ont16 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-2016-17.xlsx", sheet = "A1.PHUCov7")
ont17 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-tables-2017-18.xlsx", sheet = "A1.PHUCov7")
#ont18-21 have different PHUs
ont18 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-tables-2018-19.xlsx", sheet = "C.T1.PHUCov7")
ont19 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/routine-childhood-coverage-appendix-2019-22.xlsx", sheet = "2019-20")
ont20 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/routine-childhood-coverage-appendix-2019-22.xlsx", sheet = "2020-21")
ont21 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/routine-childhood-coverage-appendix-2019-22.xlsx", sheet = "2021-22")

PHU_names36 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-tables-2017-18.xlsx", sheet = "A3.PHUs")
#   Effective May 1, 2018, Oxford County Public Health and Elgin-St. Thomas Health Unit have merged to become Southwestern Public Health. For this assessment, the former health units were reported on separately, reflecting the configuration in place for the majority of the 2017–18 school year.
PHU_names35 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/immunization-coverage-appendix-tables-2018-19.xlsx", sheet = "PHUs")
#   Perth District Health Unit dropped, reflected starting 2019-20 school year
PHU_names34 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_vacc_rates/ontario/routine-childhood-coverage-appendix-2019-22.xlsx", sheet = "PHUs")

# Prep/clean ----
# subset columns in each dataset ---
new_column_names <- c("PHU", "mea", "mumps", "rubella", "dip", "tet", "polio", "pert", "hib", "pneum", "mcc", "var")

colnames(ont13) <- new_column_names
ont13 <- ont13[-c(1, 38, 39), ] #delete rows 1, 38, and 39
colnames(ont14) <- new_column_names
ont14 <- ont14[-c(1, 38, 39), ]
colnames(ont15) <- new_column_names
ont15 <- ont15[-c(1, 38, 39), ]
colnames(ont16) <- new_column_names
ont16 <- ont16[-c(1, 38, 39), ]
colnames(ont17) <- new_column_names
ont17 <- ont17[-c(1, 38, 39), ]

colnames(ont18) <- new_column_names
ont18 <- ont18[-c(1, 37, 38), ]

colnames(ont19) <- new_column_names
ont19 <- ont19[-c(1, 36), ]
colnames(ont20) <- new_column_names
ont20 <- ont20[-c(1, 36), ]
colnames(ont21) <- new_column_names
ont21 <- ont21[-c(1, 36), ]

# make all rows except row 1 numerical 
char_columns <- sapply(ont13, class) == "character" & names(ont13) != "PHU"
ont13[ , char_columns] <- sapply(ont13[ , char_columns], as.numeric)
ont14[ , char_columns] <- sapply(ont14[ , char_columns], as.numeric)
ont15[ , char_columns] <- sapply(ont15[ , char_columns], as.numeric)
ont16[ , char_columns] <- sapply(ont16[ , char_columns], as.numeric)
ont17[ , char_columns] <- sapply(ont17[ , char_columns], as.numeric)
ont18[ , char_columns] <- sapply(ont18[ , char_columns], as.numeric)
ont19[ , char_columns] <- sapply(ont19[ , char_columns], as.numeric)
ont20[ , char_columns] <- sapply(ont20[ , char_columns], as.numeric)
ont21[ , char_columns] <- sapply(ont21[ , char_columns], as.numeric)

# check variable classes
ont17
sapply(ont17, class)

# abbreviate PHU column values from PHU_names dataframe
PHU_column_names <- c("code", "PHU")
colnames(PHU_names36) <- PHU_column_names
colnames(PHU_names35) <- PHU_column_names
colnames(PHU_names34) <- PHU_column_names
PHU_names36 <- PHU_names36[-c(1, 38), ] #remove rows 1 & 38
PHU_names35 <- PHU_names35[-c(1, 37), ] #remove rows 1 & 37
PHU_names34 <- PHU_names34[-c(1), ] #remove row 1
PHU_names36$code <- sapply(PHU_names36$code, function(x) gsub("[[:punct:]]", "", x)) #remove symbols from column values

# make abbreviated PHU code names, the values in each PHU column
PHU_short36 <- PHU_names36$code
ont13$PHU <- PHU_short36
ont14$PHU <- PHU_short36
ont15$PHU <- PHU_short36
ont16$PHU <- PHU_short36
ont17$PHU <- PHU_short36
PHU_short35 <- PHU_names35$code
ont18$PHU <- PHU_short35
PHU_short34 <- PHU_names34$code
ont19$PHU <- PHU_short34
ont20$PHU <- PHU_short34
ont21$PHU <- PHU_short34

#check new datasets
head(ont16, n = 2)
summary(ont16, na.rm = TRUE)


# Create combined dataset ----
# add a new column for year
ont13$year <- 2013
ont14$year <- 2014
ont15$year <- 2015
ont16$year <- 2016
ont17$year <- 2017
ont18$year <- 2018
ont19$year <- 2019
ont20$year <- 2020
ont21$year <- 2021

#HOW TO DO THE MERGE???
# combine the datasets into one, bound by new year variable
combined_data <- bind_rows(ont13, ont14, ont15, ont16, ont17, ont18, ont19, ont20, ont21)
view(combined_data)

# Compare average MEASLES coverage over time----
# calculate the average measles vaccine coverage by PHU and year
average_mea_coverage <- combined_data %>%
  group_by(PHU, year) %>%
  summarise(avg_mea = mean(mea, na.rm = TRUE))
average_mea_coverage

# line plot showing change in average measles coverage over time by PHU
average_mea_coverage$year <- as.factor(average_mea_coverage$year) # Convert the 'year' column to a factor to ensure correct x-axis labeling
plot <- ggplot(data = average_mea_coverage, aes(x = year, y = avg_mea, group = PHU, color = PHU)) +
  geom_line() +
  labs(x = "Year", y = "Average Measles Vaccine Coverage") +
  theme_minimal()
plot

#determine which PHU has the lowest value
lowest_index <- which.min(average_mea_coverage$avg_mea) # Find the index of the row with the lowest value in the "avg_mea" column
lowest_row <- average_mea_coverage[lowest_index, ] # Extract the entire row with the lowest value
lowest_row 
   #   PHU   year  avg_mea
   # 1 HAM   2014    57.3   (City of Hamilton Public Health Services)


# Compare average COMBINED MMR coverage over time----
# create a new 'MMR' column that contains the mean of measles, mumps, and rubella values
combined_data <- combined_data %>%
  mutate(MMR = rowMeans(select(., mea, mumps, rubella), na.rm = TRUE))
combined_data

# calculate the average MMR vaccine coverage by PHU and year
average_MMR_coverage <- combined_data %>%
  group_by(PHU, year) %>%
  summarise(avg_MMR = mean(MMR, na.rm = TRUE))
average_MMR_coverage

# line plot showing change in average MMR coverage over time by PHU
average_MMR_coverage$year <- as.factor(average_MMR_coverage$year) # Convert the 'year' column to a factor to ensure correct x-axis labeling
plot <- ggplot(data = average_MMR_coverage, aes(x = year, y = avg_MMR, group = PHU, color = PHU)) +
  geom_line() +
  labs(x = "Year", y = "Average MMR Vaccine Coverage") +
  theme_minimal()
plot

lowest_index <- which.min(average_MMR_coverage$avg_MMR) # Find the index of the row with the lowest value in the "avg_MMR" column
lowest_row <- average_MMR_coverage[lowest_index, ] # Extract the entire row with the lowest value
lowest_row

head(average_MMR_coverage)
head(combined_data)


# Compare average COMBINED DTaP coverage over time----
# create a new 'DTaP' column that contains the mean of diphtheria, tetanus, and pertussis values
combined_data <- combined_data %>%
  mutate(DTaP = rowMeans(select(., dip, tet, pert), na.rm = TRUE))
head(combined_data)

# calculate the average DTaP vaccine coverage by PHU and year
average_DTaP_coverage <- combined_data %>%
  group_by(PHU, year) %>%
  summarise(avg_DTaP = mean(DTaP, na.rm = TRUE))
average_DTaP_coverage

# line plot showing change in average DTaP coverage over time by PHU
average_DTaP_coverage$year <- as.factor(average_DTaP_coverage$year) # Convert the 'year' column to a factor to ensure correct x-axis labeling
plot <- ggplot(data = average_DTaP_coverage, aes(x = year, y = avg_DTaP, group = PHU, color = PHU)) +
  geom_line() +
  labs(x = "Year", y = "Average DTaP Vaccine Coverage") +
  theme_minimal()
plot


# Compare average coverage OF ALL VACCINES over time----
# create a new 'up_to_date' column that contains the mean of all vaccines
combined_data <- combined_data %>%
  mutate(up_to_date = rowMeans(select(., mea, mumps, rubella, dip, tet, polio, pert, hib, pneum, mcc, var), na.rm = TRUE))
head(combined_data)

# calculate the average up-to-date vaccine coverage by PHU and year
average_all_coverage <- combined_data %>%
  group_by(PHU, year) %>%
  summarise(avg_all = mean(up_to_date, na.rm = TRUE))
average_all_coverage

# line plot: change in average up-to-date coverage over time by PHU
average_all_coverage$year <- as.factor(average_all_coverage$year) # Convert the 'year' column to a factor to ensure correct x-axis labeling
line_plot <- ggplot(data = average_all_coverage, aes(x = year, y = avg_all, group = PHU, color = PHU)) +
  geom_line() +
  labs(x = "Year", y = "Average Up-to-date Vaccine Coverage") +
  theme_minimal()
line_plot

# bar plot: distribution of average up-to-date coverage for each year, you can use a box plot - see how the spread of data varies across years.
box_plot <- ggplot(average_all_coverage, aes(x = factor(year), y = avg_all)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Up-to-date Vaccine Coverage by Year",
       x = "Year",
       y = "Average Up-to-date Coverage") +
  theme_minimal()
box_plot


#Compare each Public Health Unit's (PHU) average improvement in up-to-date vaccine coverage over time ----

view(average_all_coverage)
view(combined_data)

# Step 1: calculate the change in vaccine coverage for each year
average_change <- average_all_coverage %>%
  group_by(PHU) %>%
  arrange(PHU, year) %>%
  mutate(change_in_coverage = avg_all - lag(avg_all)) #mutate function takes each value in the avg_all column, subtracts the previous value (the value in the row above it), and stores the result in the new change_in_coverage column.
view(average_change)

# Step 2: calculate the average change in vaccine coverage for each PHU from 2013-2017
average_change <- average_change %>%
  group_by(PHU) %>%
  summarise(avg_change = mean(change_in_coverage, na.rm = TRUE))
average_change

# Step 3: visualize the results 
# bar plot
bar_plot <- ggplot(average_change, aes(x = PHU, y = avg_change)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Average Change in Up-to-date Vaccine Coverage by PHU (2013-2017)",
       x = "Public Health Unit (PHU)",
       y = "Average Percent Change in Up-to-date Coverage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
bar_plot



## THIS STUFF IS FRANCISCO'S: ----
# subset columns in each dataset -----
ont13 <- NISPUF07 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP07, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, # demographics
                             INS_1, INS_2, INS_3, INS_4, INS_5, INS_6, INS_11,  # insurance information
                             P_NUMHEP, P_NUMMMR, P_NUMDTP) # vaccination doses

ont14 <- NISPUF08 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP08, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4, INS_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

ont15 <- NISPUF09 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP09, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

ont16 <- NISPUF10 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP10, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

ont17 <- NISPUF11 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP11, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)

ont18 <- NISPUF12 %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, ESTIAP12, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                             INS_1, INS_2, INS_3, INS_4_5, INS_6, INS_11, 
                             P_NUMHEP, P_NUMMMR, P_NUMDTP)


# merge datasets together into four groups based on iterations of key variables -----
data1 <- bind_rows(nis07, nis08)
data2 <- bind_rows(nis09, nis10, nis11, nis12, nis13, nis14, nis15)
data3 <- nis16
data4 <- bind_rows(nis17, nis18, nis19, nis20)

# create indicator of insurance type-----

# replace NAs with 0 for datasets with more detailed insurance indicators
data1 <- data1 %>% replace_na(list(INS_1 = 0, INS_2 = 0, INS_3 = 0, INS_4   = 0, INS_5 = 0, INS_6 = 0))
data2 <- data2 %>% replace_na(list(INS_1 = 0, INS_2 = 0, INS_3 = 0, INS_4_5 = 0, INS_6 = 0))

# create three new indicator variables for private insurance, medicaid, or other insurance

# ------
# WHAT ABOUT MISSIGNESS: HOW TO TAKE THOSE INTO ACCOUNT?
# WHAT ABOUT BREAK IN INSURANCE
# -----

data1 <- data1 %>%
  mutate(private_ins = case_when(
    INS_1==1 ~ 1,
    TRUE ~ 0 )) %>%
  mutate(medicaid = case_when(
    INS_2==1 ~ 1,
    TRUE ~ 0 )) %>% 
  mutate(other_ins = case_when(
    INS_3==1 | INS_4==1 | INS_5==1 | INS_6==1 ~ 1, # any other insurance 
    TRUE ~ 0 ))

data2 <- data2 %>% 
  mutate(private_ins = case_when(
    INS_1==1 ~ 1,
    TRUE ~ 0 )) %>%
  mutate(medicaid = case_when(
    INS_2==1 ~ 1, 
    TRUE ~ 0)) %>%
  mutate(other_ins = case_when(
    INS_3==1 | INS_4_5==1 | INS_6==1 ~ 1,
    TRUE ~ 0))

# create composite index to create variable that matches other years
data1 <- data1 %>% 
  mutate(INSURANCE = case_when(
    private_ins==1 & medicaid==0 & other_ins==0 ~ 1, # private insurance only
    private_ins==0 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==1 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==0 & medicaid==0 & other_ins==0 ~ 4  # uninsured
  ))

data2 <- data2 %>% 
  mutate(INSURANCE = case_when(
    private_ins==1 & medicaid==0 & other_ins==0 ~ 1, # private insurance only
    private_ins==0 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==1 ~ 2, # any medicaid
    private_ins==1 & medicaid==1 & other_ins==0 ~ 2, # any medicaid
    private_ins==0 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==1 & medicaid==0 & other_ins==1 ~ 3, # other insurance
    private_ins==0 & medicaid==0 & other_ins==0 ~ 4  # uninsured
  ))

data3$INSURANCE <- data3$INS_STAT_I

data4$INSURANCE <- data4$INS_STAT2_I

# bind each data frame together ----
full_data <- bind_rows(data1, data2, data3, data4)

# create new variable of original estimation area
full_data <- full_data %>%
  mutate(ESTIAP_ORIG = case_when(
    YEAR==2007 ~ ESTIAP07,
    YEAR==2008 ~ ESTIAP08,
    YEAR==2009 ~ ESTIAP09,
    YEAR==2010 ~ ESTIAP10,
    YEAR==2011 ~ ESTIAP11,
    YEAR==2012 ~ ESTIAP12,
    YEAR==2013 ~ ESTIAP13,
    YEAR==2014 ~ ESTIAP14,
    YEAR==2015 ~ ESTIAP15,
    YEAR==2016 ~ ESTIAP16,
    YEAR==2017 ~ ESTIAP17, 
    YEAR==2018 ~ ESTIAP18,
    YEAR==2019 ~ ESTIAP19,
    YEAR==2020 ~ ESTIAP20))

# create new variable that will standardize estimation areas
full_data <- full_data %>%
  mutate(ESTIAP = case_when(
    ESTIAP_ORIG==15 ~ 14, # City of Baltimore
    ESTIAP_ORIG==24 ~ 22, #Miami-Dade County
    ESTIAP_ORIG==37 ~ 36, # IN-Marion County
    ESTIAP_ORIG==52 ~ 51, # TX-Dallas County
    ESTIAP_ORIG==53 ~ 51, # TX-El Paso County
    ESTIAP_ORIG==69 ~ 68, # CA-Los Angeles County
    ESTIAP_ORIG==70 ~ 68, # CA-Santa Clara County
    ESTIAP_ORIG==79 ~ 68, # CA-Alameda County
    ESTIAP_ORIG==80 ~ 68, # CA-San Bernardino County
    ESTIAP_ORIG==85 ~ 68, # CA- Northern Ca
    ESTIAP_ORIG==91 ~ 22, # FL-Orange County
    ESTIAP_ORIG==92 ~ 34, # IL-Madison/St.Clair Counties
    ESTIAP_ORIG==93 ~ 40, # MN-Twin Cities
    ESTIAP_ORIG==96 ~ 36, # IN-Lake County
    ESTIAP_ORIG==97 ~ 77, # WA-Eastern Washington
    ESTIAP_ORIG==102 ~ 77, # WA-Western WA
    ESTIAP_ORIG==103 ~ 14, # MD-Prince George's County
    ESTIAP_ORIG==107 ~ 51, # TX-Hidalgo County
    ESTIAP_ORIG==108 ~ 51, # TX-Travis County
    ESTIAP_ORIG==109 ~ 51, # TX-Tarrant County
    ESTIAP_ORIG==773 ~ 77, # TX-Western Washington
    ESTIAP_ORIG==774 ~ 77, # WA-Eastern/Western WA
    TRUE ~ as.double(ESTIAP_ORIG)
  ))

# create new indicator of fully vaccinated status
full_data <- full_data %>% 
  mutate(dtp_vac = case_when(
    P_NUMDTP>=4 ~ 1,
    P_NUMDTP<4 ~ 0)) %>% 
  mutate(mmr_vac = case_when(
    P_NUMMMR>=1 ~ 1,
    P_NUMMMR<1 ~ 0)) %>% 
  mutate(hep_vac = case_when(
    P_NUMHEP>=3 ~ 1,
    P_NUMHEP<3 ~ 0))

# change reference values for raceethk
full_data <- full_data %>% 
  mutate(RACEETHK_R = case_when(
    RACEETHK==4 ~ 4,
    RACEETHK==3 ~ 3,
    RACEETHK==2 ~ 1, 
    RACEETHK==1 ~ 2))


full_data <- full_data %>% 
  filter(PDAT==1) %>% # filter children with adequate provider data
  filter(ESTIAP!=95 & ESTIAP!=105 & ESTIAP!=106) # filter US Virgin Islands, Guam, Puerto Rico (only a few surveys)


# select columns of interest in each data set
full_data <- full_data %>% select(SEQNUMC, PDAT, YEAR, AGEGRP, RACEETHK, RACEETHK_R, EDUC1, INCQ298A, INCPOV1, LANGUAGE, MOBIL_I, 
                              INSURANCE, ESTIAP, ESTIAP_ORIG, 
                              P_NUMHEP, P_NUMMMR, P_NUMDTP, dtp_vac, mmr_vac, hep_vac)

# Save final dataset
saveRDS(full_data, file = paste0(prepped_data_dir, "01_complete_nis_data.RDS"))
