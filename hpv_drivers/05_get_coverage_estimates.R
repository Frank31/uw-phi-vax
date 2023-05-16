# 03_get_coverage_estimates.R
# Francisco Rios Casas
# March 15 2023

# Purpose: estimate HPV coverage in each state

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

years_with_data    <- seq(2012,2021)

# loop through each dataset to extract the percent of teen's that initiated with HPV vaccinations
for (i in 1:length(years_with_data)){
  # un comment to trouble shoot
  # i <- 3
  
  # set the initial parameters of data to be prepped
  year      <- years_with_data[i]
  file_name <- paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_",year,".rds")
  
  # print statement so you know progress of code
  print(paste0(i, " Now prepping ", year, " NIS Teen Data"))
  
  # load the data
  NISTEEN <- read_rds(file_name)
  
  # calculate how many teens have initiated HPV vaccination
  if (year == 2014){
    NISTEEN <- subset(NISTEEN, HPVI_ANY_REC %in% c("YES", "NO"))
  } else {
    NISTEEN <- subset(NISTEEN, HPVI_ANY %in% c("YES", "NO"))
  }
  
  # drop states that are missing (territories)
  NISTEEN <- NISTEEN %>% filter(!is.na(STATE))
  
  # Specify a sampling design
  # Create survey design
  if (year %in% c(2012, 2013, 2014, 2015, 2016, 2017)){
    nisteendsgn <- svydesign(
      id=~SEQNUMT,
      strata = ~STRATUM,
      weights = ~RDDWT_D,
      data = NISTEEN
    )
  } else {
    nisteendsgn <- svydesign(
      id=~SEQNUMT,
      strata = ~STRATUM,
      weights = ~RDDWT_C,
      data = NISTEEN)
  }
  
  
  # HPV Any doses = YES ESTIMATES BY STATE
  if (year==2014){
    r_est3 <- svyby(~HPVI_ANY_REC, ~STATE, nisteendsgn, svymean)
  } else {
    r_est3 <- svyby(~HPVI_ANY, ~STATE, nisteendsgn, svymean)
  }
  
  # if year is 2014 then rename columns of interest
  if (year == 2014){
    # mutate 
    r_est3 <- r_est3 %>% 
      mutate(HPVI_ANYYES = HPVI_ANY_RECYES) %>%
      mutate(HPVI_ANYNO = HPVI_ANY_RECNO) %>%
      mutate(se.HPVI_ANYYES = se.HPVI_ANY_RECYES) %>%
      mutate(se.HPVI_ANYNO = se.HPVI_ANY_RECNO)
  }
  
  # subset to columns of interest
  estimated_rates <- r_est3 %>% select(STATE, HPVI_ANYYES, HPVI_ANYNO, se.HPVI_ANYYES, se.HPVI_ANYNO) %>%
    # add in additional columns
    mutate(year = year)
  
  # save the individual extracted data
  saveRDS(estimated_rates, file = paste0(data_folder, "prepped_data/cdc_nis_teen/hpv_estimates_", year, ".RDS"))
 
  # append the different years together using rbind
  if (i == 1){
    extracted_data <- estimated_rates
  } else {
    extracted_data <- plyr::rbind.fill(extracted_data, estimated_rates)
  }
}

# save the prepped data
saveRDS(extracted_data, file = paste0(data_folder, "prepped_data/cdc_nis_teen/merged_nis_estimates_2012-2021.RDS"))

# use this section to estimate proportion of fully vaccinated teens with HPV
rm(list=ls())

# Source the set-up file
source("./01_set_up_file.R")

years_with_data <- seq(2012,2021)

# loop through each dataset to extract the percent of teen's that were fully vaccinated with HPV vaccinations
for (i in 1:length(years_with_data)){
  # un comment to trouble shoot
  i <- 1
  
  # set the initial parameters of data to be prepped
  year      <- years_with_data[i]
  file_name <- paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_",year,".rds")
  
  # print statement so you know progress of code
  print(paste0(i, " Now prepping ", year, " NIS Teen Data"))
  
  # load the data
  NISTEEN <- read_rds(file_name)
  
  # # subset variable indicating how many teens have completed HPV vaccination
  # if (year == 2014){
  #   NISTEEN <- subset(NISTEEN, HPVI_NUM_REC %in% c("YES", "NO"))
  # } else {
  #   NISTEEN <- subset(NISTEEN, HPVI_NUM_TOT %in% c("YES", "NO"))
  # }
  
  # drop states that are missing (territories)
  NISTEEN <- NISTEEN %>% filter(!is.na(STATE))
  
  # calculate all shots variable
  NISTEEN <- NISTEEN %>% 
    mutate(
      HPVI_FULL_VACC = HPVI_NUM_TOT
    ) %>%
    mutate(
      HPVI_FULL_VACC = case_when(
        HPVI_FULL_VACC %in% c("3", "4", "5", "6", "7", "8", "9", "50") ~ "ALL SHOTS",
        HPVI_FULL_VACC %in% c("1", "2") ~ "Incomplete",
        HPVI_FULL_VACC %in% c("0") ~ "None", 
        HPVI_FULL_VACC %in% c("77") ~ "DON'T KNOW",
        HPVI_FULL_VACC %in% c("99") ~ "MISSING IN ERROR"
      )
    )
  
  # Specify a sampling design
  # Create survey design
  if (year %in% c(2012, 2013, 2014, 2015, 2016, 2017)){
    nisteendsgn <- svydesign(
      id=~SEQNUMT,
      strata = ~STRATUM,
      weights = ~RDDWT_D,
      data = NISTEEN
    )
  } else {
    nisteendsgn <- svydesign(
      id=~SEQNUMT,
      strata = ~STRATUM,
      weights = ~RDDWT_C,
      data = NISTEEN)
  }
  
  # HPV All shots = YES ESTIMATES BY STATE
  if (year==2014){
    r_est3 <- svyby(~HPVI_NUM_REC, ~STATE, nisteendsgn, svymean)
  } else {
    r_est3 <- svyby(~HPVI_FULL_VACC, ~STATE, nisteendsgn, svymean)
  }
  
  # if year is 2014 then rename columns of interest
  if (year == 2014){
    # mutate 
    r_est3 <- r_est3 %>% 
      mutate(HPVI_ANYYES = HPVI_ANY_RECYES) %>%
      mutate(HPVI_ANYNO = HPVI_ANY_RECNO) %>%
      mutate(se.HPVI_ANYYES = se.HPVI_ANY_RECYES) %>%
      mutate(se.HPVI_ANYNO = se.HPVI_ANY_RECNO)
  }
  
  
}






# create a vector of the states present in the dataset
# areas_with_data <- unique(NISTEEN$ESTIAPT20)

# loop through each state to calculate the vaccination estimates
# for (s in 1:length(areas_with_data)){

# uncomment to troubleshoot
# s <- 1

# set the state to be analyzed
# area <- areas_with_data[s]

# print(paste(s, " Now prepping Area Statistical Code: ", area))

# subset to the state of interest
# NISTEEN_subset <- NISTEEN %>% filter(ESTIAPT20== area)

# Set options for allowing a single observation per stratum
# subset the files to those without missing data