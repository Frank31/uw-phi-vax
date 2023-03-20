# 03_get_coverage_estimates.R
# Francisco Rios Casas
# March 15 2023

# Purpose: estimate HPV coverage in each state

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

years_with_data    <- seq(2016,2021)

# loop through each dataset to extract the 
for (i in 1:length(years_with_data)){z
  # un comment to trouble shoot
  i <- 5
  
  # set the initial parameters of data to be prepped
  year      <- years_with_data[i]
  file_name <- paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_",year,".rds")
  
  print(paste0(i, " Now prepping ", year, " NIS Teen Data"))
  
  # load the data
  NISTEEN <- read_rds(file_name)
  
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
  NISTEEN <- subset(NISTEEN, HPVI_ANY %in% c("YES", "NO"))
  
  # drop states that are missing (territories)
  NISTEEN <- NISTEEN %>% filter(!is.na(STATE))
  
  # Specify a sampling design
  # Create survey design
  nisteendsgn <- svydesign(
    id=~SEQNUMT,
    strata = ~STRATUM,
    weights = ~RDDWT_C,
    data = NISTEEN)
  
  # HPV Any doses = YES ESTIMATES BY STATE
  r_est3 <- svyby(~HPVI_ANY, ~STATE, nisteendsgn, svymean)
  
  # NISTEEN <- NISTEEN %>% filter(HPVI_ANY)
  #   options(survey.lonely.psu = "adjust")
  #   
  #  
  #   
  #   # store the flu vaccine coverage within an object
  #   estimates <- svymean(~factor(FLUSHOT7),
  #                        brfssdsgn,
  #                        na.rm = TRUE)
    
    # produce estimates of teen being vaccinated for hp in each state
    
    
    
    
    
  # }
  # save the individual datasets
  # format and factor the character types
  

  
  # subset to columns of interest
  
  # append the different years together using rbind
  
  
  
  
  
}


# save the prepped data