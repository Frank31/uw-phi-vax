# 03_prep_nis_teen_data.R
# Francisco Rios Casas
# March 14 2023

# Purpose: Loop that preps only the necessary variables for analysis

# Note: Set the working directory at the root of this repository
source("./01_set_up_file.R")

# maybe load the file list
years_with_data <- c(2021, 2020, 2019, 2018, 2017, 2016, 
                     2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008)

for (i in 1:length(years_with_data)){
  
  # uncomment for troubleshooting
  # i <- 10
  
  # establish the parameters
  year <- years_with_data[i]
  file_name <- file_list$file_name[i]
  
  # print update statement
  print(paste0("Now prepping file ", i, ": ", file_name))
  
  # define a function that will load the data into a new object
  loadRData <- function(fileName){
    # loads an RDATA file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  # save the extracted data into a new file
  data <- loadRData(paste0(data_folder, "raw_data/cdc_nis_teen/", file_name, ".rdata"))
  
  # subset the columns of interest according to year
  # as some variables have different requirements
  if (year == 2018) {
    data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        EST_GRANT,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT_C, # final single-frame cell-phone rdd-phase weights (excludes territories)
        PROVWT_C, # use this weight forr estimates based on teens with adequate provider data 
        STRATUM
      )
  } else if (year %in% c(2015, 2016, 2017)) {
    data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        EST_GRANT,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT_D, # final single-frame cell-phone rdd-phase weights (excludes territories)
        PROVWT_D, # use this weight forr estimates based on teens with adequate provider data 
        STRATUM
      )
  } else if (year %in% c(2014)){
    data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        EST_GRANT,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY_REC,
        # one hpv vaccine was dropped from these analysis
        # HPVI_ANY_SC, # hpv vaccination 
        
        HPVI_NUM_REC, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT_D, # final single-frame cell-phone rdd-phase weights (excludes territories)
        PROVWT_D, # use this weight forr estimates based on teens with adequate provider data 
        STRATUM
      )
    
    } else if (year %in% c(2012, 2013)) {
    data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        EST_GRANT,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY_SC, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT_D, # final single-frame cell-phone rdd-phase weights (excludes territories)
        PROVWT_D, # use this weight forr estimates based on teens with adequate provider data 
        STRATUM
      )
    } else if (year %in% c(2011)){
      data_subset <- data %>%
        # subset variables of interest
        select(
          SEQNUMT, # unique teen identifier
          # geographic identifiers
          STATE, 
          RACEETHK, # race/ethnicity
          SEX,      # sex of teen
          LANGUAGE, # language of interview
          INCPOV1,  # poverty status
          INCQ298A, # family income
          RENT_OWN, # family living arrangements
          EDUC1,    # education level of mother
          HPVI_ANY_SC, # hpv vaccination
          HPVI_NUM_TOT, # number of hpv vaccines received
          P_N13MEN, # number of meningococcal serogroup acwy-containing shots
          # number of seasonal influenza vaccinations in past 3 years is not available in 2018
          # P_N13FLU,
          # weighting variables
          RDDWT_D, # final single-frame cell-phone rdd-phase weights (excludes territories)
          STRATUM_D)
      } else if (year %in% c(2010)){
        data_subset <- data %>%
          # subset variables of interest
          select(
            SEQNUMT, # unique teen identifier
            # geographic identifiers
            ESTIAPT10,
            STATE, 
            RACEETHK, # race/ethnicity
            SEX,      # sex of teen
            LANGUAGE, # language of interview
            INCPOV1,  # poverty status
            INCQ298A, # family income
            RENT_OWN, # family living arrangements
            EDUC1,    # education level of mother
            HPVI_ANY_SC, # hpv vaccination
            HPVI_NUM_TOT, # number of hpv vaccines received
            P_N13MEN, # number of meningococcal serogroup acwy-containing shots
            # number of seasonal influenza vaccinations in past 3 years is not available in 2018
            # P_N13FLU,
            # weighting variables
            RDDWT # final single-frame cell-phone rdd-phase weights (excludes territories)
          )
      } else if (year %in% c(2009)){
        data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        ESTIAPT09,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY_SC, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT # final single-frame cell-phone rdd-phase weights (excludes territories)
        )
  } else if (year %in% c(2008)) {
    data_subset <- data %>%
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        ESTIAPT08,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        # RENT_OWN not collected in 2008
        # RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY_SC, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received --asked only of females
        HPVI_KNOW, # asks if they have heard of the HPV Vaccine
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        # number of seasonal influenza vaccinations in past 3 years is not available in 2018
        # P_N13FLU,
        # weighting variables
        RDDWT # final single-frame cell-phone rdd-phase weights (excludes territories)
      )
    } else {
    data_subset <- data %>% 
      # subset variables of interest
      select(
        SEQNUMT, # unique teen identifier
        # geographic identifiers
        EST_GRANT,
        STATE, 
        RACEETHK, # race/ethnicity
        SEX,      # sex of teen
        LANGUAGE, # language of interview
        INCPOV1,  # poverty status
        INCQ298A, # family income
        RENT_OWN, # family living arrangements
        EDUC1,    # education level of mother
        HPVI_ANY, # hpv vaccination
        HPVI_NUM_TOT, # number of hpv vaccines received
        P_N13MEN, # number of meningococcal serogroup acwy-containing shots
        P_N13FLU, # number of seasonal influenza vaccinations in past 3 years
        # weighting variables
        RDDWT_C, # final single-frame cell-phone rdd-phase weights (excludes territories)
        PROVWT_C, # use this weight for estimates based on teens with adequate provider data 
        STRATUM
      )
  }
  

  # factor certain variables
  # including race/ethnicity, sex of teen, language in which interview, 
  # poverty status, family income, family living arrangements, 
  # education level of mother
  
  ## race/ethnicty
  RACEETHKlevels=c(1,2,3,4)
  RACEETHKlabels=c("HISPANIC", "NON-HISPANIC WHITE ONLY", "NON-HISPANIC BLACK ONLY", "NON-HISPANIC OTHER + MULTIPLE RACE")
  
  ## states of interest
  STATElevels=c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17,
                18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53,
                54, 55, 56)
  STATElabels=c(
    "ALABAMA",
    "ALASKA",
    "ARIZONA",
    "ARKANSAS",
    "CALIFORNIA",
    "COLORADO",
    "CONNECTICUT",
    "DELAWARE",
    "DISTRICT OF COLUMBIA",
    "FLORIDA",
    "GEORGIA",
    "HAWAII",
    "IDAHO",
    "ILLINOIS",
    "INDIANA",
    "IOWA",
    "KANSAS",
    "KENTUCKY",
    "LOUISIANA",
    "MAINE",
    "MARYLAND",
    "MASSACHUSETTS",
    "MICHIGAN",
    "MINNESOTA",
    "MISSISSIPPI",
    "MISSOURI",
    "MONTANA",
    "NEBRASKA",
    "NEVADA",
    "NEW HAMPSHIRE",
    "NEW JERSEY",
    "NEW MEXICO",
    "NEW YORK",
    "NORTH CAROLINA",
    "NORTH DAKOTA",
    "OHIO",
    "OKLAHOMA",
    "OREGON",
    "PENNSYLVANIA",
    "RHODE ISLAND",
    "SOUTH CAROLINA",
    "SOUTH DAKOTA",
    "TENNESSEE",
    "TEXAS",
    "UTAH",
    "VERMONT", 
    "VIRGINIA",
    "WASHINGTON",
    "WEST VIRGINIA",
    "WISCONSIN",
    "WYOMING"
  )
  
  # sex of teen
  SEXlevels=c(1,2,77,98,99)
  SEXlabels=c("MALE", "FEMALE", "DON'T KNOW", "MISSING IN ERROR", "REFUSED")
  
  # yes no don't know factors
  YNDKRFlevels=c(1,2,77,98,99)
  YNDKRFlabels=c("YES", "NO", "DON'T KNOW", "MISSING IN ERROR", "REFUSED")
  
  # language in which interview was conducted
  LANGUAGElevels=c(1,2,3)
  LANGUAGElabels=c("ENGLISH", "SPANISH", "OTHER")
  
  # poverty levels
  INCPOVlevels=c(1,2,3,4)
  INCPOVlabels=c("ABOVE POVERTY > $75K", "ABOVE POVERTY <= $75K", "BELOW POVERTY", "UNKNOWN")
  
  # family income
  INCQ298Alevels=c(10,11,12,13,14,3,4,5,6,7,77,8,9,99)
  INCQ298Alabels=c("$35001 - $40000", "$40001 - $50000", "$50001 - $60000", "$60001 - $75000", "$75001+", "$0 - $7500", "$7501 - $10000", "$10001 - $17500", "$17501 - $20000", "$20001 - $25000", "DON'T KNOW", "$25001 - $30000", "$30001 - $35000", "REFUSED")
  
  # family living arrangements
  RENTOWNlevels=c(1,2,3,77,99)
  RENTOWNlabels=c("OWNED OR BEING BOUGHT", "RENTED", "OTHER ARRANGMENT", "DON'T KNOW", "REFUSED")
  
  # maternal education
  EDUC4_Mlevels=c(1,2,3,4,77,98,99)
  EDUC4_Mlabels=c("LESS THAN 12 YEARS", "12 YEARS", "MORE THAN 12 YEARS, NON-COLLEGE GRAD", "COLLEGE GRADUATE", "DON'T KNOW", "MISSING IN ERROR", "REFUSED")
  
  # number of shots received
  HHCOUNTlevels=c(50,77,98,99)
  HHCOUNTlabels=c("ALL SHOTS", "DON'T KNOW", "MISSING IN ERROR", "REFUSED")
  
  # Estimation areas
  ESTIAPT08Flevels=c(1,10,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8)
  ESTIAPT08Flabels=c("CT", "NY-REST OF STATE", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI",
                     "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ")
  
  ESTIAPT09Flevels=c(1,10,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,37,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,69,7,72,73,74,75,76,77,8,95,96)
  ESTIAPT09Flabels=c("CT", "NY-REST OF STATE", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO",
                     "IN-REST OF STATE", "IN-MARION COUNTY", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD",
                     "UT", "WY", "AZ", "CA-REST OF STATE", "CA-LOS ANGELES COUNTY", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS", "IN-LAKE COUNTY")
 
  ESTIAPT10Flevels=c(1,10,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
  ESTIAPT10Flabels=c("CT", "NY-REST OF STATE", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI",
                     "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV",
                     "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
  
  # apply the labels for hpv vaccination
  if (year==2014){
    data_subset$HPVI_ANY_REC <- factor(data_subset$HPVI_ANY_REC, levels=YNDKRFlevels, labels=YNDKRFlabels)

  }
  # apply the labels and levels for estimation areas
  if (year==2010){
    data_subset$ESTIAPT10 <- factor(data_subset$ESTIAPT10, levels=ESTIAPT10Flevels, labels=ESTIAPT10Flabels)
  } else if (year==2009){
    data_subset$ESTIAPT09 <- factor(data_subset$ESTIAPT09, levels=ESTIAPT10Flevels, labels=ESTIAPT10Flabels)
  } else if (year==2008){
    data_subset$ESTIAPT08 <- factor(data_subset$ESTIAPT08, levels=ESTIAPT08Flevels, labels=ESTIAPT08Flabels)
    # early in the survey questions around HPV were asked differently
    data_subset$HPVI_KNOW <- factor(data_subset$HPVI_KNOW, levels=YNDKRFlevels, labels=YNDKRFlabels)
  }
  
  
  # apply the factor levels and labels
  data_subset$RACEETHK <- factor(data_subset$RACEETHK, levels=RACEETHKlevels, labels=RACEETHKlabels)
  data_subset$STATE <- factor(data_subset$STATE, levels=STATElevels, labels=STATElabels)
  data_subset$SEX <- factor(data_subset$SEX, levels=SEXlevels, labels=SEXlabels)
  if (year==2014){
    data_subset$HPVI_NUM_REC <- factor(data_subset$HPVI_NUM_REC, levels=YNDKRFlevels, labels=YNDKRFlabels)
  } else {
    data_subset$HPVI_ANY <- factor(data_subset$HPVI_ANY, levels=YNDKRFlevels, labels=YNDKRFlabels)
  }
  # for 2014 include HPVI_NUM_REC
  
  data_subset$LANGUAGE <- factor(data_subset$LANGUAGE, levels=LANGUAGElevels, labels=LANGUAGElabels)
  data_subset$INCPOV1 <- factor(data_subset$INCPOV1, levels=INCPOVlevels, labels=INCPOVlabels)
  data_subset$INCQ298A <- factor(data_subset$INCQ298A, levels=INCQ298Alevels, labels=INCQ298Alabels)
  if (year != 2008){
    data_subset$RENT_OWN <- factor(data_subset$RENT_OWN, levels=RENTOWNlevels, labels=RENTOWNlabels)
  }
  data_subset$EDUC1 <- factor(data_subset$EDUC1, levels=EDUC4_Mlevels, labels=EDUC4_Mlabels)
  
  # save individual prepped data
  saveRDS(data_subset, file = paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_", year, ".RDS"))
  
  # print update statement
  print(paste0(i, " Now saved: prepped ", file_name, " data"))
}
