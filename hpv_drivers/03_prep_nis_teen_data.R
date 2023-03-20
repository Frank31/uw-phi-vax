# 03_prep_nis_teen_data.R
# Francisco Rios Casas
# March 14 2023

# Purpose: Loop that preps only the necessary variables for analysis

# Note: Set the working directory at the root of this repository
source("./01_set_up_file.R")

# maybe load the file list
years_with_data    <- seq(2016,2021)

# load the raw dataset
load(file = paste0(data_folder, "raw_data/cdc_nis_teen/NISTEENPUF20.rdata"))

# re-write to form a loop eventully...
i <- 5

year <- years_with_data[i]

# prep the file 
data <- NISTEENPUF20 %>% 
  # subset variables of interest
  select(
    SEQNUMT, # unique teen identifier
    # geographic identifiers
    ESTIAPT20,
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
    P_N13MEN, # number of meningococcal serogroup acwy-containing shots
    P_N13FLU, # number of seasonal influenza vaccinations in past 3 years
    # weighting variables
    RDDWT_C, # final single-frame cell-phone rdd-phase weights (excludes territories)
    PROVWT_C, # use this weight fgor estimates based on teens with adequate provider data 
    STRATUM
    )

# factor any variables (?)

## race/ethnicty
RACEETHKlevels=c(1,2,3,4)
RACEETHKlabels=c("HISPANIC", "NON-HISPANIC WHITE ONLY", "NON-HISPANIC BLACK ONLY", "NON-HISPANIC OTHER + MULTIPLE RACE")

data$RACEETHK <- factor(data$RACEETHK, levels=RACEETHKlevels, labels=RACEETHKlabels)

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

data$STATE <- factor(data$STATE, levels=STATElevels, labels=STATElabels)

YNDKRFlevels=c(1,2,77,98,99)
YNDKRFlabels=c("YES", "NO", "DON'T KNOW", "MISSING IN ERROR", "REFUSED")

data$HPVI_ANY <- factor(data$HPVI_ANY, levels=YNDKRFlevels, labels=YNDKRFlabels)

# save prepped data
saveRDS(data, file = paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_", year, ".RDS"))
