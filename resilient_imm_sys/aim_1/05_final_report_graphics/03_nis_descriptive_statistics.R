# Francisco Rios Casas
# October 11 2022

# Calculate some key descriptive statistics for the report

# set up
source("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/05_final_report_graphics/01_set_up_R.r")

# load the data
data <- readRDS(paste0(prepped_data_dir, "02_estimated_vaccine_coverage.RDS"))

# calculate the range for each vaccine, year, and racial/ethic-income group
data %>% 
  filter(YEAR==2007, VACCINE=="DTP") %>%
  select(PredictedProb) %>%
  describe()

data %>% 
  filter(YEAR==2007, VACCINE=="MMR") %>%
  select(PredictedProb) %>%
  describe()

data %>% 
  filter(YEAR==2007, VACCINE=="HEP B") %>%
  select(PredictedProb) %>%
  describe()

data %>% 
  filter(YEAR==2019, VACCINE=="DTP") %>%
  select(PredictedProb) %>%
  describe()

data %>% 
  filter(YEAR==2019, VACCINE=="MMR") %>%
  select(PredictedProb) %>%
  describe()

data %>% 
  filter(YEAR==2019, VACCINE=="HEP B") %>%
  select(PredictedProb) %>%
  describe()