# run_regression.R
# Francisco Rios Casas
# March 2, 2023

# Purpose: estimate flu shot coverage in each metro/micro area

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load data

# state flu estimates
flu <- readRDS(paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_all_years.RDS"))

# load the state fips codebook
fips <- read_xlsx(paste0(data_folder, "documentation/brfss/codebooks/state_fips_code.xlsx"))

# population density
pop_den <- readRDS(paste0(data_folder, "prepped_data/census_bureau/prepped_population_density.RDS"))

# gdp in each state
gdp <- readRDS(paste0(data_folder, "prepped_data/bea/gdp_data.RDS")) #why is South Carolina missing???

# subset and reshape and merge

# flu take only the most recent year
flu_subset <- flu %>% filter(factor=="factor(FLUSHOT7)1") %>% filter(YEAR==2020) %>%
  # create a STATE_FIPS_CODE
  mutate(STATE_FIPS_CODE=as.numeric(STATE)) %>%
  # merge onto the state names
  full_join(fips, by="STATE_FIPS_CODE") %>%
  # merge population density
  full_join(pop_den, by = c("STATE_NAME"="area")) %>%
  # merge the gdp in each state
  full_join(gdp, by = c("STATE_NAME"="location")) %>%
  # filter out the district of columbia
  filter(STATE!="11") %>%
  filter(STATE_FIPS_CODE!="72") %>%
  filter(STATE_FIPS_CODE!="78") %>%
  filter(STATE_FIPS_CODE!="66") 

# factor variables as numeric
flu_subset$gdp_2020 <- as.numeric(flu_subset$gdp_2020)
flu_subset$pop_density_2020 <- as.numeric(flu_subset$pop_density_2020)

# transform data ???
flu_subset <- flu_subset %>%
  mutate(pop_density_2020_trans = pop_density_2020^2) %>%
  mutate(gdp_2020_trans = gdp_2020^2)

# visualize distributions
hist(flu_subset$pop_density_2020_trans)
hist(flu_subset$gdp_2020_trans)

# run logistic regression 

mylogit <- glm(mean ~ pop_density_2020 + gdp_2020, data = flu_subset, family = "binomial")

summary(mylogit)


# predict coverage and compare to observed for each state in 2021