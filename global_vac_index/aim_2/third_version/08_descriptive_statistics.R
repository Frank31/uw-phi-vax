# Create some descriptive statistics for Final Report
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/third_version/01_set_up_R.R"))

# set up
library(tidyverse)

# load data
# data <- readRDS("C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General/Data/prepped_data/aim_2/19_index_results_third_version.RDS")
data <- readRDS("C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General/Data/prepped_data/aim_2/17_merged_dataset_third_version.RDS")

# create a table to store each of the results
table <- matrix(ncol = 2, nrow=13)
colnames(table) = c("variable", "geographies")
variables <- names(data)[8:20]

i = 1
for (i in 1:13){
  v = variables[i]
  table[i, 1] <- v
  tmpData <- data %>% filter(!is.na(get(v))) %>% select(location, year)
  table[i, 2] <- length(unique(tmpData$location))
}

# export table as csv
write.csv(table, file = paste0(visDir, "aim_2/third_version/12_geographies_represented.csv"))
          