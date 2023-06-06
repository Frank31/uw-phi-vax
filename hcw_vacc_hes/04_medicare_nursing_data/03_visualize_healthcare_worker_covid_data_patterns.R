# 04_visualize_healthcare_worker_covid_data_patterns.R
# Francisco Rios Casas
# March 17, 2023

# Purpose: visualize the patterns of data availability and trends in states 
# among healthcare workers in nursing facilities

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load the merged dataset
data <- readRDS(paste0(data_folder, "prepped_data/cms_healthcare_worker/_merged_nursing_home_data2023-02-26.RDS")) %>%
# filter out data that is missing since most likely indicates that the facility has closed down
  filter(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time!=0) %>%
  drop_na(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time) %>%
  filter(percentage_of_current_healthcare_personnel_who_received_a_completed_covid_19_vaccination_at_any_time!="")

# what kind of visuals do we want? 

# the number of unique healthcare facilities in each state
data_frequency <- data %>% count(provider_state)

# load excel file with list of states to extract
state_list <- read_xlsx(paste0(data_folder, "documentation/cms_healthcare_worker/potential_states_of_interest.xlsx"))

# merge state name
data_frequency <- data_frequency %>% full_join(state_list, by=c("provider_state"="state"))
  

# create a bar plot of the frequency of unique providers in each state
figure1 <- ggplot(data_frequency, aes(x = reorder(state_name, n), y = n)) +
  geom_bar(stat= "identity", fill = 'steelblue') +
  coord_flip() +
  theme_minimal() +
  geom_text(position = position_dodge(width = 1), aes(label=n), vjust = 0.5, hjust = -0.5) +
  labs(
    title = "Number of healthcare facilities reporting data, week ending Feb 26 2023"
  ) + 
  ylab("Sample size") +
  xlab("State/Territory") + 
  theme(text = element_text(size= 18))

# export figures
pdf(file = paste0("C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/04_state_nursing_home_data_availability.pdf"),
    height = 9, width = 12, pointsize = 20)
print(figure1)
dev.off()
