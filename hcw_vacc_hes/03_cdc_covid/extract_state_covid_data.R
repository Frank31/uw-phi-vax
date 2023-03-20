# 02_extract_covid_data.R
# Francisco Rios Casas
# March 15 2023

# Purpose: Loop that extracts all variables necessary for analysis

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# file path and name of the file
data_file <- paste0(data_folder, "raw_data/cdc_covid/covid19_vaccinations_in_the_united_states.csv")

# load the header names
headers = read.csv(data_file, skip = 5, header = F, nrows = 1, as.is = T)

# load the dataset and skip first few columns
df = read.csv(data_file, skip = 6, header = F)

# rename the headers of the data
colnames(df)= headers

# subset data
data_subset <- df %>% select(
  `Jurisdiction (State/Territory) or Federal Entity`, 
  `Percent of 18+ pop with a completed primary series`
  ) %>%
  # rename to easier names to work with
  rename(
    state = `Jurisdiction (State/Territory) or Federal Entity`
  ) %>%
  # convert structure to numeric
  mutate(percent_of_adults_vaccinated_with_covid = as.numeric(`Percent of 18+ pop with a completed primary series`)
         )

# calculate the quantiles
data_subset$first_q <- quantile(data_subset$percent_of_adults_vaccinated_with_covid, na.rm=TRUE)[2]
data_subset$third_q <- quantile(data_subset$percent_of_adults_vaccinated_with_covid, na.rm=TRUE)[4]
  
data_subset <- data_subset %>%
  mutate(category = case_when(
    percent_of_adults_vaccinated_with_covid >= first_q & percent_of_adults_vaccinated_with_covid <= third_q ~ "medium",
    percent_of_adults_vaccinated_with_covid < first_q ~ "low",
    percent_of_adults_vaccinated_with_covid > third_q ~ "high")) %>%
  filter(!is.na(percent_of_adults_vaccinated_with_covid))
  # filter out a few locations that are not US states
  
# create visualizations of the distribution of the data
figure1 <- ggplot(data_subset %>% filter(category=="low"),
       aes(x = reorder(state, -percent_of_adults_vaccinated_with_covid), y = percent_of_adults_vaccinated_with_covid)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  # geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  theme(text = element_text(size= 18)) +
  labs(
    title = "States with lowest coverage, as of March 15 2023"
  ) + 
  ylab("Percent") +
  xlab("State/Territory")

figure2 <- ggplot(data_subset %>% filter(category=="medium"),
                  aes(x = reorder(state, -percent_of_adults_vaccinated_with_covid), y = percent_of_adults_vaccinated_with_covid)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  # geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  theme(text = element_text(size= 18)) +
  labs(
    title = "States with average coverage, as of March 15 2023"
  ) + 
  ylab("Percent") +
  xlab("State/Territory")

figure3 <- ggplot(data_subset %>% filter(category=="high"),
                  aes(x = reorder(state, -percent_of_adults_vaccinated_with_covid), y = percent_of_adults_vaccinated_with_covid)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  # geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  theme(text = element_text(size= 18)) +
  labs(
    title = "States with highest coverage, as of March 15 2023"
  ) + 
  ylab("Percent") +
  xlab("State/Territory")

# export figures
pdf(file = paste0("C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/03_state_covid_trends.pdf"),
    height = 9, width = 12, pointsize = 20)
print(figure1)
print(figure2)
print(figure3)
dev.off()
