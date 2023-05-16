# 0X_visualize_state_level_trends.R
# Francisco Rios Casas
# March 2, 2023

# Purpose: Visualize state-level trends in vaccination coverage

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load the State vaccination coverage estimates
data <- read_rds(file=paste0(data_folder, "prepped_data/cdc_brfss/brfss_estimated_coverage_all_years.rds"))

# load the state FIPS values to label data properly
fips_codebook <- read_excel(path = paste0(data_folder, "documentation/brfss/codebooks/state_fips_code.xlsx"))

# add STATE_FIPS_CODE variable to use in visuals
data$STATE_FIPS_CODE <- as.numeric(data$STATE)

# add STATE_NAME label to use in visuals
data <- data %>% full_join(fips_codebook, by = "STATE_FIPS_CODE")

# plot trends between years
coverage <- filter(data %>% filter(factor== "factor(FLUSHOT7)1")) %>% 
  filter(!STATE_NAME %in% c("District of Columbia",
                            "Guam",
                            "Virgin Islands",
                            "Puerto Rico"))

# create state level categories
figure1 <- ggplot(coverage, aes(x=factor(YEAR), y=mean)) + 
  geom_boxplot() + 
  labs(
    title = "Distribution of flu vaccination rates across all US States"
  ) +
  xlab("year")+
  ylab("percent")+
  theme_minimal()

# create line trend lines in each state
figure2 <- ggplot(coverage, aes(x=YEAR, y=mean, color=factor(STATE_NAME))) + 
  geom_line() + 
  labs(
    title = "Overall trends across all US States"
  ) +
  xlab("year")+
  ylab("percent")+
  theme_minimal()

# calculate the change between  for each year across all states #####

# subset years
trends <- coverage %>% filter(YEAR%in%c(2016, 2021)) %>%
  # pivot wider in order to calculate the difference between years
  pivot_wider(id_cols = c("factor","STATE_NAME"), 
              names_from = "YEAR",
              values_from = "mean") %>%
  # calculate a new variable called "change"
  mutate(change = `2021`-`2016`) %>%
  # calculate the percent change 
  mutate(percent_change = (change/`2016`)*100)

# calculate the median and mean change / percent change
summary(trends$percent_change)

# calculate the distribution in the change
# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply(trends[,5:6] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = c("change", "percent_change"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )

# re-shape the dataset
categorized_states <- trends %>%
  select(STATE_NAME, `2016`, `2021`, change, percent_change) %>%
  pivot_longer(
    !c(STATE_NAME),
    names_to  = "variable",
    values_to = "values",
    values_drop_na = TRUE) %>%
  filter(variable%in%c("change", "percent_change"))

# merge the cutoff scores with the dataset
categorized_states <- full_join(categorized_states, quantiles, by=c("variable"="group"))

# calculate what group each STATE_NAME falls in
categorized_states <- categorized_states %>% 
  mutate(category = case_when(values >= first_q & values <= third_q ~ "medium",
                              values < first_q ~ "low",
                              values > third_q ~ "high"))

# create a raster plot to visualize how each state is doing

# create label for values
categorized_states$label_val <- round(categorized_states$values, 0)

# create plot to visualize how states compare to each other
##### -----
# IDEA: re-write this using a loop?
# IDEA: Sort in descending order
##### -----
figure3 <- ggplot(categorized_states %>% filter(variable=="percent_change" & category=="low"),
       aes(x = reorder(STATE_NAME, -values), y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with lowest improvement, 2016 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory")

figure4 <- ggplot(categorized_states %>% filter(variable=="percent_change" & category=="medium"),
       aes(x = reorder(STATE_NAME, -values), y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with average improvement, 2016 - 2021"
  ) + 
  ylab("Percent change")+
  xlab("State/Territory")

figure5 <- ggplot(categorized_states %>% filter(variable=="percent_change" & category=="high"),
       aes(x = reorder(STATE_NAME, -values), y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal()  +
  labs(
    title = "States with highest improvement, 2016 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory")

# calculate if differences are statistically significant

# visualize trends among select states
selected_states <- coverage %>% filter(STATE_NAME %in% c("Vermont", "Massachusetts", "District of Columbia",
                                                                   "Iowa", "New Mexico", "Minnesota", "Ohio", "Colorado", "Nebraska", 
                                                                   "Virginia", "Pennsylvania", "Nevada", "Delaware", 
                                                                   "West Virginia", "Mississippi", "Missouri", "Georgia", "Wyoming"))

# add in the category of the states
selected_states <- selected_states %>% mutate(category = case_when(
  STATE_NAME == "Vermont" ~ "high performer",
  STATE_NAME == "Massachusetts" ~ "high performer",
  STATE_NAME == "District of Columbia" ~ "high performer",
  STATE_NAME == "West Virginia" ~ "low performer",
  STATE_NAME == "Mississippi" ~ "low performer",
  STATE_NAME == "Missouri" ~ "low performer",
  STATE_NAME == "Georgia" ~ "low performer",
  STATE_NAME == "Wyoming" ~ "low performer",
  TRUE ~ "average performer"
  ))

figure6 <- ggplot(selected_states %>% filter(category=="high performer"), aes(x=YEAR, y=mean, color=factor(STATE_NAME))) + 
  geom_line() + 
  labs(
    title = "Flu vaccine trends"
  ) +
  xlab("year")+
  ylab("percent")+
  ylim(0.25, 0.60) +
  theme_minimal() +
  labs(color="High-performing states") + 
  theme(text = element_text(size= 18))

figure7 <- ggplot(selected_states %>% filter(category=="average performer"), aes(x=YEAR, y=mean, color=factor(STATE_NAME))) + 
  geom_line() + 
  labs(
    title = "Flu vaccine trends"
  ) +
  xlab("year")+
  ylab("percent")+
  ylim(0.25, 0.60) +
  theme_minimal() +
  labs(color="Average-performing states") + 
  theme(text = element_text(size= 18))

figure8 <- ggplot(selected_states %>% filter(category=="low performer"), aes(x=YEAR, y=mean, color=factor(STATE_NAME))) + 
  geom_line() + 
  labs(
    title = "Flu vaccine trends"
  ) +
  xlab("year")+
  ylab("percent")+
  ylim(0.25, 0.60) +
  theme_minimal() +
  labs(color="Low-performing states") + 
  theme(text = element_text(size= 18))

# re-create figures 3-4 with outliers removed

# create a function to spot outliers in data
# # drop outliers from each state
# # create function that deletes outliers
is_outlier <- function(x){
  lower_bound <- quantile(x, 0.05)
  upper_bound <- quantile(x, 0.95)
  ifelse((x < lower_bound | x > upper_bound), NA, x)
}

# outlier detection version
no_outlier <- as.data.table(categorized_states %>% 
                              filter(variable=="percent_change") %>%
                              filter(!STATE_NAME %in% c("District of Columbia",
                                                      "Guam",
                                                      "Virgin Islands",
                                                      "Puerto Rico")))

no_outlier[, ("values"):=is_outlier(get("values"))]

no_outlier <- no_outlier %>% filter(!is.na(values))

# figure 9
figure9 <- ggplot(no_outlier %>% filter(category=="low"),
                  aes(x = reorder(STATE_NAME, -values), y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with lowest improvement (no outliers), 2016 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory")

# figure 10
figure10 <- ggplot(no_outlier %>% filter(category=="high"),
                   aes(x = reorder(STATE_NAME, -values), y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with average improvement (no outliers), 2016 - 2021"
  ) + 
  ylab("Percent change")+
  xlab("State/Territory")

# export figures
pdf(file = paste0("C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/01_state_trends.pdf"),
    height = 9, width = 12)
print(figure1)
print(figure2)
print(figure3)
print(figure4)
print(figure5)
print(figure6)
print(figure7)
print(figure8)
print(figure9)
print(figure10)
dev.off()