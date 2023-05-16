# 06_aim2_visualize_trends.R
# Francisco Rios Casas
# April 12 2023

# Purpose: estimate HPV coverage in each state and categorize states based on 
# their performance

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load the prepped data on state vaccination rates
data <- readRDS(file = paste0(data_folder, "prepped_data/cdc_nis_teen/merged_nis_estimates_2012-2021.RDS"))

# create 3 following graphics:
# 1. boxplot showing distribution of values over the time period
# 2. line graphs visualizing all the trends over the time period
# 3-5. barplots showing how each state compares to the other

# 1. Create boxplots showing distribution of vaccination coverage over the time period
figure1 <-  ggplot(data, aes(x=factor(year), y=HPVI_ANYYES)) + 
  geom_boxplot() + 
  labs(
    title = "Distribution of HPV vaccination initiation rates across all US States"
  ) +
  xlab("year")+
  ylab("percent")+
  theme_minimal()  + 
  theme(text = element_text(size= 18))

# 2. Create line graphs visualizing all the trends over the time period
figure2 <-  ggplot(data, aes(x=year, y=HPVI_ANYYES, color=factor(STATE))) + 
  geom_line() + 
  labs(
    title = "Overall trends in HPV initiation across all US States"
  ) +
  xlab("year")+
  ylab("percent")+
  theme_minimal() + 
  theme(text = element_text(size= 18))



# 3. create barplots showing how each state compares to the other
# create two groupings: those above and below the average

#  calculate the difference between earliest and latest years
trends <- data %>% filter(year%in%c(2012, 2021)) %>%
  # pivot wider in order to calculate the diffeence between years
  pivot_wider(id_cols = c("STATE"),
              names_from = "year",
              values_from = "HPVI_ANYYES") %>%
  # calculate new variable called "change"
  mutate(change = `2021` - `2012`) %>%
  #calculate the percent change
  mutate(percent_change = (change/`2012`)*100)

# calculate the median and mean change / percent change
summary(trends$percent_change)

# calculate the distribution in the change
median <- summary(trends$percent_change)[3]

# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply(trends[,4:5] , 2 , quantile , probs = quants , na.rm = TRUE))
first_q <- unlist(quantiles[1,2])
third_q <- unlist(quantiles[2,2])

# create three groupings
trends <- trends %>% mutate(category = case_when(
  percent_change > first_q & percent_change < third_q ~ "average_improvement",
  percent_change <= first_q ~ "low_improvement",
  percent_change >= third_q ~ "high_improvement"
))

# 
# quantiles$quant <- NA
# quantiles$quant[1] <- "first_q"
# quantiles$quant[2] <- "third_q"

# quantiles <- pivot_longer(quantiles, cols = c("change", "percent_change"), names_to = c("group"))
# quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )

# re-shape the dataset
# categorized_states <- trends %>%
#   select(STATE, `2012`, `2021`, percent_change) %>%
#   pivot_longer(
#     !c(STATE),
#     names_to  = "variable",
#     values_to = "values",
#     values_drop_na = TRUE) %>%
#   filter(variable%in%c("change", "percent_change"))

# merge the cutoff scores with the dataset
# categorized_states <- full_join(categorized_states, quantiles, by=c("variable"="group"))

# calculate what group each STATE_NAME falls in
# categorized_states <- categorized_states %>% 
#   mutate(category = case_when(values >= first_q & values <= third_q ~ "medium",
#                               values < first_q ~ "low",
#                               values > third_q ~ "high"))

# drop outliers from high and low performers
# create function that deletes outliers
is_outlier <- function(x){
  lower_bound <- quantile(x, 0.05)
  upper_bound <- quantile(x, 0.95)
  ifelse((x < lower_bound | x > upper_bound), NA, x)
}

# # outlier detection version
no_outlier <- as.data.table(trends)

no_outlier[, ("percent_change"):=is_outlier(get("percent_change"))]

# drop outlier values
no_outlier <- no_outlier %>% filter(!is.na(percent_change))

# create label for values
no_outlier$label_val <- round(no_outlier$percent_change, 0)

figure3 <- ggplot(no_outlier %>% filter(category=="low_improvement"),
       aes(x = reorder(STATE, -percent_change), y = percent_change)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with lowest improvement, 2012 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory") + 
  theme(text = element_text(size= 18))

figure4 <- ggplot(no_outlier %>% filter(category=="average_improvement"),
       aes(x = reorder(STATE, -percent_change), y = percent_change)) +
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal() +
  labs(
    title = "States with average improvement, 2012 - 2021"
  ) +
  ylab("Percent change") +
  xlab("State/Territory") +
  theme(text = element_text(size= 18))

figure5 <- ggplot(no_outlier %>% filter(category=="high_improvement"),
       aes(x = reorder(STATE, -percent_change), y = percent_change)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal()  +
  labs(
    title = "States with highest improvement, 2012 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory") + 
  theme(text = element_text(size= 18))

# create figure to visualize trends in only a few select states from each category
figure6 <- ggplot(data %>% filter(STATE %in% c("CALIFORNIA", "TEXAS", "FLORIDA", "NEW YORK", "PENNSYLVANIA", "ILLINOIS", "OHIO", "GEORGIA", "NORTH CAROLINA", "MICHIGAN", "NEW JERSEY", "VIRGINIA")) , aes(x=year, y=HPVI_ANYYES, color=factor(STATE))) + 
  geom_line() + 
  labs(
    title = "Overall trends in HPV initiation across select US States"
  ) +
  xlab("year")+
  ylab("percent")+
  theme_minimal() + 
  theme(text = element_text(size= 18))

# plot according to state category
no_outlier$category <- factor(no_outlier$category, levels = c("low_improvement", "average_improvement", "high_improvement"))
figure7 <- ggplot(no_outlier %>% filter(STATE %in% c("CALIFORNIA", "TEXAS", "FLORIDA", "NEW YORK", "PENNSYLVANIA", "ILLINOIS", "OHIO", "GEORGIA", "NORTH CAROLINA", "MICHIGAN", "NEW JERSEY", "VIRGINIA")),
                  aes(x = reorder(STATE, -percent_change), y = percent_change)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal()  +
  labs(
    title = "States categorized according to improvement, 2012 - 2021"
  ) + 
  ylab("Percent change") +
  xlab("State/Territory") + 
  facet_wrap(~category) +
  theme(text = element_text(size= 18))

# export figures
pdf(file = paste0("C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/07_state_hpv_trends.pdf"),
    height = 9, width = 12)
print(figure1)
print(figure2)
print(figure3)
print(figure4)
print(figure5)
print(figure6)
print(figure7)
dev.off()

# review county level data--maybe Texas and another high-performing state
