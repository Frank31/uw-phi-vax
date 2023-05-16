# 0X_visualize_local_level_trends.R
# Francisco Rios Casas
# March 2, 2023

# Purpose: Visualize local-level trends in vaccination coverage

# set the working directory at the root of this repository

# Source the set-up file
source("./01_set_up_file.R")

# load the State vaccination coverage estimates
data <- read_rds(file=paste0(data_folder, "prepped_data/cdc_brfss_smart/brfss_smart_estimated_coverage_all_years.rds"))

# load the codebook that includes the proper county names
mmsa_codebook <- read_rds(file=paste0(data_folder, "documentation/brfss_smart/mmsa_codebook.RDS"))

# create a summary figure of which metropolitan areas are represented
data <- data %>% full_join(mmsa_codebook, by = c("AREA"="_MMSA"))

data_frequency <- data %>% filter(YEAR%in%c(2016,2021)) %>% filter(factor=="factor(FLUSHOT7)1")

data_frequency <- data_frequency %>% count(YEAR, STATE_ABBREVIATION)

figure1 <- ggplot(data_frequency, aes(x = reorder(STATE_ABBREVIATION, n), y = n)) +
  coord_flip()+
  geom_bar(stat= "identity", fill = 'steelblue') +
  # geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(~YEAR) +
  theme_minimal() +
  labs(
    title = "Local areas with data available in each state"
  ) + 
  ylab("Number of local areas")+
  xlab("State/Territory") +
  scale_y_continuous(breaks = round(seq(min(data_frequency$n), max(data_frequency$n), by = 1),1))
  
  

# add some labels to the bars and a title to the graph and clean up the axes
# also make a similar graphic for the year 2016 and compare the two or pivot by year!

# plot trends between years
coverage <- filter(data %>% filter(factor== "factor(FLUSHOT7)1"))

# filter to only a few geographic areas in the survey because there are currently too many geographies
areas_of_interest <- c(10580, 10740, 10900) # choosing three at random for now

# visualize the distribution of data for all the years in the dataset
ggplot(coverage, aes(x=factor(YEAR), y=mean)) + 
  geom_boxplot()

# create trend lines for three geographic areas of interest
ggplot(coverage %>% filter(AREA %in% areas_of_interest), aes(x=YEAR, y=mean, color=factor(AREA))) + 
  geom_line() + 
  theme_minimal()

# categorize the percent change in vaccination in each metro region and compare to other values within a given state
# calculate the change between  for each year across all states #####

# subset years
trends <- coverage %>% filter(YEAR%in%c(2016, 2021)) %>%
  # pivot wider in order to calculate the difference between years
  pivot_wider(id_cols = c("factor","AREA"), 
              names_from = "YEAR",
              values_from = "mean") %>%
  # calculate a new variable called "change"
  mutate(difference = `2021`-`2016`) %>%
  # calculate the percent change 
  mutate(percent_change = (difference/`2016`)*100)

# calculate the median and mean change / percent change
summary(trends$percent_change)

# calculate the distribution in the difference
# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply(trends[,5:6] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = c("difference", "percent_change"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value")

# re-shape the dataset
categorized_areas <- trends %>%
  select(AREA, `2016`, `2021`, difference, percent_change) %>%
  pivot_longer(
    !c(AREA),
    names_to  = "variable",
    values_to = "values",
    values_drop_na = TRUE) %>%
  filter(variable%in%c("difference", "percent_change"))

# merge the cutoff scores with the dataset
categorized_areas <- full_join(categorized_areas, quantiles, by=c("variable"="group"))

# calculate what group each state falls in
categorized_areas <- categorized_areas %>% 
  mutate(category = case_when(values >= first_q & values <= third_q ~ "medium",
                              values < first_q ~ "low",
                              values > third_q ~ "high"))

# create label for values
categorized_areas$label_val <- round(categorized_areas$values, 0)

ggplot(categorized_areas %>% filter(variable=="percent_change" & category=="low"),
       aes(x = AREA, y = values)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  geom_text(position = position_dodge(width = 1), aes(label=label_val), vjust = 0.5, hjust = -0.5) +
  theme_minimal()

# export images created in this script
pdf(file = paste0("C:/Users/frc2/UW/og_merck_healthcare_vaccine_hesitancy - Documents/Quantitative/results/02_local_trends.pdf"),
    height = 9, width = 12)
print(figure1)
dev.off()
