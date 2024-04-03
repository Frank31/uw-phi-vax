# Prep 2016-17 Ontario Vaccination Data
# Author: Frances Gellert
# Date: Sept 27 2023

# clear all
rm(list=ls())

# source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))

# load data
library(readxl)
ont_16 <- read_excel("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/province_level_vacc_rates/ontario/immunization-coverage-appendix-2016-17.xlsx", sheet = "A1.PHUCov7")

# CLEAN
# make row 1 column names and delete row 1, 38, and 39
new_column_names <- c("PHU", "mea", "mumps", "rubella", "dip", "tet", "polio", "pert", "hib", "pneum", "mcc", "var")
colnames(ont_16) <- new_column_names #set column names to dataframe
ont_16 <- ont_16[-c(1, 38, 39), ]
view(ont_16)

# MANIPULATE
# check variable classes
sapply(ont_16, class)

# make all rows except row 1 numerical 
char_columns <- sapply(ont_16, class) == "character" & names(ont_16) != "PHU"
ont_16[ , char_columns] <- sapply(ont_16[ , char_columns], as.numeric)
sapply(ont_16, class)
view(ont_16)

# SUMMARIZE
head(ont_16, n = 2)
summary(ont_16, na.rm = TRUE)

# exclude non-numeric columns and NA values
numeric_cols <- sapply(ont_16[-1, ], is.numeric)
summary(ont_16[-1, numeric_cols], na.rm = TRUE)

# calculate means of each vaccine and create "col_means" variable
col_means <- colMeans(ont_16[-1, numeric_cols], na.rm = TRUE)
col_means

ont_16 %>%
  summarise(mean_mea = mean(mea, na.rm = TRUE),
            mean_mumps = median(mumps, na.rm = TRUE))

# VISUALIZATION
# simple plotting
#dotchart(ont_16$mea, labels = ont_16$PHU, xlab = "Measles", ylab = "Public Health Unit")
#boxplot(mea ~ PHU, ont_16 = ont_16, xlab = "Public Health Unit", ylab = "Measles Coverage")
barplot(ont_16$mea, names.arg = ont_16$PHU, xlab = "Public Health Unit", ylab = "Measles Coverage") # this one is the best
barplot(ont_16$mumps, names.arg = ont_16$PHU, xlab = "Public Health Unit", ylab = "Mumps Coverage") 

# more complex bar chart
ggplot(ont_16, aes(x = PHU, y = mea)) +
  geom_bar(stat = "identity") +
  labs(title = "Vaccine Coverage by PHU", x = "Public Health Unit", y = "Measles Coverage")

# compare each PHU coverage to the mean
mean_coverage <- mean(ont_16$mea, na.rm = TRUE)
ont_16$mea_comparison <- ifelse(ont_16$mea > mean_coverage, "Above Mean", "Below Mean")
head(ont_16)
mean(mea)
view(ont_16)

# create measles comparison bar chart - above/below the mean
ggplot(ont_16, aes(x = PHU, y = mea, fill = mea_comparison)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Comparison of Public Health Unit Measles Vaccine Coverage",
    x = "Public Health Unit",
    y = "Measles Coverage Percentage",
    fill = "Comparison"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

