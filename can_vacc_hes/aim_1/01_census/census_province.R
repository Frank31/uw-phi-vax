## Analyze 2021 Province Census Data
# Author: Frances Gellert
# Date: Sept 20 2023

# clear all
rm(list=ls())

# source set up script
source(paste0("C:/Users/fgellert/OneDrive - UW/Documents/uw-phi-vax/can_vacc_hes/aim_1/01_province_level_analyses/01_set_up_R.R"))
 
# load data 
df <- read_csv(file = paste0("C:/Users/fgellert/UW/og_merck_canada_vaccine_hesitancy - Documents/Aim 1/Data/raw_data/2021_census/census_province.csv"))
view(df)

#CLEAN/FORMAT ALL ----
#rename columns and remove rows 1-3
new_column_names <- c("topic", "characteristic", "can_count", "can_rate", "bc_count", "bc_rate", "on_count", "on_rate", "qb_count", "qb_rate")
colnames(df) <- new_column_names #set column names to dataframe
df <- df[-c(1:3), ] #remove rows 1-3
view(df)

# make all rows except row 1 & 2 numerical 
char_columns <- sapply(df, class) == "character" & names(df) != "topic" & names(df) !="characteristic"
df[ , char_columns] <- sapply(df[ , char_columns], as.numeric)
sapply(df, class)


##RACE ----
#select race data
race_df <- filter(df, topic=="Visible minority")
race_df <- select(race_df, !"topic")
view(race_df)

#MANIPULATE
#pivot_longer
long_race <- race_df %>% pivot_longer(cols = contains("count"))

long_race <- race_df %>%
  pivot_longer(cols = contains("rate"), names_to = "province") %>%
  mutate(province = gsub("_rate", "", province)) %>%
  select(-contains("count")) %>%  #remove columns containing "count"
  select(province, everything()) %>%  #move province column to first column
  filter(!grepl("total", characteristic, ignore.case = TRUE)) %>%  #remove rows containing "total"
  rename(race = characteristic)   #rename "characteristic" row to "race"

#RENAME VALUES
#rename values in race column
long_race <- long_race %>%
  mutate(
    race = case_when(
      grepl("n\\.i\\.e\\.", race, ignore.case = TRUE) ~ "Other",
      grepl("Multiple", race, ignore.case = TRUE) ~ "Mixed",
      grepl("Not", race, ignore.case = TRUE) ~ "White",
      grepl("West Asian", race, ignore.case = TRUE) | grepl("Arab", race, ignore.case = TRUE) ~ "West Asian",
      grepl("Southeast Asian", race, ignore.case = TRUE) | grepl("Filipino", race, ignore.case = TRUE) ~ "Southeast Asian",
      grepl("Chinese", race, ignore.case = TRUE) | grepl("Japanese", race, ignore.case = TRUE) | grepl("Korean", race, ignore.case = TRUE) ~ "East Asian",
      TRUE ~ race
    )
  )
view(long_race)

#rename values in province column
long_race <- long_race %>% 
  mutate(province = recode(province, "can" = "Canada", "bc" = "B.C.", "on" = "Ontario", "qb" = "Quebec"))

#VISUALIZE RACE
# create a bar chart of percent race by province 
long_race$province <- factor(long_race$province, levels = c("Canada", "B.C.", "Ontario", "Quebec"))  #set the order of provinces

ggplot(long_race, aes(x = province, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percent Racial Group Comparison by Province",
    x = "Province",
    y = "Percent"
  ) +
  theme_minimal()

# create a bar chart of percent race by province (without White group!)
long_race_nw <- long_race %>%
  filter(!grepl("White", race, ignore.case = TRUE))  #remove "White" racial group
long_race_nw$province <- factor(long_race_nw$province, levels = c("Canada", "B.C.", "Ontario", "Quebec"))  #set the order of provinces
long_race_nw$race <- factor(long_race_nw$race, levels = c("Black", "Latin American", "East Asian", "South Asian", "Southeast Asian", "West Asian", "Mixed", "Other"))  #set the order of provinces

ggplot(long_race_nw, aes(x = province, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percent Non-White Comparison by Province",
    x = "Province",
    y = "Percent"
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "Race") +
  theme_minimal()


##INDIGENOUS ----
#select native data
native_df <- filter(df, topic=="Indigenous population")
native_df <- select(native_df, !"topic")
view(native_df)

#remove all values except "Indigenous identity"
native_df <- native_df %>%
  filter(characteristic == "Indigenous identity")

#MANIPULATE
#pivot_longer
long_native <- native_df %>% pivot_longer(cols = contains("count"))

long_native <- native_df %>%
  pivot_longer(cols = contains("rate"), names_to = "province") %>%
  mutate(province = gsub("_rate", "", province)) %>%
  select(-contains("count")) %>%  #remove columns containing "count"
  select(province, everything()) %>%  #move province column to first column
  filter(!grepl("total", characteristic, ignore.case = TRUE)) %>%  #remove rows containing "total"
  rename(race = characteristic)   #rename "characteristic" row to "status"
view(long_native)

#rename values
long_native <- long_native %>%
  mutate(race = ifelse(race == "Indigenous identity", "Indigenous", race)) %>%
  mutate(province = recode(province, "can" = "Canada", "bc" = "B.C.", "on" = "Ontario", "qb" = "Quebec"))

##COMBINE RACE AND INDIGENOUS (stack vertically) ----
combined_race <- rbind(long_race, long_native)
view(combined_race)

#VISUALIZE COMBINED RACE AND INDIGENOUS 
# create a bar chart of percent COMBINED race by province (without White group!)
combined_race_nw <- combined_race %>%
  filter(!grepl("White", race, ignore.case = TRUE))  #remove "White" racial group
combined_race_nw$province <- factor(combined_race_nw$province, levels = c("Canada", "B.C.", "Ontario", "Quebec"))  #set the order of provinces
combined_race_nw$race <- factor(combined_race_nw$race, levels = c("Black", "Latin American", "East Asian", "South Asian", "Southeast Asian", "West Asian", "Indigenous", "Mixed", "Other"))  #set the order of provinces

ggplot(combined_race_nw, aes(x = province, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percent Non-White Comparison by Province",
    x = "Province",
    y = "Percent"
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Race") +
  theme_minimal()


##GENERATION STATUS ----
#select gen data
gen_df <- filter(df, topic=="Generation status")
gen_df <- select(gen_df, !"topic")
view(gen_df)

#MANIPULATE
#pivot_longer
long_gen <- gen_df %>% pivot_longer(cols = contains("count"))

long_gen <- gen_df %>%
  pivot_longer(cols = contains("rate"), names_to = "province") %>%
  mutate(province = gsub("_rate", "", province)) %>%
  select(-contains("count")) %>%  #remove columns containing "count"
  select(province, everything()) %>%  #move province column to first column
  filter(!grepl("total", characteristic, ignore.case = TRUE)) %>%  #remove rows containing "total"
  rename(status = characteristic)   #rename "characteristic" row to "status"
view(long_gen)

#rename values
long_gen <- long_gen %>%
  mutate(province = recode(province, "can" = "Canada", "bc" = "B.C.", "on" = "Ontario", "qb" = "Quebec"))

#VISUALIZE GENERATION STATUS
# create a bar chart of percent generation status by province 
long_gen$province <- factor(long_gen$province, levels = c("Canada", "B.C.", "Ontario", "Quebec"))  #set the order of provinces

ggplot(long_gen, aes(x = province, y = value, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Percent Generation Status Comparison by Province",
    x = "Province",
    y = "Percent"
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "Generation status") +
  theme_minimal()

#OTHER-----
#create tibble
#df <- as_tibble(df)  # Convert your data frame to a tibble (if not already a tibble) ALREADY IS

#colnames(df) <- df[1:2, ] #Set row 1&2 as column names
#df <- df[-2, ]  # Remove the second row, which is now the column names

#create flipped dataset - flip (transpose) rows and columns
#transposed_df <- t(df)
#view(transposed_df)

#rename columns of transposed data and remove rows 1-2
#new_column_names <- c("topic", "characteristic", "can_count", "can_rate", "bc_count", "bc_rate", "on_count", "on_rate", "qb_count", "qb_rate")
#colnames(df) <- new_column_names #set column names to dataframe
#df <- df[-c(1:3), ] #remove rows 1-3
#view(df)




#RENAME COLUMN TO ROW NAMES
#race_df_2 <- column_to_rownames(race_df, var = "characteristic")
#head(race_df_2, n = 2)
#view(race_df_2)
#rowMeans(race_df_2, na.rm = TRUE) #calculate rowmeans, but x must be numeric SOS!!!!!

#set the row name using values from Column 1
#rownames(race_df) <- race_df$characteristic
#race_df <- race_df[, -1]
#view(race_df)


#simple plotting
#can_rate <- race_df2 %>% pull(can_rate) 
#hist(x = can_rate)


#select relevant columns
mydata <- subset(census_CMA, select = c(""))





