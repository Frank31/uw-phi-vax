# 05_aim1_analyses.R
# Francisco Rios Casas
# March 20 2023

# Purpose: Loop that extracts all variables necessary for analysis

# Note: Set the working directory at the root of this repository
source("./01_set_up_file.R")

# library(aod)

################################################
# corrections for multiple comparisons #########
################################################

# for testing five variables divided alpha of 0.05/6
0.05/4
# 0.0125

# for testing twelve terms hypothesis, divide alpha of 0.05/12
0.05/12
# 0.004166667

# for testing six variables divide alpha of 0.05/6
0.05/6
# 0.00833

# for testing sixteen terms hypothesis,divide alpha of 0.05/16
0.05/16 
# 0.003125


##############################
# 2008
##############################
data08 <- readRDS(file = paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_2008.rds"))

# add any final data transformations here
data08 <- data08 %>%
  # drop rows with missing vaccination data
  filter(HPVI_ANY %in% c("YES", "NO")) %>%
  # create new binary variable 
  mutate(HPVI_BIN = HPVI_ANY) %>%
  # recode new binary variable
  mutate(HPVI_BIN = 
           case_when(HPVI_BIN=="YES" ~ 1,
                     HPVI_BIN=="NO" ~ 0))

# fit a regression identifying the factors that determine vaccination for HPV
model08 <- glm(HPVI_BIN ~ RACEETHK + LANGUAGE + INCPOV1 + EDUC1, family = "binomial", data = data08)
summary(model08)

# race
wald.test(b=coef(model08),
          Sigma = vcov(model08),
          Terms = 2:4)

# language
wald.test(b=coef(model08),
          Sigma = vcov(model08),
          Terms = 5:6)

# income
wald.test(b=coef(model08),
          Sigma = vcov(model08),
          Terms = 7:9)

# education
wald.test(b=coef(model08),
          Sigma = vcov(model08),
          Terms = 10:12)

# create table of the odds ratio and the confidence interval
OR <- round(exp(coef(model08)), 2)
CI <- round(exp(confint(model08)), 2)

# rename confidence interval columns
colnames(CI) <- c("Lower", "Higher")

# bind columns together as dataset
table08 <- as.data.frame(cbind(OR, CI))

# aadd 95% CI in a single column

table08$CI <- paste0("(", table08$Lower, "-", table08$Higher, ")")

# subset columns
table08 <- table08[,c(1, 4)]

# rename row names
rownames(table08) = c("(Intercept)", "White", "Black", "Other + Multiple Race",
                         "Language: Spanish", "Language: Other",
                         "Income Above Poverty <= $75K", "Income Below Poverty", "Income Unknown",
                         "Education: 12 years", "Education: More than 12 years", "Education: College Graduate")

# export table
write.csv(table08, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/01_2008_regression_results.csv")

##############################
# 2014
##############################

# load the survey data for 2014 -- and later repeat the analysis for each year
data14 <- readRDS(file = paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_2014.rds"))

# add any final data transformations here
data14 <- data14 %>%
  # drop rows with missing vaccination data
  filter(HPVI_ANY_REC %in% c("YES", "NO")) %>% 
  # create new binary variable
  mutate(HPVI_BIN = HPVI_ANY_REC) %>%
  # recode new binary variable
  mutate(HPVI_BIN = 
           case_when(HPVI_BIN=="YES" ~ 1,
                     HPVI_BIN=="NO" ~ 0))

# fit a regression identifying the factors that determine vaccination for HPV
model14 <- glm(HPVI_BIN ~ RACEETHK + SEX + LANGUAGE + INCPOV1 + RENT_OWN + EDUC1, family = "binomial", data = data14)
summary(model14)

# race
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 2:4)

# sex
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 5)

# language
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 6:7)

# income
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 8:10)

# rent/own
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 11:14)

# education
wald.test(b=coef(model14),
          Sigma = vcov(model14),
          Terms = 15:17)

# create table for export
# create table of the odds ratio and the confidence interval
OR <- round(exp(coef(model14)), 2)
CI <- round(exp(confint(model14)), 2)

# rename confidence interval columns
colnames(CI) <- c("Lower", "Higher")

# bind columns together as dataset
table14 <- as.data.frame(cbind(OR, CI))

# aadd 95% CI in a single column
table14$CI <- paste0("(", table14$Lower, "-", table14$Higher, ")")

# subset columns
table14 <- table14[,c(1, 4)]

# rename row names
rownames(table14) = c("(Intercept)", "White", "Black", "Other + Multiple Race",
                      "Sex: Female",
                      "Language: Spanish", "Language: Other",
                      "Income Above Poverty <= $75K", "Income Below Poverty", "Income Unknown",
                      "Rent Home", "Other Housing Arrangement", "Housing: Don't know", "Housing:Refused",
                      "Education: 12 years", "Education: More than 12 years", "Education: College Graduate")

# export table
write.csv(table14, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/02_2014_regression_results.csv")


##############################
# 2021
##############################

# load the 2021 survey data here
data21 <- readRDS(file=paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_2021.rds"))

# add any final data transformations here
data21 <- data21 %>%
  # drop rows with missing vaccination data
  filter(HPVI_ANY %in% c("YES", "NO")) %>% 
  # create new binary variable
  mutate(HPVI_BIN = HPVI_ANY) %>%
  # recode new binary variable
  mutate(HPVI_BIN = 
           case_when(HPVI_BIN=="YES" ~ 1,
                     HPVI_BIN=="NO" ~ 0))

# add any model checks here

# fit a regression identifying the factors that determine vaccination for HPV
model21 <- glm(HPVI_BIN ~ RACEETHK + SEX + LANGUAGE + INCPOV1 + RENT_OWN + EDUC1, family = "binomial", data = data21)
summary(model21)

# examine individual factors to see if they are related to the outcome

# race
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 2:4)

# sex
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 5)

# language
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 6:7)

# income
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 8:10)

# rent/own
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 11:14)

# education
wald.test(b=coef(model21),
          Sigma = vcov(model21),
          Terms = 15:17)

# create table of the odds ratio and the confidence interval
OR <- round(exp(coef(model21)), 2)
CI <- round(exp(confint(model21)), 2)

# rename confidence interval columns
colnames(CI) <- c("Lower", "Higher")

# bind columns together as dataset
table21 <- as.data.frame(cbind(OR, CI))

# aadd 95% CI in a single column
table21$CI <- paste0("(", table21$Lower, "-", table21$Higher, ")")

# subset columns
table21 <- table21[,c(1, 4)]

# rename row names
rownames(table21) = c("(Intercept)", "White", "Black", "Other + Multiple Race",
                      "Sex: Female",
                      "Language: Spanish", "Language: Other",
                      "Income Above Poverty <= $75K", "Income Below Poverty", "Income Unknown",
                      "Rent Home", "Other Housing Arrangement", "Housing: Don't know", "Housing:Refused",
                      "Education: 12 years", "Education: More than 12 years", "Education: College Graduate")


# export table
write.csv(table21, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/03_2021_regression_results.csv")

# load and merge exported table with semi formatted table
semi_formatted_table <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/regression_results_semi_formatted_table.csv")
csv_08 <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/01_2008_regression_results.csv")
test08 <- semi_formatted_table %>% full_join(csv_2008, by="X")
write.csv(test08, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/04_2008_regression_results_semi_formatted.csv")

# load and merge exported table with semi formatted table
semi_formatted_table <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/regression_results_semi_formatted_table.csv")
csv_14 <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/02_2014_regression_results.csv")
test14 <- semi_formatted_table %>% full_join(csv_14, by="X")
write.csv(test14, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/05_2014_regression_results_semi_formatted.csv")

# load and merge exported table with semi formatted table
semi_formatted_table <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/regression_results_semi_formatted_table.csv")
csv_21 <- read.csv(file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/03_2021_regression_results.csv")
test21 <- semi_formatted_table %>% full_join(csv_21, by="X")
write.csv(test21, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/06_2021_regression_results_semi_formatted.csv")


# interpreting the results--how is the outcome variable coded
# does it require exponentiating
# https://stats.oarc.ucla.edu/r/dae/logit-regression/

# ##############################
# # 2016 --no longer needed
# ##############################
# 
# # load the survey data for 2016 -- and later repeat the analysis for each year
# data16 <- readRDS(file = paste0(data_folder, "prepped_data/cdc_nis_teen/nisteen_data_2016.rds"))
# 
# # add any final data transformations here
# data16 <- data16 %>%
#   # drop rows with missing vaccination data
#   filter(HPVI_ANY %in% c("YES", "NO")) %>% 
#   # create new binary variable
#   mutate(HPVI_BIN = HPVI_ANY) %>%
#   # recode new binary variable
#   mutate(HPVI_BIN = 
#            case_when(HPVI_BIN=="YES" ~ 1,
#                      HPVI_BIN=="NO" ~ 0))
# 
# # fit a regression identifying the factors that determine vaccination for HPV
# model16 <- glm(HPVI_BIN ~ RACEETHK + SEX + LANGUAGE + INCPOV1 + RENT_OWN + EDUC1, family = "binomial", data = data16)
# summary(model16)
# 
# # race
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 2:4)
# 
# # sex
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 5)
# 
# # language
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 6:7)
# 
# # income
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 8:10)
# 
# # rent/own
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 11:14)
# 
# # education
# wald.test(b=coef(model16),
#           Sigma = vcov(model16),
#           Terms = 15:17)
# 
# # exponentiate the results to get the odds ratio
# # exp(coef(model16))
# # 
# # # confidence intervals
# # exp(confint(model16))
# 
# # create table of the odds ratio and the confidence interval
# OR <- round(exp(coef(model16)), 2)
# CI <- round(exp(confint(model16)), 2)
# 
# # rename confidence interval columns
# colnames(CI) <- c("Lower", "Higher")
# 
# # bind columns together as dataset
# table16 <- as.data.frame(cbind(OR, CI))
# 
# # aadd 95% CI in a single column
# table16$CI <- paste0("(", table16$Lower, "-", table16$Higher, ")")
# 
# # subset columns
# table16 <- table16[,c(1, 4)]
# 
# rownames(table14) = c("(Intercept)", "White", "Black", "Other + Multiple Race",
#                       "Sex: Female",
#                       "Language: Spanish", "Language: Other",
#                       "Income Above Poverty <= $75K", "Income Below Poverty", "Income Unknown",
#                       "Rent Home", "Other Housing Arrangement", "Housing: Don't know", "Housing:Refused",
#                       "Education: 12 years", "Education: More than 12 years", "Education: College Graduate")
# 
# 
# # export table
# write.csv(table16, file = "C:/Users/frc2/UW/og_merck_hpv_vaccine_hesitancy - Documents/General/Results/03_2021_regression_results.csv")
