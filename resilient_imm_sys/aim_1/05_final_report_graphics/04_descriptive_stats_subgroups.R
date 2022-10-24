# Francisco Rios
# October 11, 2022

# Grab data which shows the trends in equitable vaccine improvement across different states
rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_state_level_analyses/01_set_up_R.R"))

data <- readRDS(paste0(prepped_data_dir, "03_estimates_vaccine_coverage_2007-2019.RDS"))

# final variable prep
data$VACCINE <- tolower(data$VACCINE)
data$VACCINE <- gsub(" ", "", data$VACCINE)

data <- data %>% 
  mutate(RACEETHK_R = case_when(RACEETHK_R==1 ~ "white",
                                RACEETHK_R==2 ~ "hispa",
                                RACEETHK_R==3 ~ "black",
                                RACEETHK_R==4 ~ "other"),
         INCPOV1 = case_when(INCPOV1==1 ~ "high",
                             INCPOV1==2 ~ "med", 
                             INCPOV1==3 ~ "low",
                             INCPOV1==4 ~ "miss"))

# code the locations for plotting
ESTIAPFlevels=c(1,10,105,106,107,109,11,12,13,14,16,17,18,19,2,20,22,25,27,28,29,30,31,34,35,36,38,4,40,41,44,46,47,49,5,50,51,52,53,54,55,56,57,58,59,6,60,61,62,63,64,65,66,68,7,72,73,74,75,76,77,8,95)
ESTIAPFlabels=c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "TX-HIDALGO COUNTY", "TX-TARRANT COUNTY", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC",
                "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO",
                "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "U.S. VIRGIN ISLANDS")
data$ESTIAP <- factor(data$ESTIAP, levels=ESTIAPFlevels, labels=ESTIAPFlabels)

# re-shape the data to facilitate calculation
dt2 <- pivot_wider(data, id_cols = c(ESTIAP, RACEETHK_R, INCPOV1, VACCINE), names_from = c(YEAR), values_from = PredictedProb)

# calculate the change in the time periods of interest for each vaccine
dt2$change <- ((dt2$`2019`-dt2$`2007`)/dt2$`2007`)*100

# reshape data for additional calculations
dt3 <- pivot_wider(dt2, id_cols = c(ESTIAP, INCPOV1, VACCINE), names_from = RACEETHK_R, values_from = c(change))

dt3$eii_hispa <- (dt3$hispa - dt3$white)
dt3$eii_black <- (dt3$black - dt3$white)
dt3$eii_other <- (dt3$other - dt3$white)

# Reshape data for additional calculations
dt4 <- pivot_wider(dt3, id_cols = c(ESTIAP), names_from = c(INCPOV1, VACCINE), values_from = c(eii_hispa, eii_black, eii_other))

# create function that deletes outliers
is_outlier <- function(x){
  lower_bound <- quantile(x, 0.05)
  upper_bound <- quantile(x, 0.95)
  ifelse((x < lower_bound | x > upper_bound), NA, x)
}

numVars <- names(dt4)[2:37]
for (v in numVars) {
  dt4 <- as.data.table(dt4)
  dt4[, (v):=is_outlier(get(v))]
}

# calculate the quantile groups for each column
# summary(dt4)
quants <- c(.25, .75)
quantiles <- as_tibble(apply( dt4[,2:37] , 2 , quantile , probs = quants , na.rm = TRUE))
quantiles$quant <- NA
quantiles$quant[1] <- "first_q"
quantiles$quant[2] <- "third_q"

quantiles <- pivot_longer(quantiles, cols = starts_with("eii"), names_to = c("group"))
quantiles <- pivot_wider(quantiles, id_cols = "group", names_from = "quant", values_from = "value" )
quantiles <- quantiles %>% separate(group, into = c("variable", "race", "income", "vaccine"))
quantiles <- quantiles %>% select(-c(variable))

# reshape dataset
dt5 <- dt4 %>%
  pivot_longer(
    !c(ESTIAP),
    names_to = c("group"),
    values_to = "eii",
    values_drop_na = FALSE) %>% 
  separate(group, into = c("variable", "race", "income", "vaccine")) %>%
  select(-c(variable))

# merge the dt_groups data to the quantiles cut off points
dt6 <- merge(dt5, quantiles, by=c("race", "income", "vaccine"))

# calculate the group for each location, race, income and vaccine
dt6 <- dt6 %>% 
  mutate(equitable_improvement = case_when(eii > 0 ~ TRUE,
                                           TRUE ~ FALSE),
         category = case_when(eii >= first_q & eii <= third_q ~ "medium",
                              eii >= third_q ~ "high",
                              eii <= first_q ~ "low",
                              is.na(eii) ~ "outlier"))

# # merge dt2 and dt6 to create dataset with both percent change, no outliers, and values
plot_data <- dt2 %>% full_join(dt6, by=c("ESTIAP", "RACEETHK_R"="race", "INCPOV1"="income", "VACCINE"="vaccine"))

plot_data <- plot_data %>% mutate(
  category = case_when(
    RACEETHK_R=="white" & is.na(category)~"reference",
    TRUE ~ category ))

# factor the race, and income variables
plot_data$RACEETHK_R <- factor(plot_data$RACEETHK_R, levels=c("white", "hispa", "black", "other"), labels = c("White", "Hispanic", "Black", "Other"))
plot_data$INCPOV1 <- factor(plot_data$INCPOV1, levels=c("low", "med", "high", "miss"), labels = c("Low", "Medium", "High", "Unknown"))
plot_data$category <- factor(plot_data$category, levels = c("reference", "low", "medium", "high", "outlier"), labels = c("Reference", "Worse", "Average", "Better", "Outlier"))
plot_data$VACCINE <- factor(plot_data$VACCINE, levels = c("dtp", "hepb", "mmr"), labels = c("DTP", "HEP B", "MMR"))

# describe the average change by each vaccine
by_vaccine <- 
  plot_data %>% group_by(VACCINE)

by_vaccine %>%
  summarize_at(c("change", "first_q", "third_q"), mean, na.rm=TRUE)

# describe the average change by race/ethnic group
by_eii <- plot_data %>% group_by(VACCINE, RACEETHK_R, INCPOV1) %>% filter(RACEETHK_R!="White")


by_eii <- by_eii %>%
  summarize_at(c("change", "first_q", "third_q", "eii"), mean, na.rm=TRUE)

View(by_eii)

# describe the average change for each vaccine
by_race_inc <- plot_data %>% group_by(RACEETHK_R, INCPOV1, VACCINE)

by_rac_inc <- by_race_inc  %>%
  summarize_at(c("change"), mean)

write.csv(plot_data, file = "//udrive.uw.edu/udrive/remote desktop transfer/merck_dataset.csv")
write.csv(plot_data, file = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/12_prepped_data_for_final_report.csv")
