# 09_prep_disease_data_for_children.R
# Updated on January 10, 2023

# Instead of using disease burden for all ages, specify age group 
# to be children 1-4 years of age

# set up
library(readxl)

drive <- "C:/Users/frc2/UW/og_phi_global_vaccination_improvement_project - General"
file_list <- data.table(read_xlsx(paste0(drive, "/Data/list_of_data_used.xlsx")))

# subset files to latest disease trends data
file_list <- file_list[data_type=="disease_trends" & data_source=="gbd_estimate"]

# specify text to indicate progress
print("Now prepping:")

for (i in 1:nrow(file_list)){
  
  # Set up file path 
  raw_data_dir <- paste0(drive, '/Data/raw_data/')
  inFile <- file_list$file_name[i]
  
  file_dir = paste0(raw_data_dir, file_list$data_type[i], '/', file_list$data_source[i], '/', file_list$disease[i], '/' )

  dx_data = read.csv(paste0(file_dir, inFile), encoding = "UTF-8")
  
  #Add indexing data
  append_cols = file_list[i, .(file_name, data_type, data_source)]
  
  stopifnot(nrow(append_cols)==1)
  
  tmpData = cbind(dx_data, append_cols)
  
  #Bind data together 
  if(i==1){
    prepped_dx_data = tmpData
  } else {
    prepped_dx_data = rbind(prepped_dx_data, tmpData, use.names=TRUE, fill = TRUE)
  }
  
  print(paste0(i, " ", file_list$data_type[i], " ", file_list$disease[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
  
}

# formatting of data ----
# load recently extracted data
dx_dt <- prepped_dx_data

# subset columns of interest
dx_dt <- dx_dt %>% 
  select(measure_name, location_id, location_name, 
         cause_id, cause_name, metric_name,
         year, val, upper, lower)

# rename columns for consistency
setnames(dx_dt, old = c("year"), new = c("year_id"))

# recode YDL values
dx_dt <- dx_dt %>%
  mutate(measure_name = recode(measure_name, Deaths='deaths', `YLDs (Years Lived with Disability)`='ylds'),
         metric_name  = recode(metric_name, Number='number', Percent='percent', Rate='rate'))

# split into three datasets (number, percent, rate)
dx_numb <- dx_dt %>% filter(metric_name=="number")
dx_rate <- dx_dt %>% filter(metric_name=="percent")
dx_perc <- dx_dt %>% filter(metric_name=="rate")

# pivot each data set wider
dx_numb <- dx_numb %>% 
  pivot_wider(
    names_from = c(measure_name, metric_name),
    names_glue = "{measure_name}_{metric_name}_{.value}",
    values_from = c(val, upper, lower)
  )

dx_rate <- dx_rate %>% 
  pivot_wider(
    names_from = c(measure_name, metric_name),
    names_glue = "{measure_name}_{metric_name}_{.value}",
    values_from = c(val, upper, lower)
  )

dx_perc <- dx_perc %>%
  pivot_wider(
    names_from = c(measure_name, metric_name),
    names_glue = "{measure_name}_{metric_name}_{.value}",
    values_from = c(val, upper, lower)
  )

# bind datasets back together
mergeCols <- c("location_id", "location_name", "cause_id", "cause_name", "year_id")

tidy_data <- dx_numb %>% 
  full_join(dx_rate, by = mergeCols) %>%
  full_join(dx_perc, by = mergeCols)

# save prepped data 
prepped_data_dir <- paste0(drive, "/Data/prepped_data/")
outputFile08updated <- paste0(prepped_data_dir, "aim_1/07_disease_trends_children_under_4.RDS")
saveRDS(tidy_data, outputFile08updated)

# print final statement
print("Step 08: Reading in disease trend data completed.")

