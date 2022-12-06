# Purpose: Final transformations of data before calculating index
# Author: 
# Date: July 12, 2022

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/third_version/01_set_up_R.R"))

# Load final data for analysis
final_data <- readRDS(paste0(prepped_data_dir, "aim_2/18_prepped_data_for_analysis_third_version.RDS"))

# Normalize to ensure all values are between 0 and 1
normVars = c('the_per_cap_mean', 'dah_per_cap_ppp_mean', 'imm_pop_perc', 'perc_urban')

norm_cut <- read_xlsx(paste0(codebook_directory, "vaccine_index_normalizations_cutoffs.xlsx"))

i <- 1
for (i in 1:length(normVars)) {
  min = norm_cut$min[i]
  max = norm_cut$max[i]
  v = norm_cut$variable[i]
  final_data[, (v):=(get(v)-min)/(max-min)]
}

# Set DAH value to NA if not eligible to receive funds
final_data <- final_data %>%
  mutate(dah_per_cap_ppp_mean = case_when(
    dah_eligible==TRUE ~ dah_per_cap_ppp_mean
  ))

# calculate how many variables will be considered for the geometric mean
final_data <- final_data %>%
  mutate(n_for_geo_mean = case_when(
    rowSums(is.na(final_data[,8:20]))==0 ~ 13,
    rowSums(is.na(final_data[,8:20]))==1 ~ 12,
    rowSums(is.na(final_data[,8:20]))==2 ~ 11,
    rowSums(is.na(final_data[,8:20]))==3 ~ 10,
    rowSums(is.na(final_data[,8:20]))==4 ~ 9,
    rowSums(is.na(final_data[,8:20]))==5 ~ 8,
    rowSums(is.na(final_data[,8:20]))==6 ~ 7,
    rowSums(is.na(final_data[,8:20]))==7 ~ 6,
    rowSums(is.na(final_data[,8:20]))==8 ~ 5,
    rowSums(is.na(final_data[,8:20]))==9 ~ 4,
    rowSums(is.na(final_data[,8:20]))==10 ~ 3,
    rowSums(is.na(final_data[,8:20]))==11 ~ 2,
    rowSums(is.na(final_data[,8:20]))==12 ~ 1,
    rowSums(is.na(final_data[,8:20]))==13 ~ 0))

# Calculate the geometric mean--each location takes into account a diverse number of variables
final_data$product <- NA

for (i in 1:nrow(final_data)){
  final_data$product[i] <- apply(final_data[,8:20][i], 1, prod, na.rm=TRUE)
}

# calculate index by taking the nth root
final_data$result <- final_data$product^(1/final_data$n_for_geo_mean)

# drop values for 2020 since those are based almost entirely on imputed data
final_data <- final_data %>% filter(year<=2019)

hist(final_data$result)

# select columns of interest
final_data <- final_data %>% select(location, year, gbd_location_id, iso_code, iso_num_code,
                                    region, dah_eligible, sdi, the_per_cap_mean, ghes_per_the_mean,
                                    dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc,
                                    perc_urban, mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective, gov_trust, result)

# Save final results
saveRDS(final_data, file=paste0(prepped_data_dir, "aim_2/19_index_results_third_version.RDS"))
write.csv(final_data, file = paste0(prepped_data_dir, "aim_2/19_index_results_third_version.csv"))

# create plot for each country to observe any sharp increases or drops
# Save file
pdf(paste0(visDir, "aim_2/third_version/11_country_results.pdf"), height=5.5, width=9)

locations <- unique(final_data$location)
for (i in 1:length(locations)) {
  h <- locations[i]
  g <- ggplot(final_data %>% filter(location==h), aes(x=year, y=result)) +
    geom_line() +
    labs(title=paste0('Index results in ',h), y='Index Value', x='Year') +
    ylim(0, 1)
  print(g)
}

dev.off()


