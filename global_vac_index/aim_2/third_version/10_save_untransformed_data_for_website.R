# Final prep to data set to plot during the 
test <- readRDS("C:/Users/frc2/Documents/uw-phi-vax/dashboard/data/17_merged_dataset_third_version.RDS")

colnames(test)[8:20] <- paste(colnames(test)[8:20], "untrnsf", sep = "_")

# load the other data set used
index_results <- readRDS("data/aim_2/19_index_results_third_version.RDS") 

names(index_results)

# merge two datasets together
final <- index_results %>% full_join(test, by=c("location", "year", "gbd_location_id",
                                                "iso_code", "iso_num_code", "region", 
                                                "dah_eligible"))

# Save the data on the correct server folder
untransformed_data
names(merged_data_for_visuals)
names(final)
