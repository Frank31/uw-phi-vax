### Create visualizations that better summarize methods and results

# set up
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(colorBlindness)
library(readr)
library(readxl)
library(tidyverse)
library(RColorBrewer)

# load the final results
data <- read.csv("/Users/francisco/Documents/Manuscripts/Heliyon/improvement_index.csv")

# Figure 1: Create map showing distribution of index in the most recent year

# create map dataframe
world <- map_data("world")

#unique(world$region)

# create blank map
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

# load data for plotting and keep only country, result and iso code
recent_index <- data %>% filter(year==2019) %>% select(location, result, iso_code)

# make sure country names in data match country names in data frame
diff <- setdiff(world$region, recent_index$location)

# # some data I don't need plotted so I will drop those countries
# diff_data <- as.data.frame(diff)
# write.csv(diff_data, "/Users/francisco/Documents/Manuscripts/VIP index/Data/locations_to_clean.csv", row.names=FALSE)

# re-code some of the location variables for easy merging
world <- world %>% 
  mutate(region = recode(str_trim(region),
                         "Bolivia" = "Bolivia (Plurinational State of)",
                         "Brunei" = "Brunei Darussalam",
                         "Czech Republic"="Czechia",
                         "Iran" = "Iran (Islamic Republic of)",
                         "Ivory Coast" = "Côte d'Ivoire",
                         "Laos" = "Lao People's Democratic Republic",
                         "North Korea" = "Democratic People's Republic of Korea",
                         "Republic of Congo" = "Congo",
                         "Russia" = "Russian Federation",
                         "South Korea" = "Republic of Korea",
                         "Syria"= "Syrian Arab Republic",
                         "Taiwan"= "Taiwan (Province of China)",
                         "Tanzania"= "United Republic of Tanzania",
                         "USA" = "United States of America",
                         "UK" = "United Kingdom",
                         "Vietnam"= "Viet Nam",
                         "Virgin Islands"="United States Virgin Islands"))

### plus a lot more cleaning is required

# merge the datasets by location
worldSubset <- right_join(world, recent_index, by = c("region"="location"))
head(worldSubset)

# create map showing a distribution of the data
## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldVIP <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = result)) +
  scale_fill_distiller(palette ="RdBu", direction = 1, name = "VIP Index") + # or direction=1
  ggtitle("Map of VIP Index") +
  plain

worldVIP

# export the image
png(file="/Users/francisco/Documents/Manuscripts/Heliyon/map_of_index.png",
    width = 2400, height = 1400, res = 300)
worldVIP
dev.off()


# Create map showing regions that have undergone the biggest change between earliest and latest time period
index_trends <- data %>% filter(year %in% c(2019, 1990)) %>% select(location, result, year)

# reshape the data wide
index_trends_wide <- index_trends %>% pivot_wider(id_cols = "location", names_from = "year", names_prefix = "year_", values_from = "result")
index_trends_wide <- index_trends_wide %>%
  mutate(diff = year_2019-year_1990)

# merge this data onto the map dataframe
worldSubsetTrends <- right_join(world, index_trends_wide, by = c("region"="location"))
head(worldSubsetTrends)

# consider 
world_coordinates <- map_data("world") 

worldVIPtrends <- ggplot(data = worldSubsetTrends, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = diff)) +
  scale_fill_distiller(palette ="RdBu", direction = 1, name = "Difference in VIP Index") + # or direction=1
  ggtitle("Map of changes in the VIP Index between 1990 to 2019") +
  plain

worldVIPtrends

# Create a heatmap of the results
png(file="/Users/francisco/Documents/Manuscripts/Heliyon/map_of_index_trends.png",
    width = 2400, height = 1400, res = 300)
worldVIPtrends
dev.off()


#####
#Heat map of VIP changing over time for each country grouped by region ====

heatmap_data <- data %>%
  filter(year==2019) %>%
  select(location, sdi, the_per_cap_mean, ghes_per_the_mean, dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban, mean_agree_vac_safe, 
         mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective, gov_trust, result)

# reshape the data long
heatmap_plot_data <- pivot_longer(heatmap_data, cols=c(2:15), names_to = "variable", values_to = "values")

# factor the variable levels
heatmap_plot_data$variable.f <- factor(heatmap_plot_data$variable,
                                       levels = c("sdi", "the_per_cap_mean", "ghes_per_the_mean", "dah_per_cap_ppp_mean", "haqi",
                                       "cpi", "perc_skill_attend", "imm_pop_perc", "perc_urban", "mean_agree_vac_safe", "mean_agree_vac_important",
                                       "mean_agree_vac_effective", "gov_trust", "result"),
                                       labels = c("SDI", "HExp", "GHES", "DAH", "HAQI", 
                                                  "CPI", "SBA", "Img", "Urb", "VSafe", "VImp", 
                                                  "VEff", "Trust", "VIP"))

# make heatmap of highest ranking countries in 2019
View(heatmap_plot_data %>% filter(variable=="result"))

heatmap_vis_high <- ggplot(heatmap_plot_data %>% 
                        filter(location %in% c(
                          "Greenland", "San Marino", "Tuvalu", "Finland", "Cook Islands", 
                          "Nauru", "Saudi Arabia", "Panama", "Brunei Darussalam", "Belgium")), 
                      aes(variable.f, location, fill=values)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_distiller(palette = "RdBu", direction =1) +
  ggtitle("Heatmap of index components and results in highest-scoring countries in 2019") +
  xlab("Component") +
  ylab("Country")

png(file="/Users/francisco/Documents/Manuscripts/Heliyon/heatmap_2019_high.png", 
    width = 2400, height = 1400, res = 300)
heatmap_vis_high
dev.off()

heatmap_vis_low <- ggplot(heatmap_plot_data %>% 
                            filter(location %in% c(
                              "Somalia", "South Sudan", "Bosnia and Herzegovina", "Central African Republic", "Eritrea", 
                              "Chad", "Tokelau", "Guinea-Bissau", "North Macedonia", "Niger")), 
                          aes(variable.f, location, fill=values)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_distiller(palette = "RdBu", direction =1) +
  ggtitle("Heatmap of index components and results in lowest-scoring countries in 2019") +
  xlab("Component") +
  ylab("Country")

png(file="/Users/francisco/Documents/Manuscripts/Heliyon/heatmap_2019_low.png", 
    width = 2400, height = 1400, res = 300)
heatmap_vis_low
dev.off()

# Make heat map of the most changed between the time period
index_trends_wide$abs_diff <- abs(index_trends_wide$diff)

# load the merged dataset for plotting
merged_df <- readRDS(file="/Users/francisco/Documents/Manuscripts/VIP index/Data/17_merged_dataset_third_version.RDS")


# load vaccine data
vax_data <- read.csv(file="/Users/francisco/Documents/Manuscripts/VIP index/Data/vaccination_data.csv") %>%
  filter(year_id==2019) %>% filter(vaccine_name=='DTP3')

# subset the data for merging
data_subset <- data %>% filter(year==2019)

# merge vaccine and index data together
dtp3_vip_data <- vax_data %>% inner_join(data_subset, by=c("location_id"="gbd_location_id"))

