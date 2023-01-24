# Francisco Rios Casas
# Create graphics to show specific states in graphics
# October 11, 2022

# set up 
rm(list=ls())

# source set up script
source(paste0("C:/Users/frc2/Documents/uw-phi-vax/resilient_imm_sys/aim_1/01_state_level_analyses/01_set_up_R.R"))

# Load data
data <-  read.csv(file = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Data/prepped_data/12_prepped_data_for_final_report.csv")

# re-code the income variable factors


# Add data label for variables to label in the graphics

# Create boxplot change between the two time periods
ggplot(data, aes(x = INCPOV1, y = change)) +
  geom_boxplot() + 
  theme_minimal() +
  facet_grid(cols = vars(VACCINE), vars(RACEETHK_R) )+
  labs(title = paste('Change in vaccine coverage'), 
       y = 'Percent Change', 
       x = 'Income Group', 
       subtitle = paste0('between 2007 and 2019, among all geographic regions in the NIS survey'))

# Create boxplot showing what the distribution of the difference between white and non-white children
eii_data <- data %>% filter(RACEETHK_R != "White" & category!="Outlier")

# refactor the INCOME categories
eii_data$INCPOV1 <- factor(eii_data$INCPOV1, levels = c("High", "Medium", "Low", "Unknown"))

ggplot(eii_data, aes(x = INCPOV1, y = eii)) +
  geom_boxplot() + 
  theme_minimal() +
  facet_grid(cols = vars(RACEETHK_R), vars(VACCINE) )+
  labs(title = paste('Difference between change in white children and children of other racial/ethnic backgrounds'), 
       y = 'Difference', 
       x = 'Income Group', 
       subtitle = paste0('Zero indicates no difference between the two groups. \n Positive value indicates white children saw a worse decrease.'))


# create bar plot showing how well states of interest performed

# Create groupings based on how states compared to each other but also on which were most likely to show equitable improvement
barplot_dt <- data %>%
  filter(category!="Outlier") %>%
  filter(category!="Reference") %>%
  group_by(ESTIAP) %>%
  mutate(freq.high = sum(category=="high"), freq.med = sum(category=="medium"), freq.low = sum(category=="low"), freq.out = sum(category=="outlier")) %>%
  ungroup %>%
  group_by(ESTIAP, category) %>%
  tally %>%
  mutate(pct=n/sum(n))

barplot_dt$category <- factor(barplot_dt$category, levels = c("Worse", "Average", "Better"), labels = c("Worse", "Average", "Better"))

barplot_dt <- barplot_dt %>% filter(ESTIAP %in% c("NM", "NC", "MA", "WA", "VA", "IL-REST OF STATE", "AZ", "OR", "TX-DALLAS COUNTY"))

# re-order the region variables
# barplot_dt$ESTIAP <- factor(barplot_dt$ESTIAP, levels =c("AZ", "OR", "TX-DALLAS COUNTY", "WA", "VA", "IL-REST OF STATE", "NM", "NC", "MA"))

# re-code the ESTIAP Variables that will be plotted to include the full name
barplot_dt <- barplot_dt %>% mutate(ESTIAP = case_when(
  ESTIAP=="AZ" ~ "Arizona",
  ESTIAP=="OR" ~ "Oregon",
  ESTIAP=="TX-DALLAS COUNTY" ~ "Dallas County, Texas",
  ESTIAP=="WA" ~ "Washington",
  ESTIAP=="VA" ~ "Virginia",
  ESTIAP=="IL-REST OF STATE" ~ "Illinois (excluding Chicago)",
  ESTIAP=="NM" ~ "New Mexico",
  ESTIAP=="NC" ~ "North Carolina",
  ESTIAP=="MA" ~ "Massachusetts"
))

barplot_dt$ESTIAP <- factor(barplot_dt$ESTIAP, levels =c("Arizona", "Oregon", "Dallas County, Texas", "Washington", "Virginia", "Illinois (excluding Chicago)", "New Mexico", "North Carolina", "Massachusetts"))


plot2 <- ggplot(barplot_dt, aes(fill=category, x=ESTIAP, y=n, label=round(pct,2))) +
  geom_bar(position="fill", stat="identity") +
  geom_text(size =3, position = position_fill(vjust = .5)) +
  coord_flip() +
  scale_fill_brewer(palette="Blues", name = "Comparison to other states") +
  theme_minimal(base_size = 12) + 
  labs(title=paste0('Relative performance of selected geographic areas'), x="State/area", y="proportion")

image_loc <- "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/final_report/"

png(filename = paste0(image_loc, "02_relative_perf.png"), width = 9, height =5, units = "in", pointsize = 10, res = 300, )
print(plot2)
dev.off()
