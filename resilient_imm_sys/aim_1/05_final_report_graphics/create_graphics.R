# load the NIS data






















# create graphics

s <- "North Carolina" # state

# load state level data
state_data <- readRDS(paste0(prepped_data_dir, "11_merged_data_for_state_profile_docs.RDS")) %>%
  # filter state
  filter(state==s)

# reshape data for plotting trends in vaccination gaps
state_data_gaps <- state_data %>% 
  select(state, YEAR, income, VACCINE, hispa_diff, black_diff, other_diff) %>%
  pivot_longer(cols = c(hispa_diff, black_diff, other_diff),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable_label = case_when(
    variable=="hispa_diff" ~ "Hispanic",
    variable=="black_diff" ~ "Black/African-American",
    variable=="other_diff" ~ "Other or multi-racial"
  )) %>% 
  mutate(income_label = case_when(
    income=="high" ~ 1,
    income=="med" ~ 2,
    income=="low" ~ 3 ,
    income=="miss" ~ 4,
  ))

# factor income_label
state_data_gaps$income_label <- factor(state_data_gaps$income_label,
                                       labels = c("High income", 
                                                  "Medium income", 
                                                  "Low income", 
                                                  "Missing income "))

# plot trends in vaccination coverage among different racial/ethnic groups
# ggplot(state_data_gaps, aes(YEAR, value, group=factor(VACCINE))) + 
#   geom_line(aes(color=VACCINE), show.legend = TRUE) +
#   facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
#   labs(title=paste('Differences in percent of children fully vaccinated in each racial/ethnic group'), 
#        subtitle=paste0('compared to white children of same income background in', " ", s),
#        caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
#   ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
#   xlab('Year') +
#   ylim(-.03, .06) +
#   theme_minimal()

state_data_gaps <- state_data_gaps %>% filter(VACCINE=="DTP")

nc <- ggplot(state_data_gaps, aes(YEAR, value, group=factor(variable_label))) + 
  geom_line(aes(color=variable_label), show.legend = TRUE) +
  facet_grid(~income_label)+
  # facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
  labs(title=paste('Differences in percent of children fully vaccinated for DTP vaccine in each racial/ethnic group'), 
       subtitle=paste0('compared to white children of same income background in', " ", s),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
  ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
  xlab('Year') +
  ylim(-.03, .06) +
  theme_minimal()

png(filename = "C:/Users/frc2/UW/Merck Resilient Immunization Programs Project - Aim 1/Results/graphics/final_report/01_north_carolina_diff.png",
    width = 9, height =5, units = "in", pointsize = 10, res = 300)
print(nc)
dev.off()

s <- "Washington" # state

ggplot(state_data_gaps, aes(YEAR, value, group=factor(variable_label))) + 
  geom_line(aes(color=variable_label), show.legend = TRUE) +
  facet_grid(~income_label)+
  # facet_grid(vars(income_label), vars(variable_label), labeller = label_value) + 
  labs(title=paste('Differences in percent of children fully vaccinated for DTP vaccine in each racial/ethnic group'), 
       subtitle=paste0('compared to white children of same income background in', " ", s),
       caption="Data Source: 2007 and 2019 National Immunization Survey (NIS)") +
  ylab('Percentage points') + scale_x_continuous(breaks=c(2007, 2019)) +
  xlab('Year') +
  ylim(-.03, .06) +
  theme_minimal()
