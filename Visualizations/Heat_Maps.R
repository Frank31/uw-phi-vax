#Created by Jacob Armitage on Aug 23 2022
#Intent is to create a heat maps for index and component variables

#Required Libraries
library(tidyverse)
library(scico) #Diverging color scales for continuous data


#read in whole dataset
df <- readRDS("/Users/ziva/R Projects/uw-phi-vax/aim_2/11_index_results.RDS")
#Convert year from numeric into factor
df <- df %>% mutate(year = as.factor(year))

#Automatically generating data frame for each region
df_byregion <- split(df, df$region) #Puts them in list
list2env(df_byregion, envir = .GlobalEnv)#Imports to environment



#Making plot of Vaccine Index Value changing over time 

#Specific region as example
ggplot(`Eastern Sub-Saharan Africa`,aes(x=year,y=location, fill=result))+
  #add border white color of line thickness 0.25
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="", y="")+
  theme_minimal(base_size=7)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  #Creates diverging color scale
  scale_fill_scico(palette = 'vikO',direction = -1)+
  guides(fill=guide_colorbar(title="Index Value"))+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  #keeps it squares
  coord_fixed()+
  ggtitle("Vaccine Improvement Index Values 1980-2019")
  
#Work on way to loop through list of date frames to make plots automatically!






#Just 2019 data
df_2019 <- df %>% filter(year ==2019)
 
#Remove some Columns that are not needed 
df_simple_2019 <- df_2019[ , -c(2:7)]

#Convert Data to Long format
df_long_2019 <- df_simple_2019 %>% pivot_longer(!location,names_to = "component",values_to = "Value")

#Make Heatmap of 2019 component variables
ggplot(df_long_2019,aes(x=component,y=location, fill=Value))+
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="", y="")+
  theme_minimal(base_size=6)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  #Creates diverging color scale
  scale_fill_scico(palette = 'vikO',direction = -1)+
  guides(fill=guide_colorbar(title="Component Value"))+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   labels=c("cpi" = "Corruption Perception Index","dah_per_cap_ppp_mean" = "Development Assistance Per Person","ghes_per_the_mean" = "Government Health Spending per Total Health Spending", "haqi" = "HAQI", "imm_pop_perc"= "Immigrant Population (%)", "perc_skill_attend" = "Skilled Attendants at Birth", "perc_urban" = "Urbanicity (%)", "result" = "Improvement Index", "sdi" = "Socio-demographic Index", "the_per_cap_mean" = "Total Health Spending per Person"))+
  #keeps it squares
  #coord_fixed()+
  ggtitle("Vaccine Improvement Index Component Values 2019")

#Make heatmap of percent change between 2010 and 2019 for each component
