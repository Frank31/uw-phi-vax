#Created by Jacob Armitage on Aug 23 2022
#Intent is to create a heat maps for index and component variables


library(tidyverse)

library(scico) #Diverging color scales for continuous data


#read in whole dataset
df <- readRDS("/Users/ziva/R Projects/uw-phi-vax/aim_2/11_index_results.RDS")
#Convert year from numeric into factor
df <- df %>% mutate(year = as.factor(year))

#Making Some Regional Subsets
#Only North Africa and Middle East 
df_NorthAM <- df %>% filter(region=="North Africa and Middle East")
#Only Central
df_CentralAsia <- df %>% filter(region=="Central Asia")
#Only Central Europe
df_CentralEurope <- df %>% filter(region=="Central Europe")
#Only Western Sub-Saharan Africa
df_WesternSub-SaharanAfrica <- df %>% filter(region=="Western Sub-Saharan Africa")
#Only Western Europe
df_WesternEurope <- df %>% filter(region=="Western Europe")

#Making subsets by SDI level



#Making plot of Index Value changing over time for NorthAM Region as example
ggplot(df_NorthAM,aes(x=year,y=location, fill=result))+
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
  coord_fixed()
  

#Just 2019 data
df_2019 <- df %>% filter(year ==2019)
 
#Remove some Columns that are not needed 
df_simple_2019 <- df_2019[ , -c(2:7)]

#Convert Data to Long format
df_long_2019 <- df_simple_2019 %>% pivot_longer(!location,names_to = "component",values_to = "Value")

#Make Heatmap of 2019 component variables
ggplot(df_long_2019,aes(x=component,y=location, fill=Value))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="", y="")+
  theme_minimal(base_size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
  #keeps it squares
  #coord_fixed()
  # scale_fill_gradient(low = "#C6DBEF", high = "#08306B")+
  # guides(fill=guide_legend(title="Index Value"))+
  # scale_y_discrete(expand=c(0,0))+
  # scale_x_discrete(expand=c(0,0))


