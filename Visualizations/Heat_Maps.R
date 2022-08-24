#Created by Jacob Armitage on Aug 23 2022
#Intent is to create a heat maps for index and component variables

library(tidyverse)

#read in whole dataset
df <- readRDS("/Users/ziva/R Projects/uw-phi-vax/aim_2/11_index_results.RDS")

#Just 2019 data
df_2019 <- df %>% filter(year ==2019)

df_NorthAM <- df %>% filter(region=="North Africa and Middle East")

#Making plot
ggplot(df_NorthAM,aes(x=year,y=location, fill=result))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="", y="")+
  theme_minimal(base_size=7)+
  #keeps it squares
  #coord_fixed()+
  scale_fill_gradient(low = "#C6DBEF", high = "#08306B")+
  guides(fill=guide_legend(title="Index Value"))+
  scale_y_discrete(expand=c(0, 0))
  
  
  
#Scale x discrete removes white space on left and right but also removes x label, example used breaks but when i do it, they still dissappear, may need to make year a factor?

