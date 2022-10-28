#Created by Jacob Armitage on Aug 23 2022.
#Intent is to create a graphics for "aim 2" the creation of the VIP Index.
#This script does depend on a few local file pathways to both locate the data and save the plots.

#Required Libraries ====
library(tidyverse)
library(scico) #Diverging color scales for continuous data
library(gghighlight) #Allows for making plots with highlighted Series
library(ggrepel) #Helps to make sure geom point labels do not touch


#Read in whole data set ====
df <- readRDS("/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Data/prepped_data/aim_2/19_index_results_third_version.RDS")
#Convert year from numeric into factor
df <- df %>% mutate(year = as.factor(year))


#####
#Heat map of VIP changing over time for each country grouped by region ====

by.region <- function(x = df){
  regions <- unique(df$region)
  for(i in seq_along(regions)){
    plot <- df %>% filter(region == regions[i]) %>%  ggplot(aes(x=year,y=location, fill=result))+
      #add border white color of line thickness 0.25
      geom_tile(colour="white", size=0.10)+
      #remove x and y axis labels
      labs(x="", y="")+
      theme_bw(base_size=7)+
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      #Creates diverging color scale
      scale_fill_scico(palette = 'vikO',direction = -1)+
      guides(fill=guide_colorbar(title="Index Value"))+
      scale_y_discrete(expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      #keeps it squares
      coord_fixed()+
      ggtitle(paste0(regions[i],": ", "VIP Index Values 1980-2019"))

ggsave(filename = paste0("/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/",regions[i],"_heatmap_Index.png"),
       plot = plot,
       width = 6, height = 4, units = "in", dpi = 300)

  }
}

#Running Function to produce and save plots in onedrive
by.region(df)


#####
#Line graph of VIP changing over time for each country grouped by region  ====
by.region.line <- function(x = df){
  regions <- unique(df$region)
  for(i in seq_along(regions)){
    plot <- df %>% filter(region == regions[i]) %>% ggplot(aes(x=year, y=result, group=location))+
      geom_line(aes(color = location))+
      #Modify axis labels
      labs(x="", y="Vaccine Index")+
      theme_bw(base_size=7)+
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      labs(color = "Country")+
      ggtitle(paste0(regions[i],": ", "VIP Index Values 1980-2019"))
    
    ggsave(filename = paste0("/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/",regions[i],"_linegraph_Index.png"),
           plot = plot,
           width = 6, height = 4, units = "in", dpi = 300)
    
  }
}
#Running Function to produce and save plots in onedrive
by.region.line(df)
#
#
#####
#Faceted line graph of VIP changing over time for all regions ====
df_by_region_year <- df %>% group_by(region,year) %>% summarise(region_year = mean(result))
#Making plot
ggplot(df_by_region_year,aes(year,region_year,color = region,group = region))+
geom_line()+
theme_classic(base_size = 8)+ 
theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank())+
ggtitle("Average VIP Index By Region 1990-2019")+
ylab("Mean VIP Index Value")+
gghighlight()+facet_wrap(~region)

ggsave(filename = "/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/regions_overtime.png", width = 8, height = 4, units = "in",dpi = 300)


#####
#Scatter plots of 2019 VIP vs SDI grouped by SDI level ====
df_2019 <- df %>% filter(year == 2019)
#Low SDI
df_2019 %>% filter(sdi<.5790) %>% ggplot(aes(sdi,result))+
  geom_point()+
  geom_smooth(method = glm)+ 
  geom_text_repel(aes(label = location),size = 3)+
  theme_minimal(base_size = 10)+
  ggtitle("2019 VIP Index vs SDI for Countries in Low SDI Group")+
  ylab("VIP Index")+
  xlab("SDI Value")
ggsave(filename = "/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/indexvsdi_low.png", width = 8, height = 4, units = "in",dpi = 300)

#Med SDI
df_2019 %>% filter(sdi >=.5790 & sdi <=.7432) %>%  ggplot(aes(sdi,result))+
  geom_point()+
  geom_smooth(method = glm)+ 
  geom_text_repel(aes(label = location),size = 3)+
  theme_minimal(base_size = 10)+
  ggtitle("2019 VIP Index vs SDI for Countries in Meduim SDI Group")+
  ylab("VIP Index")+
  xlab("SDI Value")
ggsave(filename = "/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/indexvssdi_med.png", width = 8, height = 5, units = "in",dpi = 300)

#High SDi
df_2019 %>% filter(sdi > .7432) %>%  ggplot(aes(sdi,result))+
  geom_point()+
  geom_smooth(method = glm)+ 
  geom_text_repel(aes(label = location),size = 3)+
  theme_minimal(base_size = 10)+
  ggtitle("2019 VIP Index vs SDI for Countries in High SDI Group")+
  ylab("VIP Index")+
  xlab("SDI Value")
ggsave(filename = "/Users/ziva/Library/CloudStorage/OneDrive-UW/General/Visualizations/Jacob_aim_2/indexvssdi_high.png", width = 8, height = 4, units = "in",dpi = 300)
  

#####
