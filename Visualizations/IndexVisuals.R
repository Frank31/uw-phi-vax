#Created by Jacob Armitage on Aug 23 2022
#Intent is to create a graphics for aim 2 the creation of the VIP Index

#Required Libraries
library(tidyverse)
library(scico) #Diverging color scales for continuous data
library(gghighlight) #Allows for making plots with highlighted Series
library(ggrepel)     #Helps to make sure geom point labels do not touch

#read in whole dataset
df <- readRDS("/Users/ziva/R Projects/uw-phi-vax/aim_2/11_index_results.RDS")
#Convert year from numeric into factor
df <- df %>% mutate(year = as.factor(year))

#Creating Function to make a heatmap for each Region showing Index values changing over time
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

#Experimenting with making line graphs instead of heatmaps Region showing Index values changing over time
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

#Make Heatmap of 2019 component variables:Set Up
#Just 2019 data
df_2019 <- df %>% filter(year ==2019)
#Remove some Columns that are not needed 
df_simple_2019 <- df_2019[ , -c(2:7)]
#Split Data into two equal parts
df_simple_1 <- df_simple_2019[1:87,]
df_simple_2 <- df_simple_2019[88:175,]
#Convert Data to Long format
df_long_1 <- df_simple_1 %>% pivot_longer(!location,names_to = "component",values_to = "Value")
df_long_2 <- df_simple_2 %>% pivot_longer(!location,names_to = "component",values_to = "Value")

#Heatmaps: save manually
#First Half
ggplot(df_long_1,aes(x=component,y=location, fill=Value))+
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="Index Component Values", y="")+
  theme_minimal(base_size=8)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  #Creates diverging color scale
  scale_fill_scico(palette = 'vikO',direction = -1)+
  guides(fill=guide_colorbar(title="Value"))+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   labels=c("cpi" = "Corruption Perception Index","dah_per_cap_ppp_mean" = "Development Assistance Per Person","ghes_per_the_mean" = "Government Health Spending per Total Health Spending", "haqi" = "HAQI", "imm_pop_perc"= "Immigrant Population (%)", "perc_skill_attend" = "Skilled Attendants at Birth", "perc_urban" = "Urbanicity (%)", "result" = "Improvement Index", "sdi" = "Socio-demographic Index", "the_per_cap_mean" = "Total Health Spending per Person"))+
#coord_fixed()+ #Makes graph too long
ggtitle("VIP Index Component Values 2019")

#Second Half
ggplot(df_long_2,aes(x=component,y=location, fill=Value))+
  geom_tile(colour="white", size=0.10)+
  #remove x and y axis labels
  labs(x="Index Component Values", y="")+
  theme_minimal(base_size=8)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  #Creates diverging color scale
  scale_fill_scico(palette = 'vikO',direction = -1)+
  guides(fill=guide_colorbar(title="Value"))+
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   labels=c("cpi" = "Corruption Perception Index","dah_per_cap_ppp_mean" = "Development Assistance Per Person","ghes_per_the_mean" = "Government Health Spending per Total Health Spending", "haqi" = "HAQI", "imm_pop_perc"= "Immigrant Population (%)", "perc_skill_attend" = "Skilled Attendants at Birth", "perc_urban" = "Urbanicity (%)", "result" = "VIP Index", "sdi" = "Socio-demographic Index", "the_per_cap_mean" = "Total Health Spending per Person"))+
  #coord_fixed()+ #Makes graph too long
ggtitle("VIP Index Component Values 2019")


#Make Bar Graph of Vaccine Index by region
Average_Index_Values_2019 <- df_2019 %>% group_by(region) %>% summarise(Average_Index = mean(result))

#BarPlot:Manually save
ggplot(Average_Index_Values_2019,aes(factor(region),Average_Index))+ geom_bar(stat='identity', fill ="#F8766D") + theme_classic(base_size = 8)+theme(axis.text.x = element_text(angle = 45, hjust=1))+ggtitle("Average VIP Index By Region 2019")+xlab("Region")+ylab("Mean VIP Index Value")



#Make Line Graph of Vaccine Index Value Changing over Time for all Regions
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


#Making scatter plots of 2019 VIP vs SDI for low, medium and high SDI, 
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
  

##Make Line graph showing components changing for Southern Latin America over time
#Making Dataframe
df_southernlatin_component_year <- df %>% filter(region == "Southern Latin America") %>% group_by(year) %>% summarise(Corruption_Perception_Index = mean(cpi),
Development_Assistance_Per_Person = mean(dah_per_cap_ppp_mean), 
Goverment_Health_Spending_per_Total_Health_Spending = mean(ghes_per_the_mean), 
HAQI = mean(haqi),
Immigrant_Population = mean(imm_pop_perc),
Skilled_Attendants_at_Birth = mean(perc_skill_attend),
Urbanictiy = mean(perc_urban), 
VIP_INDEX = mean(result),
SDI = mean(sdi), 
Total_Health_Spending_per_Person = mean(the_per_cap_mean))
#Changing Data frame to long format
df_Southerlatin_longer <- df_southernlatin_component_year %>% pivot_longer(!year,names_to = "Component",values_to = "Value")

#Making plot of Southern Latin America
ggplot(df_Southerlatin_longer,aes(year,Value, group = Component, color = Component))+
  geom_line(size = 2)+
  theme_minimal(base_size = 10)+ 
  #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank())+
  ggtitle("Average VIP Index Component Value 1990-2019 Southern Latin America")+
  ylab("Component Value")+
  xlab("Year")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_color_discrete(labels=c("Corruption_Perception_Index" = "Corruption Perception Index","Development_Assistance_Per_Person" = "Development Assistance Per Person","Goverment_Health_Spending_per_Total_Health_Spending" = "Government Health Spending per Total Health Spending","Immigrant_Population"= "Immigrant Population (%)", "Skilled_Attendants_at_Birth" = "Skilled Attendants at Birth", "Urbanicity" = "Urbanicity (%)","VIP_INDEX" = "VIP Index", "SDI" = "Socio-demographic Index", "Total_Health_Spending_per_Person" = "Total Health Spending per Person", "HAQI" = "Healthcare Access and Quality Index"))

#Making Facet Plot dataframe
target_locations <- c("Chile", "Argentina", "Uruguay")
df_Target <- df %>% filter(location %in% target_locations) %>% select(cpi,dah_per_cap_ppp_mean,ghes_per_the_mean,haqi,imm_pop_perc,perc_skill_attend,perc_urban,result,sdi,the_per_cap_mean,location,year)
  
#Change to long format
df_Target_long <- df_Target %>% pivot_longer(1:10, names_to = "Component", values_to = "Value")

#Make Facet Plot
ggplot(df_Target_long,aes(year,Value, group = Component, color = Component))+
  geom_line(size = 2)+
  theme_minimal(base_size = 10)+ 
 ggtitle("Average VIP Index Component Value 1990-2019 Southern Latin America")+
  ylab("Component Value")+
  xlab("Year")+
  theme(axis.text.x=element_blank())+
  scale_color_discrete(labels=c("cpi" = "Corruption Perception Index","dah_per_cap_ppp_mean" = "Development Assistance Per Person","ghes_per_the_mean" = "Government Health Spending per Total Health Spending", "haqi" = "HAQI", "imm_pop_perc"= "Immigrant Population (%)", "perc_skill_attend" = "Skilled Attendants at Birth", "perc_urban" = "Urbanicity (%)", "result" = "VIP Index", "sdi" = "Socio-demographic Index", "the_per_cap_mean" = "Total Health Spending per Person"))+
  facet_wrap( ~ location)




