Measles_Coverage <- c(24.5,37.66,50.82,63.98,77.14,90.3)


MBurden <- c(1425.78,1059.81,787.78,585.57,435.27,323.54)


Measles <- data.frame(Measles_Coverage, MBurden)

DTP_Coverage <- c(24,37,50,63,77,90)
DBurden <- c(96.6,55.6,31.81,18.02,10.02,5.39)

Diphtheria <- data.frame(DTP_Coverage,DBurden)

library(tidyverse)

ggplot(Measles, aes(Measles_Coverage,MBurden))+
  geom_point()+ ggtitle("Counter Factual Analysis of Measles Burden")+xlab("Measles Vaccine Coverage %")+ ylab("Measles Burden (DALYs rate per 100,000)")+theme_bw()


ggplot(Diphtheria, aes(DTP_Coverage,DBurden))+
  geom_point()+ ggtitle("Counter Factual Analysis of Diphtheria Burden")+xlab("Diphtheria Vaccine Coverage %")+ ylab("Diphthera Burden (DALYs rate per 100,000)")+theme_bw()
