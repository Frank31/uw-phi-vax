# Name: Final prep for analysis
# Author: Francisco Rios
# Date: July 12, 2022

rm(list=ls())

source(paste0("C:/Users/frc2/Documents/uw-phi-vax/global_vac_index/aim_2/third_version/01_set_up_R.R"))

# Load prepped data
dt <- as.data.table(read_rds(paste0(prepped_data_dir, "aim_2/17_merged_dataset_third_version.RDS")))

# subset data to specific time frame
data <- dt %>% filter(between(year, 1990, 2020))

# divide certain variables by 100 to ensure they range between 0 and 1 only
rescaleVars = c('haqi', 'perc_skill_attend', 'cpi')

rescaleTransform = function(x) {
  x/100
}

for (v in rescaleVars) {
  data[, (v):=rescaleTransform(get(v))]
}

# create complement variables
complVars = c('cpi', 'imm_pop_perc')

complTransform = function(x) {
  1-x
}

for (v in complVars) {
  data[, (v):=complTransform(get(v))]
}

# normalize the variables by adding 1 and then taking log transformation (to avoid values of 0)
NormVars = c("dah_per_cap_ppp_mean", "the_per_cap_mean")

NormalTransf = function(x) {
  log(x+1)
}

for (v in NormVars) {
  data[, (v):=NormalTransf(get(v))]
}

# normalize left-skewed variables by taking the cubed root
NormCubVars = c("perc_skill_attend", "imm_pop_perc")

CubeTransf = function(x) {
  (x)^3
}

for (v in NormCubVars) {
  data[, (v):=CubeTransf(get(v))]
}

# extrapolate where necessary using GLM for variables that range between 0 and 1
percentVars <-  c("sdi", "haqi", "ghes_per_the_mean",
                  "perc_skill_attend", "imm_pop_perc", "perc_urban", "gov_trust")

# check to see which countries have enough values to extrapolate government trust
original_data <- as.data.table(read_rds(paste0(prepped_data_dir, "aim_2/17_merged_dataset_third_version.RDS")))
enough_data <- original_data[,.(sum_responses=sum(!is.na(gov_trust), na.rm=TRUE)),
                             by=c("iso_code", "location", "gbd_location_id",
                                  "iso_num_code")] %>% filter(sum_responses>=2)

locs_with_trust_data <- unique(enough_data$location)

i<-1
pltlist <- list()

for (v in percentVars) {
  
  if (v=="gov_trust") {
    locations <- unique(locs_with_trust_data)
    } else {
    locations <- unique(data$location)
    }
   
  for (h in locations){
      
    # i=i+1
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = glm(form, data[location==h], family='binomial')
    data[location==h, tmp:=(predict(lmFit, newdata=data[location==h], type="response"))]
    
    # don't extrapolate beyond observed values for skilled attendants
    if (v=="perc_skill_attend"){
      maxlim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
      minlim = min(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
      data[location==h & tmp>maxlim, tmp:=maxlim]
      data[location==h & tmp<minlim, tmp:=minlim]
    }
    
    # lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
    # data[location==h & tmp>lim, tmp:=lim]
    
    pltlist[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title = paste0(h))
    data[location==h & is.na(get(v)), (v):=tmp]
    i=i+1
    
    # pct_complete = floor(i/(length(percentVars)*length(unique(data$location)))*100)
    
    # cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}
  

outputFile10 <- paste0(visDir, "aim_2/third_version/01_percent_data_extrapolated_with_logistic_regression.pdf")
pdf(outputFile10, height=5.5, width=9)

for(i in seq(length(pltlist))) {
  print(pltlist[[i]])
}

dev.off()

# # extrapolate government trust data for countries with adequate data 
# specialVars = c("gov_trust")
# 
# 
# 
# i<-1
# pltlistb <- list()
# rm(h)

# for (v in specialVars) {
#   for(h in unique(locs_with_trust_data)) {
#     # i=i+1
#     if (!any(is.na(data[location==h][[v]]))) next
#     if (!any(!is.na(data[location==h][[v]]))) next
#     form = as.formula(paste0(v,'~year'))
#     lmFit = glm(form, data[location==h], family='binomial')
#     data[location==h, tmp:=(predict(lmFit, newdata=data[location==h], type="response"))]
#     
#     # lim = max(data[location==h][[v]], na.rm=T)+sd(data[location==h][[v]], na.rm=T)
#     # data[location==h & tmp>lim, tmp:=lim]
#     
#     # pltlistb[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point() + geom_point(aes(y=tmp),color='red') + labs(title = paste0(h))
#     # data[location==h & is.na(get(v)), (v):=tmp]
#     # i=i+1
#     
#     # pct_complete = floor(i/(length(percentVars)*length(unique(data$location)))*100)
#     
#     # cat(paste0('\r', pct_complete, '% Complete'))
#     flush.console()
#   }
# }
# 
# data$tmp = NULL

# outputFile10a <- paste0(visDir, "aim_2/third_version/01b_gov_trust_data_extrapolated_with_logistic_regression.pdf")
# pdf(outputFile10a, height = 5.5, width=9)
# 
# # print the gov trust variable
# for(i in seq(length(pltlistb))) {
#   print(pltlistb[[i]])
# }
# 
# dev.off()

# extrapolate where necessary using GLM for numeric variables
monetaryVars <- c("the_per_cap_mean", "dah_per_cap_ppp_mean")

i=1
pltlist2 <- list()
for(v in monetaryVars) {
  for(h in unique(data$location)) {
    
    if (!any(is.na(data[location==h][[v]]))) next
    if (!any(!is.na(data[location==h][[v]]))) next
    form = as.formula(paste0(v,'~year'))
    lmFit = lm(form, data[location==h])
    data[location==h, tmp:=(predict.lm(lmFit, newdata=data[location==h]))]
    lim = min(data[location==h][[v]], na.rm=T)
    data[location==h & tmp<0, tmp:=lim]
    pltlist2[[i]] <- ggplot(data[location==h], aes_string(y=v, x='year')) + geom_point()+ labs(title = paste0(h)) + geom_point(aes(y=tmp),color='red') 
    data[location==h & is.na(get(v)), (v):=tmp]
    
    i=i+1
    # pct_complete = floor(i/(length(percentVars)*length(unique(data$location)))*100)
    # cat(paste0('\r', pct_complete, '% Complete'))
    flush.console()
  }
}
data$tmp = NULL

outputFile10_b <- paste0(visDir, "aim_2/third_version/02_monetary_data_extrapolated_with_linear_regression.pdf")
pdf(outputFile10_b, height=5.5, width=9)

for(i in seq(length(pltlist2))) {
  print(pltlist2[[i]])
}
dev.off()

# add constant of 1 to variables to avoid zero values
ConstVars = c("dah_per_cap_ppp_mean", "imm_pop_perc", "perc_urban")

addConstant = function(x) {
  x+1
}

for (v in ConstVars) {
  data[, (v):=addConstant(get(v))]
}

# Drop variables that have too much missing values (namely locations without any estimates for specific variables)
# prepped_data <- na.omit(data)

# re arrange variables and save final
prepped_data <- data %>% select(location, year, gbd_location_id, iso_code, iso_num_code, region, 
                                        dah_eligible,  sdi, the_per_cap_mean, ghes_per_the_mean, 
                                        dah_per_cap_ppp_mean, haqi, cpi, perc_skill_attend, imm_pop_perc, perc_urban,
                                        mean_agree_vac_safe, mean_agree_vac_important, mean_agree_vac_effective, gov_trust)

# Save the prepped data set for analysis
saveRDS(prepped_data, file = paste0(prepped_data_dir, "aim_2/18_prepped_data_for_analysis_third_version.RDS"))