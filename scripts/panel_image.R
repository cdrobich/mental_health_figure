library(tidyverse)
library(patchwork)

######### both  ##########
mod4_both<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4_both$data='both'


######### low marginalization ##########
mod4_low<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_high_ptable_linear.csv')
mod4_low$data='low'


######## high marginalization ##########

mod4_high<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_low_ptable_linear.csv')
mod4_high$data='high'


###### put them together ####

mods_together_data <- bind_rows(mod4_both,mod4_low,mod4_high)


### Recode #####
mods_together_data $X <- recode(mods_together_data $X, `(Intercept)`='Intercept',eexp="Weekly activity time",SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',SMKC_106='Never smoked', SMKC_2022='Occasional smoker',SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency', ALCEDWKY='Weekly alcohol consumption',
                          FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married', married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status', white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
                          imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status', INCDHH='Household income', EHG2DVR32='High school education', EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female',DHH_AGE='Age', treerich='Tree species richness', treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity', ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', ndvi='Greenness in postalcode (NDVI)',ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space',
                          greendist='Distance to green space',PropBlue="Proportion of blue space", PropGreen='Proportion of green space', area_m='Postal code area')

mods_together_data$group <- mods_together_data$X

mods_together_data$group <- recode(mods_together_data$group, 
                              "Intercept" = "NA",
                              "Weekly activity time" = "Health",
                              "Has not quit smoking" ="Health",                 
                              "Unknown smoking cessation status" = "Health",
                              "Never smoked" = "Health",
                              "Occasional smoker" = "Health",
                              "Unknown smoking frequency" = "Health",
                              "Weekly alcohol consumption" = "Health",
                              "Daily fruit and vegetable consumption" = "Health",
                              "Common-law" = "Socio-demographic" ,
                              "Never Married" = "Socio-demographic",
                              "Separated" = "Socio-demographic",
                              "Divorced" = "Socio-demographic",
                              "Widowed" = "Socio-demographic",
                              "Unknown marital status" = "Socio-demographic",
                              "Employed" = "Socio-demographic",
                              "Unknown employment status" = "Socio-demographic",           
                              "White" = "Socio-demographic",
                              "Unknown ethnicity" = "Socio-demographic",
                              "Non-immigrant (non-white)" = "Socio-demographic",            
                              "Unknown immigration status" = "Socio-demographic",
                              "Household income" = "Socio-demographic",
                              "High school education" = "Socio-demographic",                
                              "Post-secondary education" = "Socio-demographic",
                              "Unknown Education status" = "Socio-demographic",
                              "Female" = "Socio-demographic",                               
                              "Age" = "Socio-demographic",
                              "Tree species richness" = "Biodiversity",
                              "Distance to nearest ebird hotspot" = "Biodiversity",    
                              "Modeled bird Shannon diversity" = "Biodiversity",
                              "Greenness within 500m buffer (NDVI)" = "Biodiversity",
                              "Year" = "NA",                                
                              "Distance to blue space" = "Biodiversity",
                              "Distance to green space" = "Biodiversity",
                              "Proportion of blue space" = "Biodiversity",             
                              "Proportion of green space" = "Biodiversity",
                              "Postal code area" = "Socio-demographic",
                              "Non-smoker" = "Health")


mods_together_data  <- rename(mods_together_data ,
                        variable = X,
                        P = Pr...z..,
                        std.error = Std..Error)


mods_together_data <- mods_together_data %>% 
            filter(variable != "Immigrant (White, <10 years)")


mods_bd <- mods_together_data %>% 
            filter(group == "Biodiversity")


write.csv( mods_bd, 'data/mods_biodiversity_28Jul23.csv')
 


ggplot(data=data.frame( x=c(-1,2),y=c(-1,2) ), aes(x=x,y=y)) + 
            geom_point(shape = 1) +
            geom_abline(intercept = 1, slope = -1, col = "red") +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0)


#Probability of poor outcome=logit(intercept+slope*variable of interest)

#Where Logit(z) =exp(z)/(1+exp(z))

colnames(mods_bd)
unique(mods_bd$variable)

treerich <- mods_bd %>% filter(variable == "Tree species richness")
ebird <- mods_bd %>% filter(variable == "Distance to nearest ebird hotspot")
birdiv <- mods_bd %>% filter(variable == "Modeled bird Shannon diversity")
green500 <- mods_bd %>% filter(variable == "Greenness within 500m buffer (NDVI)")
bluedist <- mods_bd %>% filter(variable == "Distance to blue space")
greendist <- mods_bd %>% filter(variable == "Distance to green space")
blueprop <- mods_bd %>% filter(variable == "Proportion of blue space")
greenprop <- mods_bd %>% filter(variable == "Proportion of green space")






