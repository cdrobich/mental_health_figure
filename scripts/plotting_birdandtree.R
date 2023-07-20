
# Colours etc.

#Biodiversity - Trees, NDVI, Birds, Green space, Blue space 
"#386641"

#Health behaviours - fruit & veg consumption, smoking, smoking cessation, alcohol, physical activity
"#540b0e"

#Socio demographics - education, income, marital status, employment, sex, age, race, time since immigration, born in canada
"#003566" 



## plotting script

mod4t<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4t$mod='4'

mod1t<-read.csv('data/model_selection_tables/adj_mod1_mh_both_ptable_linear.csv')
mod1t$mod='1'

mod2t<-read.csv('data/model_selection_tables/adj_mod2_mh_both_ptable_linear.csv')
mod2t$mod='2'

mod3t<-read.csv('data/model_selection_tables/adj_mod3_mh_both_ptable_linear.csv')
mod3t$mod='3'

library(dplyr)

library(ggplot2)

modst<-bind_rows(mod1t,mod2t,mod3t,mod4t)
modst$X<-recode(modst$X, `(Intercept)`='Intercept', 
               eexp="Weekly activity time",  
               SMKC_102='Has not quit smoking',
               SMKC_104='Unknown smoking cessation status',  
               SMKC_106='Never smoked', SMKC_2022='Occasional smoker', 
               SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  
               ALCEDWKY='Weekly alcohol consumption',
               FVCDVTOT='Daily fruit and vegetable consumption', 
               married2='Common-law', married3='Never Married',
               married4='Separated',married5='Divorced',married6='Widowed', 
               married7='Unknown marital status', job1='Employed',
               job2='Unknown employment status',white1='White', 
               white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', 
               imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',
               INCDHH='Household income',EHG2DVR32='High school education',
               EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')


mod4t$X<-recode(mod4t$X, `(Intercept)`='Intercept',
                eexp="Weekly activity time", 
               SMKC_102='Has not quit smoking',
               SMKC_103='Unknown smoking cessation status',  
               SMKC_106='Never smoked', SMKC_2022='Occasional smoker',
               SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  
               ALCEDWKY='Weekly alcohol consumption',
               FVCDVTOT='Daily fruit and vegetable consumption', 
               married2='Common-law', married3='Never Married',
               married4='Separated',married5='Divorced',
               married6='Widowed', married7='Unknown marital status', 
               job1='Employed',job2='Unknown employment status',
               white1='White', white2='Unknown ethnicity',
               imi2='Non-immigrant (non-white)', 
               imi3='Immigrant (White, <10 years)',
               imi7='Unknown immigration status',
               INCDHH='Household income',
               EHG2DVR32='High school education',
               EHG2DVR33='Post-secondary education',
               EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', 
               DHH_AGE='Age', treerich='Tree species richness',
               treediv='Tree Shannon diversity', 
               DistancetoLocation='Distance to nearest ebird hotspot', 
               ModeledSDiv='Modeled bird Shannon diversity',
               ModeledSRich='Modeled bird species richness', 
               dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',
               dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity', 
               ndvi='Greenness in postalcode (NDVI)',  
               ndvi500='Greenness within 500m buffer (NDVI)', 
               ndvi1000='Greenness within 1000m buffer (NDVI)',
               YEAR='Year',bluedist='Distance to blue space', 
               greendist='Distance to green space', 
               PropBlue="Proportion of blue space", 
               PropGreen='Proportion of green space',
               area_m='Postal code area')



unique(mod4t$X)
mod4t
#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

labels <- c("NA" = "darkgrey",
            "Health" = "#ca562c",
            "Socio-demographic" = "#481a6c",
            "Biodiversity" = "#6a994e")

library(glue)
library(ggtext)

mod4t <- mod4t %>% filter(X != "Immigrant (White, <10 years)")

mod4t$variable <- mod4t$X
            
mod4t$variable <- recode(mod4t$variable, 
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

or_MH_plot <- ggplot(data=mod4t, 
                   aes(x=Estimate, y=reorder(X, Estimate),
                       xmin=Estimate-Std..Error,
                       xmax=Estimate+Std..Error,
                       fill=Pr...z..<0.05,
                       shape=Pr...z..<0.05,
                       colour = variable))+
                        geom_errorbar(lwd = 1)+
                        geom_point(size = 3, stroke = 1.5)+
                        scale_x_continuous(limits=c(-2,2), 
                               breaks=c(log(0.01),
                                        log(0.1),log(0.2), 
                                        log(0.5),0,log(2),log(5), 
                                        log(10),log(100)),
                               labels=c(0.01,0.1,0.2,0.5,1,2,5,10,100))+
                        theme_light()+
                        geom_vline(xintercept=0, colour='black', 
                                   linetype='dashed',
                                   lwd = 1,
                                   alpha = 0.45)+
                        scale_fill_manual(values = c("#003052", "lightgrey"))+
                        scale_shape_manual(values = c(22,23)) +
                        scale_colour_manual(values = labels) +
                        xlab('Odds ratio')+
            ggtitle("Poor mental health")+
                        ylab(NULL)+
                        geom_hline(yintercept=1, colour='lightgrey')+
                        theme(axis.title=element_text(size=12),
                              axis.text=element_text(size=12),
                              legend.position = "none") 
                        # guides(fill = guide_legend(title = "P < 0.05"),
                        #        shape = guide_legend(title = "P < 0.05"),
                        #        colour = guide_legend(title = "Category"))

ggsave("output/MH_oddsratio.jpg")

library(viridis)
bird<-subset(mods, X=="Modeled bird Shannon diversity")
ggplot(data=bird, 
       aes(y=Estimate, x=mod, ymin=Estimate-Std..Error,
           ymax=Estimate+Std..Error,
           colour=mod))+
            geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, colour='darkred', linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for poor mental health')+
            xlab('Model adjustment phase')+
            theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.12,0.0001),
                               breaks=c(log(0.9),log(0.92),log(0.94),
                                        log(0.96),log(0.98),0),
                               labels=c(0.9,0.92,0.94,0.96,0.98,1))+
            ggtitle("Modeled bird Shannon diversity")+
            geom_point()+
            theme(axis.title=element_text(size=16),
                  axis.text=element_text(size=14),
                  title=element_text(size=16))

ggsave('adjustment_mh_both_birds.pdf')

tree<-subset(mods, X=="Tree species richness")
ggplot(data=tree,
       aes(y=Estimate, x=mod,
           ymin=Estimate-Std..Error,
           ymax=Estimate+Std..Error,
           colour=mod))+geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, colour='darkred', linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for poor mental health')+
            xlab('Model adjustment phase')+
            theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.18,0.0001), 
                               breaks=c(log(0.8),log(0.85),
                                        log(0.90), log(0.95),0),
                               labels=c(0.8, 0.85,0.9,0.95,1))+
            ggtitle("Tree species richness")+
            geom_point()+
            theme(axis.title=element_text(size=16),
                  axis.text=element_text(size=14),
                  title=element_text(size=16))



ggsave('adjustment_mh_both_trees.pdf')


blue<-subset(mods, X=="Distance to blue space")
ggplot(data=blue, 
       aes(y=Estimate, x=mod, ymin=Estimate-Std..Error,
           ymax=Estimate+Std..Error, colour=mod))+
            geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, 
                       colour='darkred', 
                       linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for poor mental health')+
            xlab('Model adjustment phase')+
            theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.01,0.15), 
                               breaks=c(0, log(1.05), 
                                        log(1.1), log(1.15),
                                        log(1.2)),
                               labels=c(1,1.05, 1.10, 1.15, 1.2))+
            ggtitle("Distance to blue space")+
            geom_point()+
            theme(axis.title=element_text(size=16),
                  axis.text=element_text(size=14),
                  title=element_text(size=16))

ggsave('adjustment_mh_both_dblue.pdf')

########## Self-reported Stress

mod4i<-read.csv('data/model_selection_tables/adj_mod4_stress_both_ptable_mice_linear.csv')
mod4i$mod='4'
mod1i<-read.csv('data/model_selection_tables/adj_mod1_stress_both_ptable_linear.csv')
mod1i$mod='1'
mod2i<-read.csv('data/model_selection_tables/adj_mod2_stress_both_ptable_linear.csv')
mod2i$mod='2'
mod3i<-read.csv('data/model_selection_tables/adj_mod3_stress_both_ptable_linear.csv')
mod3i$mod='3'


modsi<-bind_rows(mod1i,mod2i,mod3i,mod4i)

modsi$X<-recode(modsi$X, `(Intercept)`='Intercept', eexp="Weekly activity time",  SMKC_102='Has not quit smoking',SMKC_104='Unknown smoking cessation status',  SMKC_106='Never smoked', SMKC_2022='Occasional smoker', SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  ALCEDWKY='Weekly alcohol consumption',FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married',married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status',white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',INCDHH='Household income',EHG2DVR32='High school education',EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')
mod4i$X<-recode(mod4i$X, `(Intercept)`='Intercept', eexp="Weekly activity time",  SMKC_102='Has not quit smoking',SMKC_103='Unknown smoking cessation status',  SMKC_106='Never smoked', SMKC_2022='Occasional smoker', SMKC_2023='Non-smoker', SMKC_2024='Unknown smoking frequency',  ALCEDWKY='Weekly alcohol consumption',FVCDVTOT='Daily fruit and vegetable consumption', married2='Common-law', married3='Never Married',married4='Separated',married5='Divorced',married6='Widowed', married7='Unknown marital status', job1='Employed',job2='Unknown employment status',white1='White', white2='Unknown ethnicity',imi2='Non-immigrant (non-white)', imi3='Immigrant (White, <10 years)',imi7='Unknown immigration status',INCDHH='Household income',EHG2DVR32='High school education',EHG2DVR33='Post-secondary education',EHG2DVR34='Unknown Education status',DHHE_SEX2='Female', DHH_AGE='Age', treerich='Tree species richness',treediv='Tree Shannon diversity', DistancetoLocation='Distance to nearest ebird hotspot', ModeledSDiv='Modeled bird Shannon diversity',ModeledSRich='Modeled bird species richness', dist_ChaoEstimatedSpRich='Chao-estimated bird species richness',dist_ChaoEstimatedSpDiv='Chao-estimated bird Shannon diversity',  ndvi='Greenness in postalcode (NDVI)',  ndvi500='Greenness within 500m buffer (NDVI)', ndvi1000='Greenness within 1000m buffer (NDVI)',YEAR='Year',bluedist='Distance to blue space', greendist='Distance to green space', PropBlue="Proportion of blue space", PropGreen='Proportion of green space',area_m='Postal code area')

# removed white Immigrant, <10 years because the error bars were massive
mod4i <- mod4i %>% filter(X != "Immigrant (White, <10 years)")

mod4i$variable <- mod4i$X
mod4i$variable <- recode(mod4t$variable, 
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


unique(mod4i$variable)

or_stress_plot <- ggplot(data=mod4i, 
       aes(x=Estimate, y=reorder(X, Estimate), 
           xmin=Estimate-Std..Error,
           xmax=Estimate+Std..Error, 
           fill=Pr...z..<0.05,
           shape=Pr...z..<0.05,
           colour = variable))+
            theme_light()+
            geom_errorbar(lwd = 1) +
            geom_point(size = 3, stroke = 1.5) +
             scale_x_continuous(limits=c(-2,2), 
                               breaks=c(log(0.01),log(0.1),log(0.2), 
                                        log(0.5),0,log(2),log(5), 
                                        log(10)),
                                labels=c(0.01,0.1,0.2,0.5,1,2,5,10))+
            geom_vline(xintercept=0, colour='black', 
                       linetype='dashed',
                       lwd = 1,
                       alpha = 0.45)+
            scale_fill_manual(values = c("black", "lightgrey"))+
            scale_shape_manual(values = c(22,23)) +
            scale_colour_manual(values = labels) +
            xlab('Odds ratio')+
            ggtitle('High perceived life stress') +
            ylab(NULL)+
            geom_hline(yintercept = 39, colour='lightgrey' )+
            theme(axis.title=element_text(size=14),
                               axis.text=element_text(size=12)) +
            theme(axis.title=element_text(size=12),
                  axis.text=element_text(size=12)) +
            guides(fill = guide_legend(title = "P < 0.05"),
                   shape = guide_legend(title = "P < 0.05"),
                   colour = guide_legend(title = "Category"))

ggsave("output/OR_stress.jpg")
   

library(patchwork)

odds_ratio_plot <- or_MH_plot + or_stress_plot + 
            plot_annotation(tag_levels = c("A"))
            
ggsave("output/MH_stress_OR_plot.tiff")

library(viridis)
mods$X[which(mods$X=="Modeled bird Shannon diversity")]<-"Modeled bird species richness"
bird<-subset(mods, X=="Modeled bird species richness")
ggplot(data=bird, aes(y=Estimate, x=mod, 
                      ymin=Estimate-Std..Error, 
                      ymax=Estimate+Std..Error, colour=mod))+
            geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, colour='darkred',
                       linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for high perceived life stress')+
            xlab('Model adjustment phase')+
            theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.04,0.015), 
                               breaks=c(log(0.96),log(0.97),
                                        log(0.98),log(0.99),0, 
                                        log(1.01), log(1.02)),
                               labels=c(0.96,0.97,0.98,0.99,1, 1.01,1.02))+
            ggtitle("Modeled bird species richness")+
            geom_point()+
            theme(axis.title=element_text(size=16), 
                  axis.text=element_text(size=14),
                  title=element_text(size=16))



#ggsave('adjustment_stress_both_birds.pdf')

tree<-subset(mods, X=="Tree Shannon diversity")
ggplot(data=tree,
       aes(y=Estimate, x=mod, 
           ymin=Estimate-Std..Error, ymax=Estimate+Std..Error, colour=mod))+
            geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, colour='darkred',
                       linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for high perceived life stress')+
            xlab('Model adjustment phase')+
            theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.01,0.03), 
                               breaks=c(log(0.99),0, 
                                        log(1.01), log(1.02), log(1.03)),
                               labels=c(0.99,1,1.01,1.02,1.03))+
            ggtitle("Tree Shannon diversity")+
            geom_point()+
            theme(axis.title=element_text(size=16), 
                  axis.text=element_text(size=14),
                  title=element_text(size=16))

#ggsave('adjustment_stress_both_trees.pdf')


green<-subset(mods, X=="Greenness in postalcode (NDVI)")

ggplot(data=green,
       aes(y=Estimate, 
           x=mod, ymin=Estimate-Std..Error, 
           ymax=Estimate+Std..Error, colour=mod))+
            geom_errorbar()+
            theme_classic()+
            geom_hline(yintercept=0, colour='darkred', 
                       linetype='dashed')+
            scale_colour_viridis_d()+
            ylab('Odds ratio for high perceived life stress')+
            xlab('Model adjustment phase')+theme(legend.position = 'none')+
            scale_y_continuous(limits=c(-0.01,0.135), 
                               breaks=c(0, log(1.05), log(1.1), 
                                        log(1.15), log(1.2)),
                               labels=c(1,1.05,1.10,1.15,1.2))+
            ggtitle("Greenness in postalcode (NDVI)")+
            geom_point()+
            theme(axis.title=element_text(size=16), 
                  axis.text=element_text(size=14),
                  title=element_text(size=16))

#ggsave('adjustment_stress_both_green.pdf')

                                                                