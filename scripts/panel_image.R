library(tidyverse)
library(patchwork)

######### both  ##########
mod4_health<-read.csv('data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv')
mod4_health$mod='Health'
mod4_health$data='both'

mod1_bio<-read.csv('data/model_selection_tables/adj_mod1_mh_both_ptable_linear.csv')
mod1_bio$mod='Biodiversity'
mod1_bio$data='both'

mod3_socio<-read.csv('data/model_selection_tables/adj_mod3_mh_both_ptable_linear.csv')
mod3_socio$mod='Socio-demographic'
mod3_socio$data='both'

######### low marginalization ##########
mod4_low<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_high_ptable_linear.csv')
mod4_low$mod='Health'
mod4_low$data='low'

mod1_low<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_high_ptable_linear.csv')
mod1_low$mod='Biodiversity'
mod1_low$data='low'

mod3_low<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_high_ptable_linear.csv')
mod3_low$mod='Socio-demographic'
mod3_low$data = "low"

######## high marginalization ##########

mod4_high<-read.csv('data/model_selection_table_28Jul23/adj_mod4_mh_low_ptable_linear.csv')
mod4_high$mod='Health'
mod4_high$data='high'

mod1_high<-read.csv('data/model_selection_table_28Jul23/adj_mod1_mh_low_ptable_linear.csv')
mod1_high$mod='Biodiversity'
mod1_high$data = 'high'


mod3_high<-read.csv('data/model_selection_table_28Jul23/adj_mod3_mh_low_ptable_linear.csv')
mod3_high$mod='Socio-demographic'
mod3_high$data = 'high'

###### put them together ####

ggplot(data=data.frame( x=c(-1,2),y=c(-1,2) ), aes(x=x,y=y)) + 
            geom_point(shape = 1) +
            geom_abline(intercept = 1, slope = -1, col = "red") +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0)

############# logit ##########

x<-seq(-3,3, length.out=200)
logity<-(-2.20536123+(-0.10813589)*x)

x<-seq(-3,3, length.out=200)

logitify <- function(int,slope,x = seq(-3,3, length.out=200)) {
            z<-(int+(slope)*x)
            return(exp(z)/(1+exp(z)))
}

plot(logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]
             ,mod4_low$Estimate[which(mod4_low$X=='treerich')]))


plot(logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]+
                          mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
              ,mod4_low$Estimate[which(mod4_low$X=='treerich')] +
                          mod4_low$Std..Error[which(mod4_low$X=='treerich')]))



plot(logitify(mod4_low$Estimate[which(mod4_low$X=='(Intercept)')]-
                          mod4_low$Std..Error[which(mod4_low$X=='(Intercept)')]
              ,mod4_low$Estimate[which(mod4_low$X=='treerich')]-
                          mod4_low$Std..Error[which(mod4_low$X=='treerich')]))


# x-axis # of st. deviations




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






