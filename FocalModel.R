library(dplyr)

rm(list=ls())
library(readxl)
library(Hmisc) 
#load data
multdata <- read_excel("Multimedia.xlsx")

#obtain summary stats for the data
summary(multdata)
colnames(multdata)

#Generate the Variables of interest
Sales<-multdata$`Sales (units)`
Lag_Sales<-Lag(Sales,shift=1) #creating the lag of sales value
Catalogs_ExistCust<-multdata$`Catalogs_ExistCust`
Catalogs_Winback<-multdata$`Catalogs_Winback`
Catalogs_NewCust<-multdata$`Catalogs_NewCust`
Mailings<-multdata$`Mailings`
Banner<-multdata$`Banner`
Search<-multdata$`Search`
SocialMedia<-multdata$`SocialMedia`
Newsletter<-multdata$`Newsletter`
Retargeting<-multdata$`Retargeting`
Portals<-multdata$`Portals`
ADV_Total <- multdata$`ADV_Total`
ADV_Offline <- multdata$`ADV_Offline`
ADV_Offline <- multdata$`Catalogs_Winback`


#Diminishing Returns
Sq_Catalogs_ExistCust=sqrt(Catalogs_ExistCust)
Sq_Catalogs_Winback=sqrt(Catalogs_Winback)
Sq_Catalogs_NewCust=sqrt(Catalogs_NewCust)
Sq_Mailings=sqrt(Mailings)
Sq_Banner=sqrt(Banner)
Sq_Search=sqrt(Search)
Sq_SocialMedia=sqrt(SocialMedia)
Sq_Newsletter=sqrt(Newsletter)
Sq_Retargeting=sqrt(Retargeting)
Sq_Portals=sqrt(Portals)
Sq_ADV_Total <- sqrt(ADV_Total)
Sq_ADV_Offline <- sqrt(ADV_Offline)
Sq_ADV_Offline <- sqrt(Catalogs_Winback)



### Focal Model
regmod4<-lm(Sales~Lag_Sales+Sq_Catalogs_ExistCust+Sq_Catalogs_Winback+Sq_Catalogs_NewCust+Sq_Newsletter+Sq_Portals)
summary(regmod4)
AIC(regmod4)
BIC(regmod4)

beta_y <- 0.1563
beta_ExistCust <- -23.9232
beta_Winback <- 54.3291
beta_NewCust <- -27.3190
beta_Newsletter <- 164.7168
beta_Portals <- 806.9819
lambda <- 0.1563


elasticity_ExistCust <-  beta_ExistCust*(1/2)*(sqrt(mean(na.omit(Catalogs_ExistCust))))/((mean(Sales))*(1-lambda))

elasticity_Winback <-  beta_Winback*(1/2)*(sqrt(mean(na.omit(Catalogs_Winback))))/((mean(Sales))*(1-lambda))

elasticity_NewCust <-  beta_NewCust*(1/2)*(sqrt(mean(na.omit(Catalogs_NewCust))))/((mean(Sales))*(1-lambda))

elasticity_Newsletter <-  beta_Newsletter*(1/2)*(sqrt(mean(na.omit(Newsletter))))/((mean(Sales))*(1-lambda))

elasticity_Portals <-  beta_Portals*(1/2)*(sqrt(mean(na.omit(Portals))))/((mean(Sales))*(1-lambda))



allocation_ExistCust <- elasticity_ExistCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Winback <- elasticity_Winback/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_NewCust <- elasticity_NewCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Newsletter <- elasticity_Newsletter/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Portals <- elasticity_Portals/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)


sum(allocation_ExistCust,allocation_Winback,allocation_NewCust,allocation_Newsletter,allocation_Portals)
