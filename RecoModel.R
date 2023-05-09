library(dplyr)

rm(list=ls())
library(readxl)
library(Hmisc) 
options(scipen=999)
#load data
multdata <- read_excel("Multimedia.xlsx")

plot(multdata)

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

Log_Sales<-log(multdata$`Sales (units)`)
Lag_Log_Sales<-Lag(Log_Sales,shift=1) #creating the lag of sales value

#Diminishing Returns
Log_Catalogs_ExistCust=log(Catalogs_ExistCust+1)
Log_Catalogs_Winback=log(Catalogs_Winback+1)
Log_Catalogs_NewCust=log(Catalogs_NewCust+1)
Log_Mailings=log(Mailings+1)
Log_Banner=log(Banner+1)
Log_Search=log(Search+1)
Log_SocialMedia=log(SocialMedia+1)
Log_Newsletter=log(Newsletter+1)
Log_Retargeting=log(Retargeting+1)
Log_Portals=log(Portals+1)
Log_ADV_Total <- log(ADV_Total)
Log_ADV_Offline <- log(ADV_Offline)
Log_ADV_Offline <- sqrt(Catalogs_Winback)

Sales<-multdata$`Sales (units)`
lag_sales<-Lag(Sales,shift=1) #creating the lag of sales value
Catalog_Existing_Cust <-multdata$Catalogs_ExistCust
Catalogs_Winback <-  multdata$Catalogs_Winback
Catalogs_NewCust <-multdata$Catalogs_NewCust
Portals <- multdata$Portals
search <- multdata$Search
Newsletter <- multdata$Newsletter

#Diminishing Returns
sq_Catalog_Existing_Cust=sqrt(Catalog_Existing_Cust)
sq_Catalogs_Winback=sqrt(Catalogs_Winback)
sq_Catalogs_NewCust=sqrt(Catalogs_NewCust)
sq_Portals=sqrt(Portals)
sq_search=sqrt(search)
sq_newsletter = sqrt(Newsletter)

#Quadratic Functions
x2_lag_sales<-lag_sales^2
x2_Catalog_Existing_Cust <- Catalog_Existing_Cust^2
x2_Catalogs_Winback <-  Catalogs_Winback^2
x2_Catalogs_NewCust <- Catalogs_NewCust^2
x2_Portals <- Portals^2
x2_search <- search^2
x2_Newsletter <- Newsletter^2

x3_Catalogs_NewCust = Catalogs_NewCust^3

######## MIXED MODEL ###########
mix_model = lm(Sales~lag_sales+(Catalog_Existing_Cust+x2_Catalog_Existing_Cust)+(Catalogs_Winback+x2_Catalogs_Winback)
               +(Catalogs_NewCust+x2_Catalogs_NewCust)+Portals+(Newsletter+x2_Newsletter)+
                 (Catalogs_Winback*Newsletter)+(Catalog_Existing_Cust*Portals)+(Catalog_Existing_Cust*Catalogs_Winback)
               +(Catalogs_Winback*Portals))
summary(mix_model)
AIC(mix_model)
BIC(mix_model)


plot(mix_model)
plot(x = Catalog_Existing_Cust+x2_Catalog_Existing_Cust, y = Sales)
abline(mix_model, col = "red")
lines(lowess(Catalog_Existing_Cust+x2_Catalog_Existing_Cust, Sales),col = "blue")

lambda <- -0.1358559
beta_Catalog_Existing_Cust <- -6.1069735
beta_x2_Catalog_Existing_Cust <- 0.0025305
beta_Catalogs_Winback <- -2.3322462
beta_x2_Catalogs_Winback <- -0.0135679
beta_Catalogs_NewCust <- -0.5304783
beta_x2_Catalogs_NewCust <- -0.0005790
beta_Portals <- 13.7744472
beta_Newsletter <- 19.3165368
beta_x2_Newsletter <-  -0.3354991
beta_Catalogs_WinbackNewsletter <- 0.5984923
beta_Catalog_Existing_CustPortals <- 0.2993294
beta_Catalog_Existing_CustCatalogs_Winback <- 0.0009317
beta_Catalogs_WinbackPortals <- -0.8001835

##elasticity

elasticity_ExistCust <- (beta_Catalog_Existing_Cust + 
                               2*beta_x2_Catalog_Existing_Cust*mean(na.omit(Catalog_Existing_Cust)) + 
                               beta_Catalog_Existing_CustPortals*mean(na.omit(Portals)) + 
                               beta_Catalog_Existing_CustCatalogs_Winback*mean(na.omit(Catalogs_Winback)))*
                              (mean(na.omit(Catalog_Existing_Cust)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Winback <- (beta_Catalogs_Winback + 
                         2*beta_x2_Catalogs_Winback*mean(na.omit(Catalogs_Winback)) + 
                         beta_Catalogs_WinbackPortals*mean(na.omit(Portals)) +
                         beta_Catalogs_WinbackNewsletter*mean(na.omit(Newsletter)) + 
                         beta_Catalog_Existing_CustCatalogs_Winback*mean(na.omit(Catalog_Existing_Cust)))*
                        (mean(na.omit(Catalogs_Winback)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_NewCust <- (beta_Catalogs_NewCust + 
                         2*beta_x2_Catalogs_NewCust*mean(na.omit(Catalogs_NewCust)))*
                      (mean(na.omit(Catalogs_NewCust)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Newsletter <- (beta_Newsletter + 
                         2*beta_x2_Newsletter*mean(na.omit(Newsletter))+
                         beta_Catalogs_WinbackNewsletter*mean(na.omit(Catalogs_Winback)))*
                        (mean(na.omit(Newsletter)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Portals <- (beta_Portals + 
                         beta_Catalogs_WinbackPortals*mean(na.omit(Catalogs_Winback)) +
                         beta_Catalog_Existing_CustPortals*mean(na.omit(Catalog_Existing_Cust)))* 
                         (mean(na.omit(Portals)))/((1-lambda)*(mean(na.omit(Sales)))) 





##allocation 
allocation_ExistCust <- elasticity_ExistCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Winback <- elasticity_Winback/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_NewCust <- elasticity_NewCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Newsletter <- elasticity_Newsletter/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Portals <- elasticity_Portals/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)


sum(allocation_ExistCust,allocation_Winback,allocation_NewCust,allocation_Newsletter,allocation_Portals)

###absolute allocation
allocation_ExistCust <- abs(elasticity_ExistCust)/(abs(elasticity_ExistCust) + elasticity_Winback + abs(elasticity_NewCust) + elasticity_Newsletter + elasticity_Portals)

allocation_Winback <- elasticity_Winback/(abs(elasticity_ExistCust) + elasticity_Winback + abs(elasticity_NewCust) + elasticity_Newsletter + elasticity_Portals)

allocation_NewCust <- abs(elasticity_NewCust)/(abs(elasticity_ExistCust) + elasticity_Winback + abs(elasticity_NewCust) + elasticity_Newsletter + elasticity_Portals)

allocation_Newsletter <- elasticity_Newsletter/(abs(elasticity_ExistCust) + elasticity_Winback + abs(elasticity_NewCust) + elasticity_Newsletter + elasticity_Portals)

allocation_Portals <- elasticity_Portals/(abs(elasticity_ExistCust) + elasticity_Winback + abs(elasticity_NewCust) + elasticity_Newsletter + elasticity_Portals)


mix_model_without_synergy = lm(Sales~lag_sales+(Catalog_Existing_Cust+x2_Catalog_Existing_Cust)+(Catalogs_Winback+x2_Catalogs_Winback)
               +(Catalogs_NewCust+x2_Catalogs_NewCust)+Portals+(Newsletter+x2_Newsletter))
summary(mix_model_without_synergy)
AIC(mix_model_without_synergy)
BIC(mix_model_without_synergy)

lambda <- 0.1848562
beta_Catalog_Existing_Cust <- -3.3902921
beta_x2_Catalog_Existing_Cust <- 0.0024353
beta_Catalogs_Winback <- 8.4052391
beta_x2_Catalogs_Winback <- -0.0184074
beta_Catalogs_NewCust <- -1.9810836
beta_x2_Catalogs_NewCust <- 0.0010301
beta_Portals <- 144.0646054
beta_Newsletter <- 65.7464925
beta_x2_Newsletter <-  -1.1153456


elasticity_ExistCust <- (beta_Catalog_Existing_Cust + 
                           2*beta_x2_Catalog_Existing_Cust*mean(na.omit(Catalog_Existing_Cust)))*
  (mean(na.omit(Catalog_Existing_Cust)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Winback <- (beta_Catalogs_Winback + 
                         2*beta_x2_Catalogs_Winback*mean(na.omit(Catalogs_Winback)))*
  (mean(na.omit(Catalogs_Winback)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_NewCust <- (beta_Catalogs_NewCust + 
                         2*beta_x2_Catalogs_NewCust*mean(na.omit(Catalogs_NewCust)))*
  (mean(na.omit(Catalogs_NewCust)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Newsletter <- (beta_Newsletter + 
                            2*beta_x2_Newsletter*mean(na.omit(Newsletter)))*
  (mean(na.omit(Newsletter)))/((1-lambda)*(mean(na.omit(Sales)))) 

elasticity_Portals <- (beta_Portals)* (mean(na.omit(Portals)))/((1-lambda)*(mean(na.omit(Sales)))) 


allocation_ExistCust <- elasticity_ExistCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Winback <- elasticity_Winback/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_NewCust <- elasticity_NewCust/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Newsletter <- elasticity_Newsletter/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)

allocation_Portals <- elasticity_Portals/(elasticity_ExistCust + elasticity_Winback + elasticity_NewCust + elasticity_Newsletter + elasticity_Portals)


sum(allocation_ExistCust,allocation_Winback,allocation_NewCust,allocation_Newsletter,allocation_Portals)



                                                                                                                                                                                                                                                                         
                                                                                                                                                                                                                                                                                                               