# Install packages (if they aren't) -----------------------------------------------

install.packages("PNSIBGE")
install.packages("survey")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("foreign")
install.packages("forcats")
install.packages("tidyverse")
install.packages("data.table")

# Load required packages ----------------------------------------------

library(PNSIBGE)
library(survey)
library(ggplot2)
library(dplyr)
library(foreign)
library(forcats)
library(tidyverse)
library(data.table)


#presents PNSIBGE package
#help("get_pns")


# Import microdata -----------------------------------------------------

#we'll download PNS 2019 raw data using the PNSIBGE package 
#source: https://rpubs.com/gabriel-assuncao-ibge/pns
#https://github.com/Gabriel-Assuncao/PNSIBGE
#https://cran.r-project.org/web/packages/PNSIBGE/PNSIBGE.pdf

#if we want to download all variables
PNSdata <- get_pns(year=2019, #survey year (2013 or 2019)
                    selected = TRUE, #resident questionnaire
                    anthropometry = FALSE, #anthropometry questionnaire
                    labels = FALSE, #if we want labels for categories
                    design = FALSE) #if we want to import as a survey object (with the sample design) --> FALSE = don't import as survey


#OBS: if we want to download a few variables (i.e.: leisure physical activity)
#dadosPNS_test_raw <- get_pns(year=2019, vars=c("P034","P035", "P03701", "P03702", "P036"), design=FALSE)

#Selecting valid records and calculating sample weight - verification summary
PNSdata.1<-PNSdata %>% filter(V0025A==1) #selected for individual questionnaire
PNSdata.1<-PNSdata.1 %>% mutate(selected_resident_weight=((V00291*(94114/168426190)))) #sample weight of each respondent
PNSdata.1<-PNSdata.1 %>% filter(!is.na(selected_resident_weight)) #filter NA
summary(PNSdata.1$selected_resident_weight)


#### Scope definitions ####

#Sex
PNSdata.1 <- PNSdata.1 %>% rename(Sex=C006)
PNSdata.1$Sex<-factor(PNSdata.1$Sex, levels=c(1,2), labels=c("Male", "Female"))
summary(PNSdata.1$Sex)

#States
PNSdata.1 <- PNSdata.1 %>% rename(State=V0001)
PNSdata.1$State < -factor(PNSdata.1$State, levels=c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                                         label=c("Rondonia","Acre","Amazonas","Roraima","Para","Amapa","Tocantins","Maranhao","Piaui","Ceara",
                                                 "Rio Grande do Norte","Paraiba","Pernambuco","Alagoas","Sergipe","Bahia",
                                                 "Minas Gerais","Espirito Santo","Rio de Janeiro","Sao Paulo",
                                                 "Parana","Santa Catarina","Rio Grande do Sul", 
                                                 "Mato Grosso do Sul","Mato Grosso","Goias","Distrito Federal"))
summary(PNSdata.1$State)



#Age group
PNSdata.1 <- PNSdata.1 %>% mutate(Age_group=cut(C008,
                                                     breaks = c(18,20, 25, 50, 65, Inf),
                                                     labels = c("18 to 19 years", "20 to 24 years", "25 to 49 years", "50 to 64 years", "65 years or more"), 
                                                     ordered_result = TRUE, 
                                                     right = FALSE))
summary(PNSdata.1$age_group)


####TRANSFORMING IN SURVEY####

PNSdata.1_survey <-svydesign(id=~UPA_PNS, strat=~V0024, weight=~selected_resident_weight, nest=TRUE, data=PNSdata.1)
