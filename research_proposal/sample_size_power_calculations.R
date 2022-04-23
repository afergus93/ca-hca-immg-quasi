#load libraries
library(survey)
library(epiR)
library(tidyverse)
library(foreign)
library(haven)
library('DataCombine')
rm(list = ls())
#Set working directory (where you store downloaded data sets)
# setting working directory
setwd('/Users/santonis/Documents/epi_masters/spring_2022/EPI514/data/MAS_BRFSS_2011to2020_v2')
#data1 <- read_sav("data.sav")  
# import dta file in r
dataframe <- read_dta('MAS_2011_2020_v2.dta')
write.csv(dataframe, 'MAS_2011_2020_BRFSS_intermediate.csv')
brfss <- read.csv("MAS_2011_2020_BRFSS_intermediate.csv", stringsAsFactors = F)
#summary(dataframe)
#recoding variables as missing
brfss$medcost[brfss$medcost == 7] <- NA
brfss$medcost[brfss$medcost == 9] <- NA
#factoring medcost as 'yes' or 'no' based on codebook values
brfss$medcost <- factor(brfss$medcost, levels=c(1,2), labels=c("yes", "no"))
#subsetting BRFSS data to only be for years 2015-2019
brfss_subset <- subset(brfss, year >= 2015 &  year < 2020)
#dropping NAs from medcost variable
brfss_subset <- DropNA(brfss_subset, Var = "medcost", message = FALSE)

#BRFSS data stratified by year, dropping NAs for medcost variable to get sample size numbers per year
#2015 only
brfss_2015 <- subset(brfss, year == 2015)
brfss_2015 <- DropNA(brfss_2015, Var = "medcost", message = FALSE)
#2016 only
brfss_2016 <- subset(brfss, year == 2016)
brfss_2016 <- DropNA(brfss_2016, Var = "medcost", message = FALSE)
#2017 only
brfss_2017 <- subset(brfss, year == 2017)
brfss_2017 <- DropNA(brfss_2017, Var = "medcost", message = FALSE)
#2018 only
brfss_2018 <- subset(brfss, year == 2018)
brfss_2018 <- DropNA(brfss_2018, Var = "medcost", message = FALSE)
#2019 only
brfss_2019 <- subset(brfss, year == 2019)
brfss_2019 <- DropNA(brfss_2019, Var = "medcost", message = FALSE)


####### Use a loop to calculate minimum detectable PR #########
####### for different values of prevalence in unexposed #######

#generate a hypothetical sequence
# sequence goes from 9%-13.6% prevalence of marijuana use in unexposed (general population)
sequence <- seq(from = .09, to = .136, by = .01)

for(i in sequence){
  print(paste0("Prevalence of outcome in unexposed = ",i))
  output <- epi.ssxsectn(pdexp1 = NA, 
                         pdexp0 = i, #Prevalence of outcome in unexposed
                         n = 69557, #we have access to 69767 individuals, and 69557 accounts for missingness
                         power = .8, #Set to NA in order to have it calculate for you 
                         r = 6599/62958) #ratio of exposed to unexposed
  print(output$pr)
}

