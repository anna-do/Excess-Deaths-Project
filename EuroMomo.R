library(tidyverse)
library(readxl)

# Reading in total number of deaths by gender per week for 2013-2022
dat <- read_excel("Deaths EngWal by week and gender 2013-2022.xlsx")



# Table of population size for the full year. This is "N" in the formula
MPopSize <- c(29177340, 29215251)
FPopSize <- c(30420202, 29900558)
Year <- c(2021, 2018)
Population <- data.frame(Year, MPopSize, FPopSize)



# Fitting the model on five years of data (up to and includeing fit_year) for a particular gender.
# Pred year is the year we are predicting for 
fit_year <- 2017
gender_choice <- "Female"
pred_year <- 2018



# Filtering for number of deaths per week in Autumn and Spring for a specific gender
fitdat <- dat %>% filter(Gender == gender_choice,
               Year >= fit_year-4,
               Year <= fit_year,
               xor(between(`Week number`, 32, 44), between(`Week number`, 8, 20)))



# Finding Mu vals for different weeks
weeks <- c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44)
meandeaths <- c()

for (i in weeks){
  findmeandeaths <- filter(fitdat, `Week number` == i)
  mu <- mean(findmeandeaths$Deaths)
  meandeaths <- append(meandeaths, mu)
}

cbind(data.frame(weeks), data.frame(meandeaths)) # This is Mu in the formula

