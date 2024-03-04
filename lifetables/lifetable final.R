library(MortalityTables)
library(tidyverse)
library(readxl)
library(ggthemes)
library(fpp3)
library(readr)
library(janitor)
library(mgcv)


# This file contains:
#  - data setup/loading
#  - the function "iterate" which uses life tables to create a baseline without
#    seasonality
#  - Some simple testing of "iterate" (using it to predict deaths for the life-
#    table we used to fit the model).
#  - the function "LT" which uses the baseline from "iterate" and adds seasonality.
#  - Plotting



### Loading data #############################################################

popdat <- read.csv("Combined population data.csv")
fulltable <- read.csv("Combined life table.csv")
actualdeaths <- read.csv("Deaths combined.csv")
births <- read_xlsx("Births combined.xlsx")


# Going from 105 as the max age to 100 since lifetables stop at 100
for(country in c("England", "Wales", "Scotland")){
  for(i in 2013:2020){
    popdat[popdat$age == 100 & popdat$year == i & popdat$region == country, "female"] <-
      popdat[popdat$age == 100 & popdat$year == i & popdat$region == country, "female"] +
      popdat[popdat$age == 101 & popdat$year == i & popdat$region == country, "female"] +
      popdat[popdat$age == 102 & popdat$year == i & popdat$region == country, "female"] +
      popdat[popdat$age == 103 & popdat$year == i & popdat$region == country, "female"] +
      popdat[popdat$age == 104 & popdat$year == i & popdat$region == country, "female"] +
      popdat[popdat$age == 105 & popdat$year == i & popdat$region == country, "female"]
  }
  for(i in 2013:2020){
    popdat[popdat$age == 100 & popdat$year == i & popdat$region == country, "male"] <-
      popdat[popdat$age == 100 & popdat$year == i & popdat$region == country, "male"] +
      popdat[popdat$age == 101 & popdat$year == i & popdat$region == country, "male"] +
      popdat[popdat$age == 102 & popdat$year == i & popdat$region == country, "male"] +
      popdat[popdat$age == 103 & popdat$year == i & popdat$region == country, "male"] +
      popdat[popdat$age == 104 & popdat$year == i & popdat$region == country, "male"] +
      popdat[popdat$age == 105 & popdat$year == i & popdat$region == country, "male"]
  }
}
actualdeaths$X <- NULL
popdat <- popdat[-which(popdat$age >=101), ]
popdat[, "X"] <- NULL
fullpop <- popdat
popdat <- NULL

fulltable$X <- NULL
colnames(fulltable)[2:3] <- c("male", "female")
fulltable$region <- c(rep("Wales", times = 808), rep("Scotland", times = 808), rep("England", times = 808))



### Overview of how Iterate works ######################

# Prob is a vector of 101 probabilities. Each probability is the probability of 
# someone from that age category (0 to 100) surviving each week.

# Week 0 is the starting population.

# Survival is a dataframe where each row is an age group (from 0 to 100, then 100+)
# and each column is a week (from week 0 to n*52 where n is the number of years 
# we're iterating over)

# The week 5 column is the number of people surviving at the end of week 5
# To calculate this we use "prob" to kill off a proportion of the population from week 4
# ie we multiply is week 4 population by prob.
# Then 1/52 of people have a birthday and get bumped up an age group
# Then "birthrate" many babies are born
# People aging from 100 to 100+ go into row 102 and are no longer included in
# any calculations.

# The deaths are calculated right after we multiply the previous week by prob.
# Its the difference between the total population at the end of Week 4 and
# the total of what we've just calculated (all excluding the 100+ age category).

# The function outputs the week, year, sex and predicted deaths


### Variables for iterate ######

# Country: Either "England", "Wales" or "Scotland"
# PopStructure: The year which the starting population belongs to
# LifeTable: The lifetable we use to make predictions. 2017 = 2017-2019 life table
# BirthRateYear: The year which the birthrate belons to
# Sex: Either "male" or "female"
# Iterations: Either 1,2,3. This is the number of years we "predict" for


iterate <- function(Country, PopStructure, LifeTable, BirthRateYear, Sex, Iterations){
  
  BirthRate <- births[births$Year == BirthRateYear & 
                        births$Country == Country,
                      "WeeklyBirthsBySex"]
  
  # Filtering life table to get the weekly survival probabilities
  
  prob <- fulltable[fulltable$year == LifeTable &
                      fulltable$region == Country,
                    c(Sex)]
  prob <- rep(1, times=length(prob)) - prob # Switching to survival probabilities
  prob <- prob^(1/52)
  
  # Initialising the Surviving matrix. Each row is an age group from 0 to 100+.
  # Each column is a week starting with week 0
  Surviving <- data.frame(matrix(nrow = 102, ncol = 1+(52*Iterations)))
  
  # Putting in the week0 popuation
  Surviving[, 1] <- append(fullpop[fullpop$year == PopStructure &
                                     fullpop$region == Country,
                                   Sex], 0)
  
  Surviving[102, ] <- rep(0, times=ncol(Surviving))
  
  Deaths <- c()
  
  for(i in 2:ncol(Surviving)){ #Looping over weeks, starting at i=2 so week 1
    
    Surviving[1:101, i] <- Surviving[1:101, i-1]*prob # Killing off a proportion of the previous week's entrants
    Deaths <- append(Deaths, sum(Surviving[1:101, i-1] - Surviving[1:101, i])) #The number of people who just died
    
    for (j in 2:nrow(Surviving)){ #Starting with the 1 year olds, they gain people from the row above
      
      BirthdayPop <- Surviving[j-1, i]/52 # The number of people in row j-1 year olds aging up
      Surviving[j-1, i] <- Surviving[j-1, i] - BirthdayPop # Removing them from their current agegroup
      Surviving[j, i] <- Surviving[j, i] + BirthdayPop # Adding them to their new agegroup
    }
    Surviving[1, i] <- Surviving[1, i] + BirthRate # Babies are born
  }
  
  colnames(Surviving) <- 0:(ncol(Surviving)-1)
  Surviving <- cbind(Ages = append(c(0:100), "101"), Surviving)
  
  out <- data.frame(Deaths)
  out$WeekNumber <- rep(1:52, times=Iterations)
  out$Year <- rep(PopStructure:(PopStructure+Iterations - 1), each=52)
  out$Sex <- rep(Sex, times=nrow(out))
  return(out)
}

## Testing by using 2017-2019 lifetables and 2017-2019 pop to predict what actually
# happened

testE <- iterate("England", 2017, 2017, 2017, "female", 3)
testW <- iterate("Wales", 2017, 2017, 2017, "female", 3)

pred <- sum(testE$Deaths) + sum(testW$Deaths)
real <- sum(actualdeaths[actualdeaths$Year <= 2019 &
                           actualdeaths$Year >=2017 &
                           actualdeaths$Country == "EW" &
                           actualdeaths$Gender == "Female", "Deaths"])
pred-real
pred/real
# Overestimating deaths by 0.9%



### Seasonality ###################################################################

# Country: either "EW" or "S"
# Sex: for EW either "All", "Female" or "Male. For S this must be "All"
# Iterations: Either 1,2,3. Note that PopStructure + Iterations <=2021
# 

LT <- function(Country, PopStructure, LifeTable, BirthRateYear, Sex, Iterations){
  
  # Finding the baseline using life tables. This is done separately for EW and S
  # because of limited data
  if (Country == "EW"){
    if (Sex == "All"){
      fbaseline <- data.frame(iterate("England", PopStructure, LifeTable, BirthRateYear, "female", Iterations))$Deaths +
        data.frame(iterate("Wales", PopStructure, LifeTable, BirthRateYear, "female", Iterations))$Deaths
      mbaseline <- iterate("England", PopStructure, LifeTable, BirthRateYear, "male", Iterations)$Deaths +
        iterate("Wales", PopStructure, LifeTable, BirthRateYear, "male", Iterations)$Deaths
      baseline <- fbaseline + mbaseline
    }else if(Sex == "Female"){
      baseline <- data.frame(iterate("England", PopStructure, LifeTable, BirthRateYear, "female", Iterations))$Deaths +
        data.frame(iterate("Wales", PopStructure, LifeTable, BirthRateYear, "female", Iterations))$Deaths
    }else{
      baseline <- data.frame(iterate("England", PopStructure, LifeTable, BirthRateYear, "male", Iterations))$Deaths +
        data.frame(iterate("Wales", PopStructure, LifeTable, BirthRateYear, "male", Iterations))$Deaths
    }
  }
  if (Country == "S"){
    fbaseline <- iterate("Scotland", PopStructure, LifeTable, BirthRateYear, "female", Iterations)
    mbaseline <- iterate("Scotland", PopStructure, LifeTable, BirthRateYear, "male", Iterations)
    baseline <- fbaseline$Deaths + mbaseline$Deaths
  }
  
  # The deaths and W/T values we use to fit the GAM model
  FitDeaths <- actualdeaths[actualdeaths$Country == Country &
                              actualdeaths$Gender == Sex &
                              actualdeaths$Year %in% LifeTable:(LifeTable+2), ]
  
  FitDat <- data.frame(y = FitDeaths$Deaths, w = FitDeaths$WeekNumber, t = 1:(52*3))
  
  # Fitting the GAM model
  gammodel <- gam(y ~ s(w, bs="cc", k=20) + 
                    s(t), method="REML", data=FitDat)
  
  # Getting the S(W) and S(T) values
  gamout <- data.frame(predict(gammodel, type = "terms"))
  gamout <- gamout[1:(52*Iterations), ]
  
  # Combining the life tables and GAM estimate
  PredDeaths <- gamout$s.w. + gamout$s.t. + baseline
  
  # Putting all the data back together
  Year <- rep(PopStructure:(PopStructure + Iterations -1), each=52)
  WeekNumber <- rep(1:52, times = Iterations)
  
  Deaths <- actualdeaths[actualdeaths$Gender == Sex &
                           actualdeaths$Country == Country &
                           actualdeaths$Year %in% PopStructure:(PopStructure + Iterations -1),
                         "Deaths"]
  
  Excess <- PredDeaths - Deaths
  WeekEnding <- ymd(actualdeaths[actualdeaths$Gender == Sex &
                                   actualdeaths$Country == Country &
                                   actualdeaths$Year %in% PopStructure:(PopStructure + Iterations -1),
                                 "WeekEnding"])
  out <- data.frame(Year, WeekNumber, WeekEnding, Deaths, PredDeaths, Excess)
  return(out)
}




#Country, PopStructure, LifeTable, BirthRateYear, Sex, Iterations
# Country = "England"
# PopStructure = 2020
# LifeTable = 2017
# BirthRateYear=2020
# Sex = "Female"
# Iterations = 2


### PLOTTING #######################
out <- LT("EW", 2020, 2017, 2020, "All", 2)

ggplot() +
  geom_line(data=out, aes(WeekEnding, Deaths)) +
  geom_line(data=out, aes(WeekEnding, PredDeaths)) + 
  theme_minimal()


outF <- LT("EW", 2020, 2017, 2020, "Female", 2)
outM <- LT("EW", 2020, 2017, 2020, "Male", 2)

ggplot() +
  geom_line(data=outF, aes(WeekEnding, Deaths)) +
  geom_line(data=outF, aes(WeekEnding, PredDeaths)) + 
  geom_line(data=outM, aes(WeekEnding, Deaths)) +
  geom_line(data=outM, aes(WeekEnding, PredDeaths)) + 
  theme_minimal()

out <- LT("S", 2020, 2017, 2020, "All", 2)

ggplot() +
  geom_line(data=out, aes(WeekEnding, Deaths)) +
  geom_line(data=out, aes(WeekEnding, PredDeaths)) + 
  theme_minimal()


ltallew <- ggplot(out, aes(x = WeekEnding)) +
  geom_line(aes(y = Deaths, color = "Actual Deaths")) +
  geom_line(aes(y = PredDeaths, color = "Predicted Deaths")) +
  scale_color_manual(values = c("Actual Deaths" = "red", "Predicted Deaths" = "black")) +
  labs(title = "Actual vs. Predicted Deaths in 2020 to 2021 for England and Wales",
       x = "Week Ending",
       y = "Number of Deaths",
       color = "Type") +
  theme_minimal()
ggsave("lifetableallew.png", ltallew, bg = "white")

ltmfew <- ggplot() +
  geom_line(data = outF, aes(x = WeekEnding, y = Deaths, color = "Female Actual")) +
  geom_line(data = outF, aes(x = WeekEnding, y = PredDeaths, color = "Female Predicted")) +
  geom_line(data = outM, aes(x = WeekEnding, y = Deaths, color = "Male Actual")) +
  geom_line(data = outM, aes(x = WeekEnding, y = PredDeaths, color = "Male Predicted")) +
  scale_color_manual(values = c("Female Actual" = "deeppink", "Female Predicted" = "pink",
                                "Male Actual" = "blue", "Male Predicted" = "lightblue")) +
  labs(title = "Gender-Specific Actual vs. Predicted Deaths in 2020 to 2021 for England and Wales",
       x = "Week Ending",
       y = "Number of Deaths",
       color = "Group") +
  theme_minimal()

ggsave("lifetablemfew.png", ltmfew, bg = "white")

out <- LT("S", 2020, 2017, 2020, "All", 2)
ltscot <- ggplot(out, aes(x = WeekEnding)) +
  geom_line(aes(y = Deaths, color = "Actual Deaths")) +
  geom_line(aes(y = PredDeaths, color = "Predicted Deaths")) +
  scale_color_manual(values = c("Actual Deaths" = "blue", "Predicted Deaths" = "black")) +
  labs(title = "Actual vs. Predicted Deaths in 2020 to 2021 for Scotland",
       x = "Week Ending",
       y = "Number of Deaths",
       color = "Type") +
  theme_minimal()
ggsave("lifetableacot.png", ltscot, bg = "white")
