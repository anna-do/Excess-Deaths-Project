library(tidyverse)
library(readxl)
library(segmented)
library(lubridate)

dat <- read.csv("Deaths combined.csv")
dat$WeekEnding <- dmy(dat$WeekEnding)
dat$WeekNumber <- week(dat$WeekEnding)


segreg <- function(country, fitstart, fitend, predyear, gender){
  
  # country is either "EW", "NI" or "S"
  # fitstart specifies the first year we fit the model on.
  # fitend is the last year we fit the model on.
  # predyear is the year which we predict deaths for.
  # gender is the gender we are predicting. For NI and S this must be "All".
  #     but for EW this can be "All", "Female" or "Male".
  
  #F Filtering for the correct country
  dat <- dat[dat$Country == country , ]
  
  predstart <- predyear
  predend <- predyear
  
  funcdat <- dat[dat$Gender==gender, ] # Filtering full dataset by gender
  funcdat$WeekEnding <- as.numeric(funcdat$WeekEnding) 
  
  # Fitting the model
  fitdat <- funcdat[funcdat$Year>=fitstart & funcdat$Year<=fitend, ]
  mod <- lm(Deaths ~ WeekEnding, data=fitdat)
  segmod <- segmented(mod) # Going from lm to segmented

  # Specifying the dates we're predicting for and carrying out prediction
  preddat <- funcdat[funcdat$Year>=predstart & funcdat$Year<=predend, ]
  predvals <- predict.segmented(segmod, newdata=preddat)
  
  # Calculating excess
  difference <- preddat$Deaths-predvals
  
  # Formatting data
  out <- data.frame(as_date(preddat$WeekEnding), preddat$Deaths, predvals, difference)
  colnames(out) <- c("WeekEnding", "Deaths", "PredDeaths", "Difference")
  
  return(out)
}

## Some sample plots below

allout <- segreg("EW", 2013, 2018, 2019, "Female") # Change the parameters to experiment

pivotout <- allout %>% pivot_longer(cols=c("Deaths", "PredDeaths", "Difference"))
colnames(pivotout) <- c("WeekEnding", "Type", "Deaths")

ggplot(data = pivotout, aes(WeekEnding, Deaths, color=Type)) + geom_line() + geom_hline(yintercept=0)

TotalDeaths <- sum(allout$Deaths)
ExcessDeaths <- sum(allout$Difference)

alltotal <- data.frame(TotalDeaths, ExcessDeaths)


