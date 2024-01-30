library(tidyverse)
library(readxl)
library(lubridate)

# Task: Update file names to finalised versions
deathdat <- read_csv("Deaths combined.csv")
population <- read_excel("Population combined.xlsx")

mortality$MortalityRate <- mortality$`Mortality rate`
mortality$`Mortality rate` <- NULL

deathdat$WeekEnding <- dmy(deathdat$WeekEnding)
deathdat$WeekNumber <- week(deathdat$WeekEnding)
population$WeekNumber <- week(population$Date)

EuroMOMO <- function(country, fitstart, fitend, predyear, gender){
  
  # Selecting the right country
  deathdat <- deathdat[deathdat$Country == country , ]
  population <- population[population$Nation == country , ]
  
  predstart <- predyear
  predend <- predyear
  
  # Specifying size of at-risk population for fitting the model
  Nfit <- population[population$Year>=fitstart & population$Year<=fitend, c(gender, "Date", "Year", "WeekNumber", "Nation")]
  Nfit <- Nfit[(Nfit$WeekNumber>=15 & Nfit$WeekNumber<=26) | (Nfit$WeekNumber>=36 & Nfit$WeekNumber<=45) , ]
  colnames(Nfit)[1] <- "N"
  
  #Specifying size of at-risk population for predictions
  Npred <- population[population$Year>=predstart & population$Year<=predend, c(gender, "Date", "Year", "WeekNumber", "Nation")]
  colnames(Npred)[1] <- "N"
  
  # Specifying mortality rate of at-risk group at a year level since we don't have weekly
  Xfit <- mortality[mortality$Year>=fitstart & mortality$Year<=fitend & mortality$Type==type & mortality$Sex==gender, ]
  newfit <- Xfit
  for(i in seq(1, 21)){
    newfit <- rbind(newfit, Xfit)
  }
  Xfit <- arrange(newfit, Year)
  
  # Specifying mortality rate of at-risk group at a year level since we don't have weekly
  Xpred <- mortality[mortality$Year>=predstart & mortality$Year<=predend & mortality$Type==type & mortality$Sex==gender, ]
  newpred <- Xpred
  for(i in seq(1, 51)){
    newpred <- rbind(newpred, Xpred)
  }
  Xpred <- arrange(newpred, Year)
  
  # Specifying mu (number of deaths) for fitting the model
  fitdeaths <- deathdat[deathdat$Year>=fitstart & deathdat$Year<=fitend & deathdat$Gender==gender, ]
  fitdeaths <- fitdeaths[(fitdeaths$WeekNumber>=15 & fitdeaths$WeekNumber<=26) | (fitdeaths$WeekNumber>=36 & fitdeaths$WeekNumber<=45) , ]
  
  # Specifying the actual number of deaths during the period we're predicting
  preddeaths <- deathdat[deathdat$Year>=predstart & deathdat$Year<=predend & deathdat$Gender==gender, ]
  
  # Putting together the final dataframes
  fitdat <- fitdeaths[, c("Year", "WeekNumber", "WeekEnding", "Deaths")]
  fitdat$N <- Nfit$N
  fitdat$X <- Xfit$MortalityRate
  fitdat$Y1 <- cos((2*pi/52)*fitdat$WeekNumber)
  fitdat$Y2 <- sin((2*pi/52)*fitdat$WeekNumber)
  
  colnames(fitdat) <- c("Year", "WeekNumber", "WeekEnding", "Mu", "N", "X", "Y1", "Y2")
  
  preddat <- preddeaths[, c("Year", "WeekNumber", "WeekEnding", "Deaths")]
  preddat$N <- Npred$N
  preddat$X <- Xpred$MortalityRate
  preddat$Y1 <- cos((2*pi/52)*preddat$WeekNumber)
  preddat$Y2 <- sin((2*pi/52)*preddat$WeekNumber)
  
  colnames(preddat) <- c("Year", "WeekNumber", "WeekEnding", "Mu", "N", "X", "Y1", "Y2")
  
  # Fitting the model
  mod <- glm(Mu ~ N + WeekNumber + Y1 + Y2, 
             family = quasipoisson(link="log"),
             data = fitdat)
  
  # Predicting the deaths
  predvals <- predict(object = mod, newdata = preddat, type="response")
  
  preddat$Predvals <- as.numeric(predvals)
  
  # Excess deaths
  preddat$Excess <- preddat$Mu - preddat$Predvals 
  
  return(preddat)
}
# 
# fitstart = 2013
# fitend = 2018
# predyear = 2020
# gender="Female"
# country="EW"
# type="Cohort"

out <- EuroMOMO("EW", 2013, 2018, 2020, "Female") # Vary these parameters for testing

pivotout <- pivot_longer(out, c("Mu", "Predvals", "Excess"))
colnames(pivotout)[which(names(pivotout) == "name")] <- "Type"
colnames(pivotout)[which(names(pivotout) == "value")] <- "Deaths"

ggplot(data = pivotout, aes(WeekEnding, Deaths, color = Type)) + geom_line() + geom_hline(yintercept=0)
