library(tidyverse)
library(readxl)
library(lubridate)

### ENGLAND AND WALES ##########################################################

deathdat <- read_excel("Deaths EW unprocessed.xlsx")

# Splitting into male, female and all to make life easier.
femdat <- filter(deathdat, Gender == "Female")
maledat <- filter(deathdat, Gender == "Male")
alldat <- filter(deathdat, Gender == "All")
colnames(femdat) <- c("Year", "Gender", "WeekNumber", "WeekEnding", "Deaths")
colnames(maledat) <- c("Year", "Gender", "WeekNumber", "WeekEnding", "Deaths")
colnames(alldat) <- c("Year", "Gender", "WeekNumber", "WeekEnding", "Deaths")

# Putting dates into as.date format and adding a column of cumulative deaths
femdat$WeekEnding <- as.Date(femdat$WeekEnding)
femdat$CumulativeDeaths <- cumsum(femdat$Deaths)
maledat$WeekEnding <- as.Date(maledat$WeekEnding)
maledat$CumulativeDeaths <- cumsum(maledat$Deaths)
alldat$WeekEnding <- as.Date(alldat$WeekEnding)
alldat$CumulativeDeaths <- cumsum(alldat$Deaths)

# Creating a list of week end-dates from Jan 7th 2013 to Dec 26th 2022
yearlist <- ymd('2013-01-07') + years(seq(0, 9)) #List of th 10 years
yearlist <- sort(rep(yearlist, 52)) # Repeat each year 52 times
list <- rep(seq(0, 51), 10) # 1,2,3,...,52,1,2,3...,52,... ten times
WeekEnding <- yearlist + days(7*list)

list <- (1:10)*52
WeekEnding[list] <- WeekEnding[list] + days(rep(1, times=10))
WeekEnding[list]

### CREATING NEW DATASET #########################################################

# Fitting the spline function to the cumulative deaths with original dates.
interpolate <- splinefun(femdat$WeekEnding, femdat$CumulativeDeaths,
                         method="hyman")

# Creating a new dataframe with the correct dates.
Newfemdat <- data.frame(WeekEnding)

# Finding the interpolated number of deaths for the new week end-dates.
Newfemdat$CumulativeDeaths <- interpolate(WeekEnding)

# In the original dataset we know that 6243 women died in the week-ending January
# 4th. This means roughly 3567.43 died Jan 1st-4th inclusive (assuming deaths are
# uniformly distributed across each week).

# In terms of cumulative deaths this means 6243 had died by Jan 4th and 12893 by
# Jan 11th

# We interpolate these points to get the cumulative number of deaths by Jan 7th
# which is 9259 (which makes sense because it's between 6243 and 12893). 15425 
# had died by Jan 14th and so on.

# If we do diff(9259, 15425, .....) we lose the number of people who died in the
# first week (just like how diff(2,5,6) = (3, 1)). Simon recommended we put a 0 
# at the start of the cumulative deaths to fix this however (according to the
# spline function) there aren't actually 0 deaths at the start of January (Anna
# draw a wee graph for yourself if you forget how this work).

# Manually calculating how many deaths actually happened in the first week of Jan 2013

num <- ((femdat$Deaths[1] / 7)*4) + ((femdat$Deaths[2] / 7)*3)
Newfemdat$Deaths <- append(num, diff(Newfemdat$CumulativeDeaths))

###

interpolate <- splinefun(maledat$WeekEnding, maledat$CumulativeDeaths,
                         method="hyman")

Newmaledat <- data.frame(WeekEnding)
Newmaledat$CumulativeDeaths <- interpolate(WeekEnding)

num <- ((maledat$Deaths[1] / 7)*4) + ((maledat$Deaths[2] / 7)*3)
Newmaledat$Deaths <- append(num, diff(Newmaledat$CumulativeDeaths))

###

interpolate <- splinefun(alldat$WeekEnding, alldat$CumulativeDeaths,
                         method="hyman")

Newalldat <- data.frame(WeekEnding)
Newalldat$CumulativeDeaths <- interpolate(WeekEnding)

num <- ((alldat$Deaths[1] / 7)*4) + ((alldat$Deaths[2] / 7)*3)
Newalldat$Deaths <- append(num, diff(Newalldat$CumulativeDeaths))

### PUTTING THEM BACK TOGETHER #################################################

Newmaledat$Gender <- rep("Male", nrow(Newmaledat))
Newfemdat$Gender <- rep("Female", nrow(Newfemdat))
Newalldat$Gender <- rep("All", nrow(Newalldat))

data <- rbind(Newalldat, Newfemdat, Newmaledat)
data$Year <- year(data$WeekEnding)
# data$WeekNumber <- week(data$WeekEnding) This isn't useful since weeks always
# start on different days.

### CHECKING ###################################################################

plot(WeekEnding) # Nice straight line with no funny business
WeekEnding # Start and end-dates are within the rang of the original dataset

# Deaths are of the right order of magnitude at different times of the year
# Spikes are in the right place which is nice
plot(Newfemdat$WeekEnding, Newfemdat$Deaths) 
plot(Newmaledat$WeekEnding, Newmaledat$Deaths)
plot(Newalldat$WeekEnding, Newalldat$Deaths)

### SAVING #####################################################################


write.csv(data, file = "Deaths EW final.csv")

EWfinal <- data

### SCOTLAND ######

# The dates are already correct for this data so the code just deletes the 
# unneccessary data and puts it into the correct format.

deathdat <- read_excel("Deaths S unprocessed.xlsx", skip = 2)
sdeathdat <- deathdat
deathdat <- deathdat[-c(54:62), -c(2:40, 51, 52)]
colnames(deathdat) <- c("WeekNumber", "y2013", "y2014", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "y2021", "y2022")

deathdat$y2013 <- as.numeric(deathdat$y2013)
deathdat$y2014 <- as.numeric(deathdat$y2014)
deathdat$y2015 <- as.numeric(deathdat$y2015)
deathdat$y2016 <- as.numeric(deathdat$y2016)
deathdat$y2017 <- as.numeric(deathdat$y2017)
deathdat$y2018 <- as.numeric(deathdat$y2018)
deathdat$y2019 <- as.numeric(deathdat$y2019)
deathdat$y2020 <- as.numeric(deathdat$y2020)
deathdat$y2021 <- as.numeric(deathdat$y2021)
deathdat$y2022 <- as.numeric(deathdat$y2022)

deathdat$WeekNumber <- 1:53
deathdat[53, 10] <- 0
deathdat[53, 11] <- 0
temp <- deathdat[53, 2:11]
deathdat[1:52, 2:11] <- deathdat[1:52, 2:11] + 1/52*temp[rep(as.numeric(seq_len(nrow(temp))), each=52),]
deathdat <- deathdat[-53, ]
deathdat <- pivot_longer(deathdat, cols=colnames(deathdat)[-1])
colnames(deathdat) <- c("WeekNumber", "Year", "Deaths")

deathdat <- deathdat[order(deathdat$Year), ]
deathdat$WeekEnding <- EWfinal[1:520, "WeekEnding"]
deathdat$Year <- rep(2013:2022, times=1, each=52)
deathdat$Gender <- rep("All", times = 520)

write.csv(deathdat, file = "Deaths S final.csv")

Sfinal <- deathdat



### Northern Ireland ###########################################################

# This is the same process as England and Wales but without the separation by 
# gender.
data <- read_excel("Deaths NI unprocessed.xlsx", sheet = "MySheet")

data$WeekEnding <- as.Date(data$WeekEnding)
data$CumulativeDeaths <- cumsum(data$Deaths)

yearlist <- ymd('2013-01-07') + years(seq(0, 9)) #List of th 10 years
yearlist <- sort(rep(yearlist, 52)) # Repeat each year 52 times
list <- rep(seq(0, 51), 10) # 1,2,3,...,52,1,2,3...,52,... ten times
WeekEnding <- yearlist + days(7*list)

list <- (1:10)*52
WeekEnding[list] <- WeekEnding[list] + days(rep(1, times=10))
WeekEnding[list]

interpolate <- splinefun(data$WeekEnding, data$CumulativeDeaths,
                         method="hyman")

NewData <- data.frame(WeekEnding)

NewData$CumulativeDeaths <- interpolate(WeekEnding)

num <- ((data$Deaths[1] / 7)*4) + ((data$Deaths[2] / 7)*3)
NewData$Deaths <- append(num, diff(NewData$CumulativeDeaths))

NewData$Gender <- rep("All", nrow(NewData))
NewData$WeekNumber <- week(NewData$WeekEnding)
NewData <- NewData[, -2]
NewData$Year <- year(WeekEnding)


write.csv(NewData, file = "Deaths NI final.csv")

NIfinal <- NewData

#### COMBINING ALL 4 NATIONS ##############

head(NIfinal)
head(Sfinal)
head(EWfinal)

EWfinal$CumulativeDeaths <- NULL
EWfinal$WeekNumber <- week(EWfinal$WeekEnding)

EWfinal$Country <- rep("EW", times = nrow(EWfinal))
NIfinal$Country <- rep("NI", times = nrow(NIfinal))
Sfinal$Country <- rep("S", times = nrow(Sfinal))

combined <- rbind(NIfinal, Sfinal, EWfinal)

head(combined)

combined <- combined[combined$Year <= 2021, ]

UK <- data.frame(WeekEnding = combined[combined$Gender=="All" & combined$Country=="NI", "WeekEnding"],
                 Deaths = combined[combined$Gender=="All" & combined$Country=="NI", "Deaths"] +
                   combined[combined$Gender=="All" & combined$Country=="EW", "Deaths"] + 
                   combined[combined$Gender=="All" & combined$Country=="S", "Deaths"],
                 Gender = rep("All", times = nrow(combined[combined$Gender=="All" & combined$Country=="NI", ])),
                 WeekNumber = combined[combined$Gender=="All" & combined$Country=="NI", "WeekNumber"],
                 Year = combined[combined$Gender=="All" & combined$Country=="NI", "Year"],
                 Country = rep("UK", times=nrow(combined[combined$Gender=="All" & combined$Country=="NI", ])))

combined <- rbind(combined, UK)

combined[combined$WeekNumber==53, "WeekNumber"] <- rep(52, times = length(combined[combined$WeekNumber==53, "WeekNumber"]))

write.csv(combined, file = "Deaths combined.csv")
