library(tidyverse)
library(readxl)
library(lubridate)

### ENGLAND AND WALES ##########################################################
population <- read_excel("Population EW unprocessed.xlsx")
deaths <- read.csv("Deaths EW final.csv")
deaths <- deaths[deaths$Gender=="All",]

# Adding data at the start and end to allow interpolation of 2013 and 2021
population <- rbind(c("EW", 2012, 28724412, 27843384), population)
population <- rbind(c("EW", 2022, 30719864, 29518174), population)

population$Year <- as.numeric(population$Year)
population$Female <- as.numeric(population$Female)
population$Male <- as.numeric(population$Male)
population$All <- population$Female + population$Male

population <- population[order(population$Year, decreasing=FALSE), ]
population$Date <- ymd(20120630) + years(seq(0, 10))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- ymd(newpop$Date)

# Linearly interpolating between points
interpolate <- approxfun(x = population$Date, y = population$All, method = "linear")
newpop$All <- interpolate(newpop$Date)

interpolate <- approxfun(population$Date, population$Female, method = "linear")
newpop$Female <- interpolate(newpop$Date)

interpolate <- approxfun(population$Date, population$Male, method = "linear")
newpop$Male <- interpolate(newpop$Date)

# Fixing formatting 
newpop$Year <- year(newpop$Date)
newpop$WeekNumber <- week(newpop$Date)
newpop$Nation <- rep("EW", times=520)

newpop <- newpop[newpop$Year <= 2021, ]
newpop[newpop$WeekNumber == 53, "WeekNumber"] <- rep(52, times=length(newpop[newpop$WeekNumber == 53, "WeekNumber"]))
write.csv(newpop, file="Population EW final.csv")

EWfinal <- newpop

# SCOTLAND ####################################################################

# This is the same process as with England and Wales.

population <- read_excel("Population S unprocessed.xlsx", 
                         sheet = "Table_1", skip = 5)
deaths <- read.csv("Deaths EW final.csv")

deaths <- deaths[deaths$Gender == "All",]

population <- population[ , -c(1, 6:96)]
population <- population[population$`Area name` == "Scotland" & 
                           population$Sex == "Persons" &
                           population$Year <= 2021 &
                           population$Year >= 2012, ]

population <- rbind(population, list("Scotland", "Persons", "2022", "5436600"))
colnames(population)[4] <- "Population"

population$Date <- ymd(20120630) + years(seq(0, 10))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- ymd(newpop$Date)

interpolate <- approxfun(x = population$Date, y = population$Population, method = "linear")
newpop$Deaths <- interpolate(newpop$Date)
newpop$WeekNumber <- week(newpop$Date)
newpop$Nation <- rep("S", nrow(newpop))
colnames(newpop)[2] <- "All"
newpop$Year <- year(newpop$Date)
newpop[newpop$WeekNumber == 53, "WeekNumber"] <- rep(52, times=length(newpop[newpop$WeekNumber == 53, "WeekNumber"]))
newpop <- newpop[newpop$Year <= 2021, ]

write.csv(newpop, file="Population S final.csv")

Sfinal <- newpop

# Northern Ireland ##############################################################

# Same process again

population <- read_excel("Population NI unprocessed.xlsx", 
                         sheet = "MySheet", col_names = FALSE)
deaths <- read.csv("Deaths EW final.csv")
deaths <- deaths[deaths$Gender == "All", ]

colnames(population) <- c("Year", "Deaths")
population <- rbind(population, c(2022, 1910500))
population$Date <- ymd(20120630) + years(seq(0, 10))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- ymd(newpop$Date)

interpolate <- approxfun(x = population$Date, y = population$Deaths, method = "linear")
newpop$Deaths <- interpolate(newpop$Date)
newpop$WeekNumber <- week(newpop$Date)
newpop$Nation <- rep("NI", nrow(newpop))

colnames(newpop)[2] <- "All"
newpop$Year <- year(newpop$Date)

newpop <- newpop[newpop$Year <= 2021, ]
newpop[newpop$WeekNumber == 53, "WeekNumber"] <- rep(52, times=length(newpop[newpop$WeekNumber == 53, "WeekNumber"]))
write.csv(newpop, file="Population NI final.csv")

NIfinal <- newpop

# COMBINING #############################

head(NIfinal)
head(EWfinal)
head(Sfinal)

NIfinal$Female <- rep(0, length=nrow(NIfinal))
NIfinal$Male <- rep(0, length=nrow(NIfinal))
Sfinal$Female <- rep(0, length=nrow(Sfinal))
Sfinal$Male <- rep(0, length=nrow(Sfinal))

combined <- rbind(NIfinal, Sfinal, EWfinal)

All <- NIfinal$All + Sfinal$All + EWfinal$All

UKfinal <- data.frame(All)
UKfinal$Date <- NIfinal$Date
UKfinal$WeekNumber <- NIfinal$WeekNumber
UKfinal$Nation <- rep("UK", times=nrow(UKfinal))
UKfinal$Year <- NIfinal$Year
UKfinal$Female <- rep(0, times=nrow(UKfinal))
UKfinal$Male <- rep(0, times=nrow(UKfinal))

combined <- rbind(combined, UKfinal)

combined <- combined[combined$Year <= 2021,]

write.csv(combined, file = "Population combined.csv")

