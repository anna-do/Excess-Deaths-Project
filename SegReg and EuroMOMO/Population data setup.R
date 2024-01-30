library(tidyverse)
library(readxl)
library(lubridate)

### ENGLAND AND WALES ##########################################################
population <- read_excel("Population EW unprocessed.xlsx")
deaths <- read.csv("Deaths EW final.csv")
deaths <- deaths[deaths$Gender=="All",]

# Adding in 2012 data
population <- rbind(c("EW", 2012, 28724412, 27843384), population)

population$Year <- as.numeric(population$Year)
population$Female <- as.numeric(population$Female)
population$Male <- as.numeric(population$Male)
population$All <- population$Female + population$Male

population$Date <- ymd(20120630) + years(seq(0, 9))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- dmy(newpop$Date)

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

newpop <- newpop[newpop$Year <= 2020, ]

write.csv(newpop, file="Population EW final.csv")

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

colnames(population)[4] <- "Deaths"

population$Date <- ymd(20120630) + years(seq(0, 9))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- dmy(newpop$Date)

interpolate <- approxfun(x = population$Date, y = population$Deaths, method = "linear")
newpop$Deaths <- interpolate(newpop$Date)
newpop$WeekNumber <- week(newpop$Date)
newpop$Nation <- rep("S", nrow(newpop))
colnames(newpop)[2] <- "All"
newpop$Year <- year(newpop$Date)

newpop <- newpop[newpop$Year <= 2020, ]

write.csv(newpop, file="Population S final.csv")

# Northern Ireland ##############################################################

# Same process again

population <- read_excel("Population NI unprocessed.xlsx", 
                         sheet = "MySheet", col_names = FALSE)
deaths <- read.csv("Deaths EW final.csv")

deaths <- deaths[deaths$Gender == "All", ]

colnames(population) <- c("Year", "Deaths")
population$Date <- ymd(20120630) + years(seq(0, 9))

Date <- deaths$WeekEnding
newpop <- data.frame(Date)
newpop$Date <- dmy(newpop$Date)

interpolate <- approxfun(x = population$Date, y = population$Deaths, method = "linear")
newpop$Deaths <- interpolate(newpop$Date)
newpop$WeekNumber <- week(newpop$Date)
newpop$Nation <- rep("NI", nrow(newpop))

colnames(newpop)[2] <- "All"
newpop$Year <- year(newpop$Date)

newpop <- newpop[newpop$Year <= 2020, ]

write.csv(newpop, file="Population NI final.csv")

