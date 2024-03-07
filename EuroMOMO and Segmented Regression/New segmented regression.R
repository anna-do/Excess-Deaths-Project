library(tidyverse)
library(readxl)
library(segmented)
library(lubridate)
library(MASS)
library(gridExtra)
library(grid)
library(kableExtra)

dat <- read.csv("Deaths combined.csv")
dat$WeekEnding <- ymd(dat$WeekEnding)
dat$X <- NULL


segreg <- function(country, fitstart, fitend, predstart, predend, gender){

  funcdat <- dat[dat$Gender==gender & dat$Country == country, ]
  funcdat$WeekEnding <- as.numeric(funcdat$WeekEnding) 
  
  # Fitting the model
  fitdat <- funcdat[funcdat$Year>=fitstart & funcdat$Year<=fitend, ]
  mod <- lm(Deaths ~ WeekNumber, data=fitdat)
  segmod <- segmented(mod, seg.Z=~WeekNumber, npsi=2, model=TRUE) # Going from lm to segmented
  
  print(segmod$psi)
  # Specifying the dates we're predicting for and carrying out prediction
  preddat <- funcdat[funcdat$Year>=predstart & funcdat$Year<=predend, ]
  predvals <- predict.segmented(segmod, newdata=preddat)
  
  # Calculating excess
  difference <- preddat$Deaths-predvals
  
  # Formatting data
  out <- data.frame(as_date(preddat$WeekEnding), preddat$Deaths, predvals, difference, week(as_date(preddat$WeekEnding)))
  colnames(out) <- c("WeekEnding", "Deaths", "PredDeaths", "Difference", "WeekNumber")
  
  return(out)
}

# Various plots
###############################################################################
# Model fit UK 2019-2021 600x300

out <- segreg("UK", 2015, 2019, 2019, 2021, "All")

ggplot()+
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
  geom_line(data=out, aes(WeekEnding, Deaths, color="Actual deaths")) +
  geom_line(data=out, aes(WeekEnding, PredDeaths, color="Segmented baseline")) +
  geom_hline(yintercept=0) +
  labs(x = "Date", y="Deaths", title="Segmented Regression: UK 2019-2021") +
  scale_color_manual(name="Legend:", 
                     values=c("orchid", "darkorchid4"),
                     limits = c("Actual deaths", "Segmented baseline")) +
  theme_minimal()

# Table

EW <- segreg("EW", 2015, 2019, 2020, 2020, "All")
EWF <- segreg("EW", 2015, 2019, 2020, 2020, "Female")
EWM <- segreg("EW", 2015, 2019, 2020, 2020, "Male")
S <- segreg("S", 2015, 2019, 2020, 2020, "All")
NI <- segreg("NI", 2015, 2019, 2020, 2020, "All")

excess2020 <- c(sum(EW$Difference), 
                sum(EWF$Difference), 
                sum(EWM$Difference), 
                sum(S$Difference), 
                sum(NI$Difference))

EW <- segreg("EW", 2015, 2019, 2021, 2021, "All")
EWF <- segreg("EW", 2015, 2019, 2021, 2021, "Female")
EWM <- segreg("EW", 2015, 2019, 2021, 2021, "Male")
S <- segreg("S", 2015, 2019, 2021, 2021, "All")
NI <- segreg("NI", 2015, 2019, 2021, 2021, "All")

excess2021 <- c(sum(EW$Difference), 
                sum(EWF$Difference), 
                sum(EWM$Difference), 
                sum(S$Difference), 
                sum(NI$Difference))

Country <- c("England and Wales", 
             "England and Wales - Female",
             "England and Wales - Male",
             "Scotland",
             "Northern Ireland")

SegregExcess <- data.frame(Country, excess2020, excess2021)

kbl(SegregExcess, caption=" ",format="latex")

# Excess deaths in each country multiplot

EW <- segreg("EW", 2015, 2019, 2020, 2020, "All")
S <- segreg("S", 2015, 2019, 2020, 2020, "All")
NI <- segreg("NI", 2015, 2019, 2020, 2020, "All")


excess_S <- ggplot() +
 # geom_vline(xintercept=ymd(c("2021-01-01")), linetype="dashed", color="grey70") +
    geom_bar(data=S, aes(WeekNumber, Difference), stat="identity", fill = "royalblue") +
    labs(x = "Week",
         y = "Excess deaths",
         title = "Scotland") +
    theme_minimal()

excess_NI <- ggplot() +
  #geom_vline(xintercept=ymd(c("2021-01-01")), linetype="dashed", color="grey70") +
    geom_bar(data=NI, aes(WeekNumber, Difference), stat="identity", fill = "darkgreen") +
    labs(x = "Week",
         y = "Excess deaths",
         title = "Northern Ireland") +
  theme_minimal()

excess_EW <- ggplot() +
  #geom_vline(xintercept=ymd(c("2021-01-01")), linetype="dashed", color="grey70") +
  geom_bar(data=EW, aes(WeekNumber, Difference), stat="identity", fill = "red3") +
  labs(x = "Week",
       y = "Excess deaths",
       title = "England and Wales") +
  theme_minimal()


grid.arrange(excess_EW, excess_S, excess_NI, nrow=1, top=textGrob("Segmented Regression: Excess Deaths in 2020 for all UK nations"))

### 

out <- segreg("EW", 2015, 2019, 2020, 2020, "All")
out <- rbind(out, segreg("EW", 2015, 2019, 2021, 2021, "All"))

#write.csv(out, "SegReg comparison.csv")
