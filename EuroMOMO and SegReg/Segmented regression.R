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

# Variables for debugging
# country="EW"
# fitstart=2015
# fitend=2019
# predstart=2020
# predend=2021
# gender="Female"

segreg <- function(country, fitstart, fitend, predstart, predend, gender){
  
  #Filtering for the correct country
  dat <- dat[dat$Country == country , ]
  
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
  out <- data.frame(as_date(preddat$WeekEnding), preddat$Deaths, predvals, difference, week(as_date(preddat$WeekEnding)))
  colnames(out) <- c("WeekEnding", "Deaths", "PredDeaths", "Difference", "WeekNumber")
  
  return(out)
}

deathdat <- dat

###############################################################################
# Model fit UK 2019-2021 600x300

out <- segreg("UK", 2015, 2019, 2019, 2021, "All")

ggplot()+
  geom_line(data=out, aes(WeekEnding, Deaths, color="Actual deaths")) +
  geom_line(data=out, aes(WeekEnding, PredDeaths, color="Segmented baseline")) +
  geom_hline(yintercept=0) +
  labs(x = "Date", y="Deaths", title="Segmented Regression: UK 2019-2021") +
  scale_color_manual(name="Legend:", 
                     values=c("orchid", "darkorchid4"),
                     limits = c("Actual deaths", "Segmented baseline")) +
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
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



# Plots not used in the report #################################################

## SegReg EW 2020 by sex (650 x 400)
# 
# EWF <- segreg("EW", 2015, 2019, 2020, 2021, "Female")
# EWM <- segreg("EW", 2015, 2019, 2020, 2021, "Male")
# 
# ggplot() +
#   geom_line(data = EWF, aes(WeekNumber, PredDeaths, color="Baseline deaths - female")) +
#   geom_line(data = EWF, aes(WeekNumber, Deaths, color="Actual deaths - female")) +
#   geom_line(data = EWM, aes(WeekNumber, PredDeaths, color="Baseline deaths - male")) +
#   geom_line(data = EWM, aes(WeekNumber, Deaths, color="Actual deaths - male")) +
#   geom_hline(yintercept=0) + 
#   labs(x = "Week end-date", 
#        y="Deaths", 
#        title = "Segmented Regression: England and Wales 2020 by Sex") +
#   scale_color_manual(name = "Legend:", values=c("coral", "cornflowerblue", "brown", "blue"))+
#   scale_x_continuous(limit=c(0, 52)) +
#   theme_minimal()
# 
# ## SegReg EW 2020 (650x400) ###############
# 
# EW <- segreg("EW", 2015, 2019, 2020, "All")
# 
# deaths2019 <- deathdat[deathdat$Country == "EW" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2019, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2018 <- deathdat[deathdat$Country == "EW" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2018, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2017 <- deathdat[deathdat$Country == "EW" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2017, 
#                        c("Deaths", "WeekNumber")]
# 
# ggplot() +
#   geom_line(data = deaths2019, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2018, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2017, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data=EW, aes(WeekNumber, Deaths, color = "Actual deaths")) +
#   geom_line(data=EW, aes(WeekNumber, PredDeaths, color = "Baseline deaths")) +
#   geom_hline(yintercept = 0) +
#   labs(x = "Week number",
#        y = "Deaths",
#        title = "Segmented Regression: England and Wales 2020") +
#   scale_color_manual(name="Legend", 
#                      values=c("grey75", "red4", "red1"),
#                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
#   scale_x_continuous(limit=c(0, 52)) +
#   theme_minimal()
# 
# ## SegReg S 2020 (650x400) ###############
# 
# S <- segreg("S", 2015, 2019, 2020, "All")
# 
# deaths2019 <- deathdat[deathdat$Country == "S" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2019, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2018 <- deathdat[deathdat$Country == "S" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2018, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2017 <- deathdat[deathdat$Country == "S" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2017, 
#                        c("Deaths", "WeekNumber")]
# 
# ggplot() +
#   geom_line(data = deaths2019, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2018, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2017, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = S, aes(WeekNumber, Deaths, color = "Actual deaths")) +
#   geom_line(data = S, aes(WeekNumber, PredDeaths, color = "Baseline deaths")) +
#   geom_hline(yintercept = 0) +
#   labs(x = "Week number",
#        y = "Deaths",
#        title = "Segmented Regression: Scotland 2020") +
#   scale_color_manual(name="Legend", 
#                      values=c("grey75", "cornflowerblue", "blue"),
#                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
#   scale_x_continuous(limit=c(0, 52)) +
#   theme_minimal()
# 
# ## SegReg NI 2020 (650x400) ####################################################
#   
# NI <- segreg("NI", 2015, 2019, 2020, "All")
# 
# deaths2019 <- deathdat[deathdat$Country == "NI" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2019, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2018 <- deathdat[deathdat$Country == "NI" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2018, 
#                        c("Deaths", "WeekNumber")]
# 
# deaths2017 <- deathdat[deathdat$Country == "NI" & 
#                          deathdat$Gender == "All" & 
#                          deathdat$Year == 2017, 
#                        c("Deaths", "WeekNumber")]
# 
# ggplot() +
#   geom_line(data = deaths2019, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2018, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = deaths2017, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
#   geom_line(data = NI, aes(WeekNumber, Deaths, color = "Actual deaths")) +
#   geom_line(data = NI, aes(WeekNumber, PredDeaths, color = "Baseline deaths")) +
#   geom_hline(yintercept = 0) +
#   labs(x = "Week number",
#        y = "Deaths",
#        title = "Segmented Regression: Northern Ireland 2020") +
#   scale_color_manual(name="Legend", 
#                      values=c("grey75", "green3", "darkgreen"),
#                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
#   scale_x_continuous(limit=c(0, 52)) +
#   theme_minimal()
# 
# # Excess deaths multiplot
# 
# excess_S <- ggplot() + 
#   geom_bar(data=S, aes(WeekNumber, Difference), stat="identity", fill = "royalblue") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "Scotland") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# excess_NI <- ggplot() + 
#   geom_bar(data=NI, aes(WeekNumber, Difference), stat="identity", fill = "darkgreen") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "Northern Ireland") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# excess_EW <- ggplot() + 
#   geom_bar(data=EW, aes(WeekNumber, Difference), stat="identity", fill = "red3") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "England and Wales") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# 
# grid.arrange(excess_EW, excess_S, excess_NI, nrow=1, top=textGrob("EuroMOMO: Excess Deaths in 2020 for all UK nations"))
# 
# 
# # Total excess deaths
# 
# 
# Excess <- c(sum(EW$Difference),
#             sum(S$Difference),
#             sum(NI$Difference))
# 
# SegRegExcess <- data.frame(Excess)
# SegRegExcess$Country <- c("England and Wales", "Scotland", "Northern Ireland")
# 
# kbl(SegRegExcess, caption=" ",format="latex")


