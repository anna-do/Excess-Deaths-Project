library(tidyverse)
library(readxl)
library(lubridate)
library(MASS)
library(gridExtra)
library(grid)
library(kableExtra)

deathdat <- read_csv("Deaths combined.csv")
deathdat$...1 <- NULL

population <- read_csv("Population combined.csv")
population$...1 <- NULL


# Variables for debugging

# country="EW"
# fitstart=2015
# fitend=2019
# predstart=2020
# predend=2021
# gender="All"

##################

EuroMOMO <- function(country, fitstart, fitend, predstart, predend, gender){
  
  # Selecting the right country
  deathdat <- deathdat[deathdat$Country == country , ]
  population <- population[population$Nation == country , ]
  
  # Specifying size of at-risk population for fitting the model
  Nfit <- population[population$Year>=fitstart & population$Year<=fitend, c(gender, "Date", "Year", "WeekNumber", "Nation")]
  Nfit <- Nfit[(Nfit$WeekNumber>=15 & Nfit$WeekNumber<=26) | (Nfit$WeekNumber>=36 & Nfit$WeekNumber<=45) , ]
  colnames(Nfit)[1] <- "N"
  
  #Specifying size of at-risk population for predictions
  Npred <- population[population$Year>=predstart & population$Year<=predend, c(gender, "Date", "Year", "WeekNumber", "Nation")]
  colnames(Npred)[1] <- "N"
  
  # Specifying mu (number of deaths) for fitting the model
  fitdeaths <- deathdat[deathdat$Year>=fitstart & deathdat$Year<=fitend & deathdat$Gender==gender, ]
  fitdeaths <- fitdeaths[(fitdeaths$WeekNumber>=15 & fitdeaths$WeekNumber<=26) | (fitdeaths$WeekNumber>=36 & fitdeaths$WeekNumber<=45) , ]
  
  # Specifying the actual number of deaths during the period we're predicting
  preddeaths <- deathdat[deathdat$Year>=predstart & deathdat$Year<=predend & deathdat$Gender==gender, ]
  
  # Putting together the final dataframes
  fitdat <- fitdeaths[, c("Year", "WeekNumber", "WeekEnding", "Deaths")]
  fitdat$N <- Nfit$N
  fitdat$Y1 <- cos((2*pi/52)*fitdat$WeekNumber)
  fitdat$Y2 <- sin((2*pi/52)*fitdat$WeekNumber)
  
  colnames(fitdat) <- c("Year", "WeekNumber", "WeekEnding", "Mu", "N", "Y1", "Y2")
  
  preddat <- preddeaths[, c("Year", "WeekNumber", "WeekEnding", "Deaths")]
  preddat$N <- Npred$N
  preddat$Y1 <- cos((2*pi/52)*preddat$WeekNumber)
  preddat$Y2 <- sin((2*pi/52)*preddat$WeekNumber)
  
  colnames(preddat) <- c("Year", "WeekNumber", "WeekEnding", "Mu", "N", "Y1", "Y2")
  
  # Fitting the model
  mod <- glm(Mu ~ log(N) + WeekEnding + Y1 + Y2, 
             family = quasipoisson(link="log"),
             data = fitdat)
  
  # Predicting the deaths
  predvals <- predict(object = mod, newdata = preddat, type="response")
  
  preddat$Predvals <- as.numeric(predvals)
  
  # Excess deaths
  preddat$Excess <- preddat$Mu - preddat$Predvals 
  
  colnames(preddat)[4] <- "Actual deaths"
  colnames(preddat)[8] <- "Predicted deaths"
  colnames(preddat)[9] <- "Excess deaths"
  
  return(preddat)
}

### PLOTTING ##################################################################

### TABLES

EW <- EuroMOMO("EW", 2015, 2019, 2020, 2020, "All")
EWF <- EuroMOMO("EW", 2015, 2019, 2020, 2020, "Female")
EWM <- EuroMOMO ("EW", 2015, 2019, 2020, 2020, "Male")
S <- EuroMOMO("S", 2015, 2019, 2020, 2020, "All")
NI <- EuroMOMO("NI", 2015, 2019, 2020, 2020, "All")

`Excess Deaths 2020` <- c(sum(EW$`Excess deaths`),
                          sum(EWF$`Excess deaths`),
                          sum(EWM$`Excess deaths`),
                          sum(S$`Excess deaths`), 
                          sum(NI$`Excess deaths`))

EW <- EuroMOMO("EW", 2015, 2019, 2021, 2021, "All")
EWF <- EuroMOMO("EW", 2015, 2019, 2021, 2021, "Female")
EWM <- EuroMOMO ("EW", 2015, 2019, 2021, 2021, "Male")
S <- EuroMOMO("S", 2015, 2019, 2021, 2021, "All")
NI <- EuroMOMO("NI", 2015, 2019, 2021, 2021, "All")

`Excess Deaths 2021` <- c(sum(EW$`Excess deaths`),
                          sum(EWF$`Excess deaths`),
                          sum(EWM$`Excess deaths`),
                          sum(S$`Excess deaths`), 
                          sum(NI$`Excess deaths`))

Country <- c("England and Wales - All", 
             "England and Wales - Female",
             "England and Wales - Male",
             "Scotland", 
             "Northern Ireland")

MOMOExcess <- data.frame(Country, `Excess Deaths 2020`, `Excess Deaths 2021`)

kbl(MOMOExcess, format="latex")

### Total UK Model Fit

out <- EuroMOMO("UK", 2015, 2019, 2019, 2021, "All")

ggplot() + 
  geom_line(data=out, aes(WeekEnding, `Actual deaths`, color = "Actual deaths")) +
  geom_line(data=out, aes(WeekEnding, `Predicted deaths`, color = "EuroMOMO baseline")) +
  geom_hline(yintercept=0) +
  labs(x = "Date", y="Deaths", title="EuroMOMO: UK 2019-2021") +
  scale_color_manual(name="Legend:", 
                     values=c("orchid", "darkorchid4"),
                     limits = c("Actual deaths", "EuroMOMO baseline")) +
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
  theme_minimal()

### Total UK excess 800x400

out <- EuroMOMO("UK", 2015, 2019, 2020, 2021, "All")

ggplot() + 
  geom_bar(data=out, aes(WeekEnding, `Excess deaths`), stat="identity", fill = "darkorchid4") + 
  labs(x = "Date",
       y = "Excess deaths",
       title = "EuroMOMO: UK Excess Deaths (2020 and 2021)") +
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
  theme_minimal()




# Plots not used in the report
# 
# # EuroMOMO EW 2020 650x400 #############################################################
# 
# out <- EuroMOMO("EW", 2015, 2019, 2021, 2021, "All")
# 
# excess_EW <- ggplot() + 
#   geom_bar(data=out, aes(WeekNumber, `Excess deaths`), stat="identity", fill = "red3") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "England and Wales") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# deaths2019 <- deathdat[deathdat$Country == "EW" & 
#                    deathdat$Gender == "All" & 
#                    deathdat$Year == 2019, 
#                  c("Deaths", "WeekNumber")]
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
#   geom_line(data=out, aes(WeekNumber, `Actual deaths`, color = "Actual deaths")) +
#   geom_line(data=out, aes(WeekNumber, `Predicted deaths`, color = "Baseline deaths")) +
#   geom_hline(yintercept = 0) +
#   labs(x = "Week number",
#        y = "Deaths",
#        title = "EuroMOMO: England and Wales 2020") +
#   scale_color_manual(name="Legend", 
#                      values=c("grey75", "red4", "red1"),
#                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
#   scale_x_continuous(limit=c(0, 52)) +
#   theme_minimal()
# 
# 
# # EuroMOMO EW 2020 by sex (650x400)  ###########################################
# 
# out1 <- EuroMOMO("EW", 2015, 2019, 2015, 2021, "Female") 
# out2 <- EuroMOMO("EW", 2015, 2019, 2015, 2021, "Male")
# 
# ggplot() +
#   geom_line(data=out1, aes(WeekEnding, `Actual deaths`, color="Actual deaths - female")) +
#   geom_line(data=out2, aes(WeekEnding, `Actual deaths`, color="Actual deaths - male")) +
#   geom_line(data=out1, aes(WeekEnding, `Predicted deaths`, color="Baseline deaths - female")) +
#   geom_line(data=out2, aes(WeekEnding, `Predicted deaths`, color="Baseline deaths - male")) +
#   labs(x = "Date", 
#        y="Deaths", 
#        title = "EuroMOMO: England and Wales by Sex (2015 to 2021)") +
#   scale_color_manual(name = "Legend:", values=c("coral", "cornflowerblue", "brown", "blue"))+
#   geom_vline(xintercept=ymd(c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70")+
#   theme_minimal()
# 
# # Excess is EW NI S
# 
# out <- EuroMOMO("S", 2015, 2019, 2021, 2021, "All")
# 
# excess_S <- ggplot() + 
#   geom_bar(data=out, aes(WeekNumber, `Excess deaths`), stat="identity", fill = "royalblue") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "Scotland") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# out <- EuroMOMO("NI", 2015, 2019, 2021, 2021, "All")
# 
# excess_NI <- ggplot() + 
#   geom_bar(data=out, aes(WeekNumber, `Excess deaths`), stat="identity", fill = "darkgreen") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "Northern Ireland") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# out <- EuroMOMO("EW", 2015, 2019, 2021, 2021, "All")
# 
# excess_EW <- ggplot() + 
#   geom_bar(data=out, aes(WeekNumber, `Excess deaths`), stat="identity", fill = "darkred") + 
#   labs(x = "Week number",
#        y = "Excess deaths",
#        title = "England and Wales") +
#   scale_x_continuous(limit=c(0, 52))+
#   theme_minimal()
# 
# grid.arrange(excess_EW, excess_S, excess_NI, nrow=1, top=textGrob("EuroMOMO: Excess Deaths in 2021 for all UK nations"))
# 
# # EuroMOMO S 2020 (650x400) ####################################################################
# 
# # out <- EuroMOMO("S", 2015, 2019, 2020, 2021, "All")
# # 
# # ggplot() + 
# #   geom_line(data=out, aes(WeekEnding, `Actual deaths`, color = "Actual deaths")) +
# #   geom_line(data=out, aes(WeekEnding, `Predicted deaths`, color = "EuroMOMO baseline")) +
# #   labs(x = "Date", y="Deaths", title="EuroMOMO: Scotland 2019-2021") +
# #   scale_color_manual(name="Legend:", 
# #                      values=c("cornflowerblue", "blue"),
# #                      limits = c("Actual deaths", "EuroMOMO baseline")) +
# #   geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
# #   theme_minimal()
# 
# # 
# # 
# # deaths2019 <- deathdat[deathdat$Country == "S" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2019, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # deaths2018 <- deathdat[deathdat$Country == "S" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2018, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # deaths2017 <- deathdat[deathdat$Country == "S" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2017, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # ggplot() +
# #   geom_line(data = deaths2019, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data = deaths2018, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data = deaths2017, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data=out, aes(WeekNumber, `Actual deaths`, color = "Actual deaths")) +
# #   geom_line(data=out, aes(WeekNumber, `Predicted deaths`, color = "Baseline deaths")) +
# #   geom_hline(yintercept=0) +
# #   labs(x = "Week number",
# #        y = "Deaths",
# #        title = "EuroMOMO: Scotland 2020") +
# #   scale_color_manual(name="Legend", values=c("grey75", "cornflowerblue", "blue"),
# #                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
# #   scale_x_continuous(limit=c(0, 52))+
# #   theme_minimal()
# 
# # EuroMOMO NI 2020 (650x400) ##################################################
# 
# 
# 
# 
# # ggplot() + 
# #   geom_line(data=out, aes(WeekEnding, `Actual deaths`, color = "Actual deaths")) +
# #   geom_line(data=out, aes(WeekEnding, `Predicted deaths`, color = "EuroMOMO baseline")) +
# #   geom_hline(yintercept=0) +
# #   labs(x = "Date", y="Deaths", title="EuroMOMO: Scotland 2019-2021") +
# #   scale_color_manual(name="Legend:", 
# #                      values=c("green3", "darkgreen"),
# #                      limits = c("Actual deaths", "EuroMOMO baseline")) +
# #   geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), linetype="dashed", color="grey70") +
# #   theme_minimal()
# 
# 
# 
# # deaths2019 <- deathdat[deathdat$Country == "NI" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2019, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # deaths2018 <- deathdat[deathdat$Country == "NI" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2018, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # deaths2017 <- deathdat[deathdat$Country == "NI" & 
# #                          deathdat$Gender == "All" & 
# #                          deathdat$Year == 2017, 
# #                        c("Deaths", "WeekNumber")]
# # 
# # ggplot() +
# #   geom_line(data = deaths2019, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data = deaths2018, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data = deaths2017, aes(WeekNumber, Deaths, color = "Deaths 2017-2019")) +
# #   geom_line(data=out, aes(WeekNumber, `Actual deaths`, color = "Actual deaths")) +
# #   geom_line(data=out, aes(WeekNumber, `Predicted deaths`, color = "Baseline deaths")) +
# #   geom_hline(yintercept=0) +
# #   labs(x = "Week number",
# #        y = "Deaths",
# #        title = "EuroMOMO: Northern Ireland 2020") +
# #   scale_color_manual(name="Legend", 
# #                      values=c("grey75", "green3", "darkgreen"),
# #                      limits = c("Deaths 2017-2019", "Actual deaths", "Baseline deaths")) +
# #   scale_x_continuous(limit=c(0, 52))+
# #   theme_minimal()
# 
# #
# 
# 
# 
# 
