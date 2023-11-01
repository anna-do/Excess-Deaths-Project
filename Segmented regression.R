install.packages("tidyverse")
install.packages("readxl")
install.packages("segmented")

library(tidyverse)
library(readxl)
library(segmented)


# SETUP ################################################

engdat <- read_excel("Test data for segmented reg.xls",
                  sheet = "England ASRs", 
                  col_names = FALSE, 
                  skip = 17)

engdat <- engdat %>% dplyr::select(-8, -13, -18, -23, -28) 

time <- c(seq(from = 1991, to = 2019, by = 0.25), seq(from = 1991, to = 2019, by = 0.25))

engdat <- bind_cols(engdat, time)

colnames(engdat) <- c("Country", "Sex", "Quarter", 
                     "AllAgeDeaths","AllAgeASR", "AllAgeLwr", "AllAgeUpr",
                     "U75Deaths", "U75ASR", "U75Lwr", "U75Upr",
                     "Deaths7579", "ASR7579", "Lwr7579", "Upr7579",
                     "Deaths8084", "ASR8084", "Lwr8084", "Upr8084",
                     "Deaths8590", "ASR8590", "Lwr8590", "Upr8590",
                     "Deaths90", "ASR90", "Lwr90", "Upr90",
                     "EndDate")



### Some common sense checks #################################

ggplot(engdat, aes(EndDate, ASR90,color=Sex)) + geom_point() + stat_smooth(method="lm") 

fitdat <- engdat %>% filter(Sex == "Females") 
plot(fitdat$EndDate, fitdat$ASR90)
fit <- lm(ASR90 ~ EndDate , data = fitdat)
summary(segmented(fit))
plot(segmented(fit))



# COMPARING WITH ONS DATA #############################################

# The ONS used this data and this method to calculate breakpoints for all age-groups
# and both sexes. Here we carry out the same method and compare results with their,
# to check we're doing things correctly.

Sex <- c("Males", "Females")
Age <- c("AllAgeASR","U75ASR", "ASR7579","ASR8084", "ASR8590","ASR90")
EnglandTest <- data.frame()

for (i in Sex){
  for(j in Age){
    fitdat <- engdat %>% filter(Sex == i) 
    fit <- lm(paste(j, "~ EndDate"), data = fitdat)
    seg <- segmented(fit)
    out <- c("England", i, j, seg$psi[2])
    EnglandTest <- rbind(EnglandTest, out)
  }
}

colnames(EnglandTest) <- c("Country", "Sex", "Age", "Breakpoint")

print(EnglandTest)

ggplot(engdat, aes(EndDate, ASR90,color=Sex)) + geom_point() + stat_smooth(method="lm") 



### Predicting #################################################

# Data for fitting the model.
fit_start <- 2005
fit_end <- 2015

# Predicting deaths for all of 2018 for females
pred_start <- 2016
pred_end <- 2017
sex_choice <- "Females"

# fitdat is the data used to fit the model. fit is the fitted lm. segmod is the segmented model.
fitdat <- engdat %>% filter(Sex == sex_choice, EndDate <= fit_end, EndDate >= fit_start)
fit <- lm(AllAgeDeaths ~ EndDate , data = fitdat)
segmod <- segmented(fit)
plot(segmod)

# The years we are predicting for
preddat <- data.frame(seq(pred_start, pred_end, by= 0.25))
colnames(preddat) <- c("EndDate")

predvals <- round(predict.segmented(segmod, newdata = preddat))
dates <- preddat$EndDate
truevals <- (engdat %>% filter(Sex==sex_choice, EndDate >= pred_start, EndDate <= pred_end))$AllAgeDeaths
difference <- abs(truevals - predvals)

output <- cbind(dates, predvals, truevals, difference)
output

### Same thing as a function

segreg <- function(fit_start, fit_end, pred_start, pred_end, sex_choice){
  
  fitdat <- engdat %>% filter(Sex == sex_choice, EndDate <= fit_end, EndDate >= fit_start)
  fit <- lm(AllAgeDeaths ~ EndDate , data = fitdat)
  segmod <- segmented(fit)
  
  preddat <- data.frame(seq(pred_start, pred_end, by= 0.25))
  colnames(preddat) <- c("EndDate")
  
  predvals <- round(predict.segmented(segmod, newdata = preddat))
  dates <- preddat$EndDate
  truevals <- (engdat %>% filter(Sex==sex_choice, EndDate >= pred_start, EndDate <= pred_end))$AllAgeDeaths
  difference <- abs(truevals - predvals)
  
  output <- cbind(dates, predvals, truevals, difference)
  
  return(output)
}

segreg(2005, 2015, 2016, 2017, "Females")
