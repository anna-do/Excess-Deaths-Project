install.packages("tidyverse")
install.packages("readxl")
install.packages("segmented")

library(tidyverse)
library(readxl)
library(segmented)


engdat <- read_excel("referencetables09032020145206.xls",
                  sheet = "England ASRs", 
                  col_names = FALSE, 
                  skip = 17)

engdat <- engdat %>% select(-8, -13, -18, -23, -28) 


time <- c(seq(from = 1991, to = 2019, by = 0.25), seq(from = 1991, to = 2019, by = 0.25))

engdat <- bind_cols(engdat, time)

colnames(engdat) <- c("Country", "Sex", "Quarter", 
                     "AllAgeDeaths","AllAgeASR", "AllAgeLwr", "AllAgeUpr",
                     "U75Deaths", "U75ASR", "U75Lwr", "U75Upr",
                     "7579Deaths", "7579ASR", "7579Lwr", "7579Upr",
                     "8084Deaths", "8084ASR", "8084Lwr", "8084Upr",
                     "8590Deaths", "8590ASR", "8590Lwr", "8590Upr",
                     "90Deaths", "90ASR", "90Lwr", "90Upr",
                     "EndDate")


### Some common sense checks

ggplot(engdat, aes(EndDate, AllAgeASR,color=Sex)) + geom_point() + stat_smooth(method="lm")

fitdat <- engdat %>% filter(Sex == "Females") 
fit <- lm(AllAgeASR ~ EndDate , data = fitdat)
summary(segmented(fit))

AgeCats <- c()


