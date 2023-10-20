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


### Some common sense checks

ggplot(engdat, aes(EndDate, ASR90,color=Sex)) + geom_point() + stat_smooth(method="lm") 

fitdat <- engdat %>% filter(Sex == "Females") 
fit <- lm(ASR90 ~ EndDate , data = fitdat)
summary(segmented(fit))
plot(segmented(fit))
points(fitdat$EndDate, fitdat$ASR90, color=)

###

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

