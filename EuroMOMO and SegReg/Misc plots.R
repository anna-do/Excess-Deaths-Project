library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)
library(segmented)

deathdat <- read_csv("Deaths combined.csv")
population <- read_csv("Population combined.csv")

###

EWdeaths <- deathdat[deathdat$Gender == "All" & 
                      deathdat$WeekEnding <= ymd("2021-01-01") & 
                      deathdat$WeekEnding >= ymd("2018-12-01") &
                      deathdat$Country == "EW", ]

Sdeaths <- deathdat[deathdat$Gender == "All" & 
                       deathdat$WeekEnding <= ymd("2021-01-01") & 
                       deathdat$WeekEnding >= ymd("2018-12-01") &
                       deathdat$Country == "S", ]

NIdeaths <- deathdat[deathdat$Gender == "All" & 
                      deathdat$WeekEnding <= ymd("2021-01-01") & 
                      deathdat$WeekEnding >= ymd("2018-12-01") &
                      deathdat$Country == "NI", ]

p1 <- ggplot(data=EWdeaths) + 
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), color="grey") +
  geom_rect(data = EWdeaths,
            inherit.aes = FALSE,
            mapping = aes(xmin = ymd("2020-03-23"), xmax = ymd("2020-06-01"),
                          ymin = 5000, ymax = max(Deaths) + 1000),
            color = "transparent",
            fill = "grey75",
            alpha = .01) +
  geom_line(aes(WeekEnding, Deaths), color="red3") + 
  ggtitle("England and Wales") +
  labs(xlabel="Week End-date", ylabel="Deaths") +
  theme_minimal() 

p2 <- ggplot(data=Sdeaths) + 
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), color="grey") +
  geom_rect(data = Sdeaths,
            inherit.aes = FALSE,
            mapping = aes(xmin = ymd("2020-03-23"), xmax = ymd("2020-06-01"),
                          ymin = 500, ymax = max(Deaths) + 10),
            color = "transparent",
            fill = "grey75",
            alpha = .01) +
  geom_line(aes(WeekEnding, Deaths), color="royalblue3") + 
  theme_minimal() +
  ggtitle("Scotland") +
  labs(xlabel="Week End-date", ylabel="Deaths")

p3 <- ggplot(data=NIdeaths) +
  geom_vline(xintercept=ymd(c("2019-01-01", "2020-01-01", "2021-01-01")), color="grey") +
  geom_rect(data = NIdeaths,
            inherit.aes = FALSE,
            mapping = aes(xmin = ymd("2020-03-23"), xmax = ymd("2020-06-01"),
                          ymin = 100, ymax = max(Deaths)),
            color = "transparent",
            fill = "grey75",
            alpha = .01) +
  geom_line(aes(WeekEnding, Deaths), color="seagreen") + 
  theme_minimal() +
  ggtitle("Northern Ireland") +
  labs(title="Northern Ireland", xlabel="Week End-date", ylabel="Deaths")

grid.arrange(p1, p2, p3, nrow=3, top=textGrob("Weekly Deaths in the UK Nations (2019 to 2022)"))

## Segreg example #############################

X <- c(0:19)
Y <- c(seq(0, 9, by = 1), seq(10, 39, by = 3)) + rnorm(20, mean = 0, sd = 1)
df <- data.frame(X, Y)

mod <- lm(Y ~ X, data = df)
segmod <- segmented(mod)
modline <- predict.segmented(X = 0:19, segmod)


ggplot() +
  geom_point(data=df, aes(X, Y, color="red"), show.legend = FALSE) +
  geom_line(data=data.frame(X, modline), aes(X, modline), show.legend = FALSE) +
  labs(title="Example of Segmented Regression")+
  theme_minimal()
  
