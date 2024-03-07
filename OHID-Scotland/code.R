# Load Packages
library(tidyverse)
library(timeDate)
library(furrr)
library(zoo)
library(readxl)
library(writexl)
library(lubridate)

years = 2015:2022

# Mid-year population
# Denominator data
mid_year_pop = read_xlsx('data/scotland_midyear_pop.xlsx')
mid_year_pop = mid_year_pop %>% 
  rename(mid_year_population = mid_pop)

# Mortality data
death = read_xlsx('data/scotland_death.xlsx',col_names = TRUE)
year_num_week_last_day = list()

for(i in seq_along(years)){
  
  temp_death = death %>% 
    filter(year==years[i]) %>% 
    select(year,num_week) %>% 
    distinct() %>% 
    mutate(last_day = NA)
  
  start_date = as.Date(str_c(years[i],'-01-01'))
  end_date = as.Date(str_c(years[i],'-12-31'))
  dates = seq.Date(from = start_date,
                   to = end_date,
                   by = 'day')
  first_last_day_index1 = which(weekdays(dates)=='Friday')[1]
  first_last_day_index2 = which(weekdays(dates)=='Friday')[2]
  if(month(dates[first_last_day_index1])==1 & day(dates[first_last_day_index1])==1){
    first_last_day_index = first_last_day_index2
  } else{
    first_last_day_index = first_last_day_index1
  }
  
  last_day = seq.Date(from = dates[first_last_day_index],
                      length.out = max(temp_death$num_week),
                      by = 'week')
  
  year_num_week_last_day[[i]] = temp_death %>% 
    mutate(last_day = seq.Date(from = dates[first_last_day_index],
                               length.out = max(temp_death$num_week),
                               by = 'week'))
}

year_num_week_last_day = do.call('rbind',year_num_week_last_day)

death = death %>% 
  filter(year %in% years) %>% 
  left_join(year_num_week_last_day,by = c('year','num_week')) %>% 
  mutate(mid_day = last_day-3)

# Merge Data
plan(strategy = 'multisession',workers = 4)
death = death %>% 
  left_join(mid_year_pop,by = c('year')) %>% 
  mutate(linear_trend = future_map_dbl(.x = last_day,
                                       .f = ~ mean(seq.Date(from = .x-6,to = .x,by = 'day')-as.Date('2016-12-31'))/52),
         linear_trend = as.numeric(linear_trend),
         holiday = future_map2_lgl(.x = last_day,
                                   .y = year,
                                   .f = ~ any(seq.Date(from = .x-6,to = .x,by = 'day') %in% as.Date(holidayLONDON(.y)))),
         holiday = as.numeric(holiday))

# Populations for each breakdown by week are estimated by smoothing the changes in population between the years, to avoid step changes.
year_num_week = death %>% 
  select(year,num_week) %>% 
  distinct()

smooth_mid_year_pop = death %>% 
  select(year,mid_year_population) %>% 
  distinct() %>% 
  mutate(num_week = 26) %>%
  right_join(year_num_week,by = c('year','num_week')) %>% 
  arrange(year,num_week) %>% 
  mutate(mid_year_population = na.spline(mid_year_population)) %>% 
  arrange(year,num_week)

write_csv(smooth_mid_year_pop,'smooth_mid_year_pop.csv')

final_data = death %>% 
  select(-mid_year_population) %>% 
  full_join(smooth_mid_year_pop,by = c('year','num_week')) |> 
  arrange(year,last_day)

write_rds(final_data,'final_data.rds')
final_data = read_rds('final_data.rds')

# Data Split
model_data = final_data |> 
  filter(year<=2019)
  
predict_data = final_data |> 
  filter(year>2019)

# Quasi-Poisson regression model
model1 = glm(death ~ holiday + factor(month(last_day)) + linear_trend + num_week,
             family = quasipoisson(link = 'log'),
             offset = log(mid_year_population),
             data = model_data)
summary(model1)

# Prediction
rqpois = function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu/(theta-1))
}

predict_result = predict(model1,newdata = predict_data,type = 'response')

final_data = final_data %>% 
  mutate(fit_death = c(model1$fitted.values,predict_result),
         excess_death = death-fit_death,
         lower = NA,
         upper = NA)

set.seed(1)
for(i in 1:nrow(final_data)){
  random_sample = rqpois(n = 5000,mu = final_data$fit_death[i],theta = summary(model1)$dispersion)
  final_data$lower[i] = quantile(random_sample,probs = 0.001)
  final_data$upper[i] = quantile(random_sample,probs = 0.999)
}

final_data %>% 
  select(year,
         num_week,
         true_death = death,
         fit_death,
         lower,
         upper,
         excess_death) %>% 
  write_csv('excess_death.csv')

weekly_excess_deaths <- final_data %>%
  group_by(year, num_week) %>%
  summarise(total_deaths = sum(fit_death)) %>%
  write_csv('weekly_excess_death.csv')

# Display the first few rows of the result
head(weekly_excess_deaths)


# Visualization
p = final_data %>% 
  mutate(bias = if_else(excess_death>0,'positive','negative')) %>% 
  filter(year==2020) %>% 
  ggplot(aes(x = last_day,y = excess_death,fill = bias)) +
  geom_col() +
  scale_fill_brewer(palette = 'Set1',direction = -1) +
  scale_x_date(date_breaks = '1 month', date_labels = '%Y-%m') +
  labs(x = 'Date',
       y = 'Excess Mortality',
       title = 'Excess Mortality in Scotland') +
  theme_light() +
  theme(legend.position = 'none')
ggsave(filename = str_c('plot/scotland.jpg'),
       plot = p,
       width = 7500,
       height = 2500,
       units = 'px',
       dpi = 600)

final_data |> 
  group_by(year) |> 
  summarise(truth_death = sum(death),
            fit_death = sum(fit_death),
            diff_death = truth_death-fit_death) |> 
  ungroup()

data_filtered <- final_data %>% filter(year %in% c(2020, 2021)) %>%
  group_by(year, num_week) %>%
  summarise(true_death = sum(death), fit_death = sum(fit_death)) %>%
  pivot_longer(cols = c(true_death, fit_death), names_to = "Type", values_to = "Deaths")



ggplot(data_filtered, aes(x=num_week, y=Deaths, color=Type)) +
  geom_line() +
  facet_wrap(~year, ncol=1, scales="free_x") +
  labs(title="True vs Fit Death Comparison for 2020 and 2021(Scotland)", x="Week Number", y="Number of Deaths") +
  scale_color_manual(values = c("true_death"="red", "fit_death"="blue")) +
  theme_minimal()
ggsave(filename = str_c('plot/','True vs Fit Death Comparison for 2020 and 2021(Scotland).jpg'),
       width = 7500,
       height = 2500,
       units = 'px',
       dpi = 600)

final_data <- final_data %>%
  mutate(date = ymd(last_day), 
         week_end = floor_date(date, "week"))


data_filtered <- final_data %>%
  filter(year(date) %in% c(2020, 2021))


weekly_excess_deaths <- data_filtered %>%
  group_by(week_end) %>%
  summarise(excess_death_sum = sum(excess_death, na.rm = TRUE))


p <- ggplot(weekly_excess_deaths, aes(x = week_end, y = excess_death_sum)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Weekly Excess Deaths (2020-2021)",
       x = "Week End",
       y = "Excess Deaths") +
  theme_minimal()


print(p)

ggsave(filename = "weekly_excess_deaths_2020_2021(Scotland).jpg", plot = p, width = 15, height = 6, units = "in", dpi = 300)



