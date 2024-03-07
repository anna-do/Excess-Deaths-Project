# Load Packages
library(tidyverse)
library(timeDate)
library(furrr)
library(zoo)
library(readxl)
library(writexl)
library(dplyr)
library(scales)
library(lubridate)

years = 2015:2022

# Mid-year population
# Denominator data
mid_year_pop = read_xlsx('data/mid-year-population.xlsx')
mid_year_pop = mid_year_pop %>% 
  mutate(Age = parse_number(Age),
         Age = case_when(
           between(Age,0,14) ~ '0 to 14',
           between(Age,15,44) ~ '15 to 44',
           between(Age,45,64) ~ '45 to 64',
           between(Age,65,74) ~ '65 to 74',
           between(Age,75,84) ~ '75 to 84',
           Age>=85 ~ '85+')) %>% 
  pivot_longer(cols = c(All,Females,Males),
               names_to = 'Sex',
               values_to = 'value') %>% 
  mutate(
    Sex = case_when(
      Sex=='All' ~ 'Combine',
      Sex=='Females' ~ 'Female',
      Sex=='Males' ~ 'Male'),
    Location = 'EngWal') %>% 
  group_by(Year,Location,Sex,Age) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(Year,Location,Sex),
              names_from = Age,
              values_from = value) %>% 
  filter(Sex!='Combine') %>% 
  mutate(Sex = str_to_lower(Sex)) %>% 
  select(-Location) %>% 
  pivot_longer(cols = -c(Year,Sex),
               names_to = 'age',
               values_to = 'mid_year_population') %>% 
  rename(year = Year,
         gender = Sex) %>%
  mutate(age = factor(age,levels = c('0 to 14',
                                     '15 to 44',
                                     '45 to 64',
                                     '65 to 74',
                                     '75 to 84',
                                     '85+'))) %>% 
  arrange(year,age,gender)

# Mortality data
death_male = read_xlsx('data/Weekly_deaths_group.xlsx',sheet = 'Male',range = 'A5:V579',col_names = TRUE)

death_male = death_male %>% 
  select(-`All ages`) %>% 
  pivot_longer(cols = -`Year & week number`,
               names_to = 'age',
               values_to = 'death') %>% 
  mutate(year = str_extract(`Year & week number`,'[[:digit:]]{4}'),
         year = as.numeric(year),
         num_week = str_extract(`Year & week number`,'-.+'),
         num_week = str_remove(num_week,'-'),
         num_week = parse_number(num_week)) %>% 
  select(-`Year & week number`) %>% 
  select(year,num_week,age,death)

year_num_week_last_day = list()

for(i in seq_along(years)){
  
  temp_death_male = death_male %>% 
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
                      length.out = max(temp_death_male$num_week),
                      by = 'week')
  
  year_num_week_last_day[[i]] = temp_death_male %>% 
    mutate(last_day = seq.Date(from = dates[first_last_day_index],
                               length.out = max(temp_death_male$num_week),
                               by = 'week'))
}

year_num_week_last_day = do.call('rbind',year_num_week_last_day)

death_male = death_male %>% 
  filter(year %in% years) %>% 
  left_join(year_num_week_last_day,by = c('year','num_week')) %>% 
  mutate(mid_day = last_day-3)

death_female = read_xlsx('data/Weekly_deaths_group.xlsx',sheet = 'Female',range = 'A5:V579',col_names = TRUE)

death_female = death_female %>% 
  select(-`All ages`) %>% 
  pivot_longer(cols = -`Year & week number`,
               names_to = 'age',
               values_to = 'death') %>% 
  mutate(year = str_extract(`Year & week number`,'[[:digit:]]{4}'),
         year = as.numeric(year),
         num_week = str_extract(`Year & week number`,'-.+'),
         num_week = str_remove(num_week,'-'),
         num_week = parse_number(num_week)) %>% 
  select(-`Year & week number`) %>% 
  select(year,num_week,age,death)

year_num_week_last_day = list()

for(i in seq_along(years)){
  
  temp_death_female = death_female %>% 
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
                      length.out = max(temp_death_female$num_week),
                      by = 'week')
  
  year_num_week_last_day[[i]] = temp_death_female %>% 
    mutate(last_day = seq.Date(from = dates[first_last_day_index],
                               length.out = max(temp_death_female$num_week),
                               by = 'week'))
}

year_num_week_last_day = do.call('rbind',year_num_week_last_day)

death_female = death_female %>% 
  filter(year %in% years) %>% 
  left_join(year_num_week_last_day,by = c('year','num_week')) %>% 
  mutate(mid_day = last_day-3)

death = bind_rows(death_male %>% mutate(gender = 'male'),
                  death_female %>% mutate(gender = 'female')) %>% 
  select(year,num_week,last_day,age,gender,death) %>%
  mutate(age = case_when(
    age %in% c('<1','01-04','05-09','10-14') ~ '0 to 14',
    age %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15 to 44',
    age %in% c('45-49','50-54','55-59','60-64') ~ '45 to 64',
    age %in% c('65-69','70-74') ~ '65 to 74',
    age %in% c('75-79','80-84') ~ '75 to 84',
    age %in% c('85-89','90+') ~ '85+'),
    age = factor(age,levels = c('0 to 14',
                                '15 to 44',
                                '45 to 64',
                                '65 to 74',
                                '75 to 84',
                                '85+'))) %>% 
  group_by(year,num_week,last_day,age,gender) %>% 
  summarise(death = sum(death)) %>% 
  ungroup() %>% 
  arrange(year,num_week,age,gender)

# Merge Data
plan(strategy = 'multisession',workers = 4)
death = death %>% 
  left_join(mid_year_pop,by = c('year','age','gender')) %>% 
  mutate(linear_trend = future_map_dbl(.x = last_day,
                                       .f = ~ mean(seq.Date(from = .x-6,to = .x,by = 'day')-as.Date('2016-12-31'))/52),
         linear_trend = as.numeric(linear_trend),
         holiday = future_map2_lgl(.x = last_day,
                                   .y = year,
                                   .f = ~ any(seq.Date(from = .x-6,to = .x,by = 'day') %in% as.Date(holidayLONDON(.y)))),
         holiday = as.numeric(holiday))

# Populations for each breakdown by week are estimated by smoothing the changes in population between the years, to avoid step changes.
smooth_mid_year_pop = list()

age_gender = death %>% 
  select(age,gender) %>% 
  distinct()

year_num_week = death %>% 
  select(year,num_week) %>% 
  distinct()

for(i in 1:nrow(age_gender)){
  
  smooth_mid_year_pop[[i]] = death %>% 
    select(year,age,gender,mid_year_population) %>% 
    distinct() %>% 
    mutate(num_week = 26) %>% 
    filter(age==age_gender$age[i],
           gender==age_gender$gender[i]) %>% 
    right_join(year_num_week,by = c('year','num_week')) %>% 
    arrange(year,num_week) %>% 
    mutate(mid_year_population = na.spline(mid_year_population),
           age = age_gender$age[i],
           gender = age_gender$gender[i])
  
}

smooth_mid_year_pop = do.call('rbind',smooth_mid_year_pop)
write_csv(smooth_mid_year_pop,'smooth_mid_year_pop.csv')

final_data = death %>% 
  select(-mid_year_population) %>% 
  full_join(smooth_mid_year_pop,by = c('year','num_week','age','gender')) |> 
  arrange(year,last_day,age,gender)

write_rds(final_data,'final_data.rds')
final_data = read_rds('final_data.rds')

# Data Split
model_data = final_data |> 
  filter(year<=2019)
  
predict_data = final_data |> 
  filter(year>2019)

# Quasi-Poisson regression model
model1 = glm(death ~ holiday+ factor(month(last_day)) + linear_trend + age * gender + age * num_week,
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
         age,
         gender,
         true_death = death,
         fit_death,
         lower,
         upper,
         excess_death) %>% 
  write_csv('excess_death.csv')


# Visualization
for(i in 1:nrow(age_gender)){
  p = final_data %>% 
    filter(age==age_gender$age[i],
           gender==age_gender$gender[i]) %>% 
    mutate(bias = if_else(excess_death>0,'positive','negative')) %>% 
    filter(year==2020) %>% 
    ggplot(aes(x = last_day,y = excess_death,fill = bias)) +
    geom_col() +
    scale_fill_brewer(palette = 'Set1',direction = -1) +
    scale_x_date(date_breaks = '1 month', date_labels = '%Y-%m') +
    labs(x = 'Date',
         y = 'Excess Mortality',
         title = 'Excess Mortality in England',
         subtitle = str_c('age:',age_gender$age[i],' gender:',age_gender$gender[i])) +
    theme_light() +
    theme(legend.position = 'none')
  ggsave(filename = str_c('plot/',age_gender$age[i],'_',age_gender$gender[i],'.jpg'),
         plot = p,
         width = 7500,
         height = 2500,
         units = 'px',
         dpi = 600)
}

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
  labs(title="True vs Fit Death Comparison for 2020 and 2021(England and Wales)", x="Week Number", y="Number of Deaths") +
  scale_color_manual(values = c("true_death"="red", "fit_death"="blue")) +
  theme_minimal()
ggsave(filename = str_c('plot/','True vs Fit Death Comparison for 2020 and 2021(England and Wales.jpg'),
       width = 7500,
       height = 2500,
       units = 'px',
       dpi = 600)


data_filtered <- final_data %>%
  filter(year %in% c(2020, 2021), !is.na(excess_death)) %>%
  mutate(age_group = factor(paste(age, gender), levels = unique(paste(age, gender))))


p <- ggplot(data_filtered, aes(x = last_day, y = excess_death, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2020" = "blue", "2021" = "red"), name = "Year") +
  labs(x = "Date", y = "Excess Death", title = "Excess Death by Age Group and Gender (2020-2021)") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  
    axis.text.y = element_text(color = "black"),  
    axis.title = element_text(size = 12, color = "black"),  
    plot.title = element_text(size = 16, color = "black", face = "bold"),  
    plot.subtitle = element_text(color = "black"),  
    strip.text = element_text(size = 12, color = "black")  
  ) +
  facet_wrap(~age_group, scales = "free_y", ncol = 2)  



ggsave(filename = "excess_mortality_by_gender_and_year.jpg",
       plot = p,
       width = 10, height = 15, units = "in", dpi = 300)


final_data <- final_data %>%
  mutate(date = ymd(last_day), 
         week_start = floor_date(date, "week"))


data_filtered <- final_data %>%
  filter(year(date) %in% c(2020, 2021))


weekly_excess_deaths <- data_filtered %>%
  group_by(week_start) %>%
  summarise(excess_death_sum = sum(excess_death, na.rm = TRUE)) %>%
  write_csv('weekly_excess_death.csv')




p <- ggplot(weekly_excess_deaths, aes(x = week_start, y = excess_death_sum)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Weekly Excess Deaths (2020-2021)",
       x = "Week Starting",
       y = "Excess Deaths") +
  theme_minimal()


print(p)


ggsave(filename = "weekly_excess_deaths_2020_2021.jpg", plot = p, width = 15, height = 6, units = "in", dpi = 300)

