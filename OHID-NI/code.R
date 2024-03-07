# Load Packages
library(tidyverse)
library(timeDate)
library(furrr)
library(zoo)
library(readxl)
library(writexl)
library(scales)
library(lubridate)

years = 2015:2022

# Mid-year population
# Denominator data
mid_year_pop = read_xlsx('data/NI mid_year_pop.xlsx')
mid_year_pop = mid_year_pop %>% 
  mutate(age = factor(age,levels = c('0 to 14',
                                     '15 to 44',
                                     '45 to 64',
                                     '65 to 74',
                                     '75 to 84',
                                     '85+')))

# Mortality data
death = read_xlsx('data/NI _death_pop.xlsx',col_names = TRUE)
death = death %>% 
  rename(num_week = `Registration Week`,
         last_day = `Week Ends (Friday)`) %>% 
  select(-`Week Starts (Saturday)`) %>% 
  mutate(last_day = ymd(last_day),
         year = case_when(
           num_week==1 ~ year(last_day),
           TRUE ~ NA)) %>% 
  fill(year,.direction = 'down') %>% 
  mutate(num_week = parse_number(num_week)) %>% 
  pivot_longer(cols = -c(year,num_week,last_day),
               names_to = 'age',
               values_to = 'death') %>% 
  mutate(age = case_when(
    age %in% c('Aged <7 days','Aged >=7 days and < 1 year','Aged 1-14 years') ~ '0 to 14',
    age=='Aged 15-44 years' ~ '15 to 44',
    age=='Aged 45-64 years' ~ '45 to 64',
    age=='Aged 65-74 years' ~ '65 to 74',
    age=='Aged 75-84 years' ~ '75 to 84',
    age=='Aged 85+ years' ~ '85+'),
    age = factor(age,levels = c('0 to 14',
                                '15 to 44',
                                '45 to 64',
                                '65 to 74',
                                '75 to 84',
                                '85+'))) %>% 
  group_by(year,num_week,last_day,age) %>% 
  summarise(death = sum(death)) %>% 
  ungroup() %>% 
  select(year,num_week,last_day,age,death) %>% 
  arrange(year,num_week,age)

# Merge Data
plan(strategy = 'multisession',workers = 4)
death = death %>% 
  left_join(mid_year_pop,by = c('year','age')) %>% 
  mutate(linear_trend = future_map_dbl(.x = last_day,
                                       .f = ~ mean(seq.Date(from = .x-6,to = .x,by = 'day')-as.Date('2016-12-31'))/52),
         linear_trend = as.numeric(linear_trend),
         holiday = future_map2_lgl(.x = last_day,
                                   .y = year,
                                   .f = ~ any(seq.Date(from = .x-6,to = .x,by = 'day') %in% as.Date(holidayLONDON(.y)))),
         holiday = as.numeric(holiday))

# Populations for each breakdown by week are estimated by smoothing the changes in population between the years, to avoid step changes.
smooth_mid_year_pop = list()

age_tab = death %>% 
  select(age) %>% 
  distinct()

year_num_week = death %>% 
  select(year,num_week) %>% 
  distinct()

for(i in 1:nrow(age_tab)){
  
  smooth_mid_year_pop[[i]] = death %>% 
    select(year,age,mid_year_population) %>% 
    distinct() %>% 
    mutate(num_week = 26) %>% 
    filter(age==age_tab$age[i]) %>% 
    right_join(year_num_week,by = c('year','num_week')) %>% 
    arrange(year,num_week) %>% 
    mutate(mid_year_population = na.spline(mid_year_population),
           age = age_tab$age[i])
  
}

smooth_mid_year_pop = do.call('rbind',smooth_mid_year_pop)
smooth_mid_year_pop = smooth_mid_year_pop %>% 
  arrange(year,age,num_week)
write_csv(smooth_mid_year_pop,'smooth_mid_year_pop.csv')

final_data = death %>% 
  select(-mid_year_population) %>% 
  full_join(smooth_mid_year_pop,by = c('year','num_week','age')) |> 
  arrange(year,last_day,age)

write_rds(final_data,'final_data.rds')
final_data = read_rds('final_data.rds')

# Data Split
model_data = final_data |> 
  filter(year<=2019)
  
predict_data = final_data |> 
  filter(year>2019)

# Quasi-Poisson regression model
model1 = glm(death ~ holiday + factor(month(last_day)) + linear_trend + age * num_week,
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
         true_death = death,
         fit_death,
         lower,
         upper,
         excess_death) %>% 
  write_csv('excess_death.csv')


# Visualization
for(i in 1:nrow(age_tab)){
  p = final_data %>% 
    filter(age==age_tab$age[i]) %>% 
    mutate(bias = if_else(excess_death>0,'positive','negative')) %>% 
    filter(year==2020) %>% 
    ggplot(aes(x = last_day,y = excess_death,fill = bias)) +
    geom_col() +
    scale_fill_brewer(palette = 'Set1',direction = -1) +
    scale_x_date(date_breaks = '1 month', date_labels = '%Y-%m') +
    labs(x = 'Date',
         y = 'Excess Mortality',
         title = 'Excess Mortality in NI',
         subtitle = str_c('age:',age_tab$age[i])) +
    theme_light() +
    theme(legend.position = 'none')
  ggsave(filename = str_c('plot/',age_tab$age[i],'.jpg'),
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
  labs(title="True vs Fit Death Comparison for 2020 and 2021(Northern Ireland)", x="Week Number", y="Number of Deaths") +
  scale_color_manual(values = c("true_death"="red", "fit_death"="blue")) +
  theme_minimal()
ggsave(filename = str_c('plot/','True vs Fit Death Comparison for 2020 and 2021(Northern Ireland).jpg'),
       width = 7500,
       height = 2500,
       units = 'px',
       dpi = 600)

final_data <- final_data %>%
  mutate(date = ymd(last_day), 
         week_end = floor_date(date, "week"))

# select 2020 and 2021 data
data_filtered <- final_data %>%
  filter(year(date) %in% c(2020, 2021))

# calculate the sum of weekly excess death
weekly_excess_deaths <- data_filtered %>%
  group_by(week_end) %>%
  summarise(excess_death_sum = sum(excess_death, na.rm = TRUE))


p <- ggplot(weekly_excess_deaths, aes(x = week_end, y = excess_death_sum)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Weekly Excess Deaths (2020-2021)",
       x = "Week end",
       y = "Excess Deaths") +
  theme_minimal()


print(p)


ggsave(filename = "weekly_excess_deaths_2020_2021(NI).jpg", plot = p, width = 15, height = 6, units = "in", dpi = 300)



data_filtered <- final_data %>%
  filter(year %in% c(2020, 2021), !is.na(excess_death)) %>%
  mutate(age_group = factor(age))  


p <- ggplot(data_filtered, aes(x = last_day, y = excess_death, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1", name = "Year") +
  labs(x = "Date", y = "Excess Death", title = "Excess Death by Age Group (2020-2021)") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  
    axis.text.y = element_text(color = "black"),  
    axis.title = element_text(color = "black"),  
    plot.title = element_text(color = "black", size = 16),  
    plot.subtitle = element_text(color = "black"),  
    strip.text = element_text(size = 14, color = "black", face = "bold")  
  ) +
  facet_wrap(~age_group, scales = "free_y", ncol = 1)  

ggsave(filename = "excess_mortality_by_age_group(Northern Ireland).jpg",
       plot = p,
       width = 10, height = 15, units = "in", dpi = 300)


