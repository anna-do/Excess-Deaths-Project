# Load Pacakges
library(tidyverse)
library(readxl)
library(timeDate)
library(scales)

# Import Data
years = 2014:2022

## E(x,g,y)
mid_year_pop = read_xlsx('data/Population_yearly_1521.xlsx',sheet = 1,col_names = TRUE)
mid_year_pop = mid_year_pop %>% 
  filter(Sex!='Combine',
         Year %in% years) %>% 
  mutate(Sex = str_to_lower(Sex)) %>% 
  select(-Location) %>% 
  pivot_longer(cols = -c(Year,Sex),
               names_to = 'age',
               values_to = 'mid_year_population') %>%
  mutate(
    age = case_when(
      age %in% c('<1','01-04','5-9','10-14') ~ '0 to 14',
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
  rename(year = Year,
         gender = Sex) %>% 
  group_by(year,age,gender) %>% 
  summarise(mid_year_population = sum(mid_year_population)) %>% 
  arrange(year,age,gender)
  
## P(x,g)
std_pop = read_csv('data/european_standard_population_by_sex.csv')
std_pop = std_pop %>% 
  mutate(AgeGroup = str_remove(AgeGroup,' years')) %>%
  mutate(
    age = case_when(
      AgeGroup %in% c('0-4','5-9','10-14') ~ '0 to 14',
      AgeGroup %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15 to 44',
      AgeGroup %in% c('45-49','50-54','55-59','60-64') ~ '45 to 64',
      AgeGroup %in% c('65-69','70-74') ~ '65 to 74',
      AgeGroup %in% c('75-79','80-84') ~ '75 to 84',
      AgeGroup %in% c('85-89','90plus') ~ '85+')) %>% 
  mutate(age = factor(age,
                      labels = levels(mid_year_pop$age)),
         Sex = str_to_lower(Sex)) %>% 
  rename('gender' = 'Sex') %>% 
  select(-AgeGroup) %>% 
  group_by(age,gender) %>% 
  summarise(EuropeanStandardPopulation = sum(EuropeanStandardPopulation)) %>% 
  ungroup()

## D(x,g,i)
death_male = read_xlsx('data/Weekly_deaths_group.xlsx',sheet = 'Male',range = 'A5:V579',col_names = TRUE)

death_male = death_male %>% 
  select(-`All ages`) %>% 
  pivot_longer(cols = -`Year & week number`,
               names_to = 'age',
               values_to = 'death') %>% 
  mutate(
    age = case_when(
      age %in% c('<1','01-04','05-09','10-14') ~ '0 to 14',
      age %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15 to 44',
      age %in% c('45-49','50-54','55-59','60-64') ~ '45 to 64',
      age %in% c('65-69','70-74') ~ '65 to 74',
      age %in% c('75-79','80-84') ~ '75 to 84',
      age %in% c('85-89','90+') ~ '85+')) %>% 
  mutate(year = str_extract(`Year & week number`,'[[:digit:]]{4}'),
         year = as.numeric(year),
         num_week = str_extract(`Year & week number`,'-.+'),
         num_week = str_remove(num_week,'-'),
         num_week = parse_number(num_week)) %>% 
  select(-`Year & week number`) %>% 
  select(year,num_week,age,death) %>% 
  group_by(year,num_week,age) %>% 
  summarise(death = sum(death)) %>% 
  ungroup()

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
  mutate(
    age = case_when(
      age %in% c('<1','01-04','05-09','10-14') ~ '0 to 14',
      age %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15 to 44',
      age %in% c('45-49','50-54','55-59','60-64') ~ '45 to 64',
      age %in% c('65-69','70-74') ~ '65 to 74',
      age %in% c('75-79','80-84') ~ '75 to 84',
      age %in% c('85-89','90+') ~ '85+')) %>% 
  mutate(year = str_extract(`Year & week number`,'[[:digit:]]{4}'),
         year = as.numeric(year),
         num_week = str_extract(`Year & week number`,'-.+'),
         num_week = str_remove(num_week,'-'),
         num_week = parse_number(num_week)) %>% 
  select(-`Year & week number`) %>% 
  select(year,num_week,age,death) %>% 
  group_by(year,num_week,age) %>% 
  summarise(death = sum(death)) %>% 
  ungroup()

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

death_interval = bind_rows(death_male %>% mutate(gender = 'male'),
                           death_female %>% mutate(gender = 'female')) %>% 
  select(year,num_week,mid_day,last_day,age,gender,death) %>%
  mutate(age = factor(age,levels = c('0 to 14',
                                     '15 to 44',
                                     '45 to 64',
                                     '65 to 74',
                                     '75 to 84',
                                     '85+'))) %>% 
  group_by(year,num_week,mid_day,age,gender) %>% 
  summarise(death = sum((death))) %>% 
  ungroup() %>% 
  arrange(year,num_week,age,gender)

## E(x,g,i)
mid_year_pop = mid_year_pop %>% 
  mutate(mid_day = as.Date(str_c(year,'-07-01')))

mid_day_year = unique(mid_year_pop$mid_day)

exposure_pop = death_interval %>% 
  select(-death) %>% 
  filter(between(year,2015,2021)) %>% 
  mutate(exposure = NA)

for(i in 1:nrow(exposure_pop)){
  
  index = findInterval(x = exposure_pop$mid_day[i], vec = mid_day_year)
  mid_day1 = mid_day_year[index]
  mid_day2 = mid_day_year[index+1]
  pops = mid_year_pop %>%
    filter(mid_day %in% c(mid_day1,mid_day2),
           gender==exposure_pop$gender[i],
           age==exposure_pop$age[i]) %>% 
    pull(mid_year_population)
  pop1 = pops[1]
  pop2 = pops[2]
  dist1 = as.numeric(exposure_pop$mid_day[i]-mid_day1)
  dist2 = as.numeric(mid_day2-exposure_pop$mid_day[i])
  w1 = (1/dist1)/(1/dist1+1/dist2)
  w2 = (1/dist2)/(1/dist1+1/dist2)
  exposure_pop$exposure[i] = w1 * pop1 + w2 * pop2
  
}

## SMR(G,i)
SMR = exposure_pop %>%
  left_join(death_interval,
            by = c('year',
                   'num_week',
                   'mid_day',
                   'age',
                   'gender')) %>% 
  mutate(rate = death/exposure) %>% 
  left_join(std_pop,
            by = c('age','gender')) %>% 
  mutate(std_death = EuropeanStandardPopulation*rate) %>% 
  group_by(age,gender,year,num_week,mid_day) %>% 
  summarise(std_death = sum(std_death)) %>% 
  ungroup() %>% 
  full_join(std_pop,
            by = c('age','gender')) %>% 
  mutate(smr = std_death / EuropeanStandardPopulation) %>% 
  select(year,num_week,mid_day,age,gender,smr) %>% 
  mutate(lag_smr = NA)

for(i in 1:nrow(SMR)){
  
  if(SMR$year[i]>min(SMR$year) & SMR$year[i]<=2019){
    
    if(SMR$num_week[i]<53){
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i]-1,
               num_week==SMR$num_week[i],
               age==SMR$age[i],
               gender==SMR$gender[i]) %>% 
        pull(smr)
    }
    
    if(SMR$num_week[i]==53){
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i],
               num_week==1,
               age==SMR$age[i],
               gender==SMR$gender[i]) %>% 
        pull(smr)
    }
    
  }
  
  if(SMR$year[i]>2019){
    
    if(SMR$num_week[i]<53){
    
    SMR$lag_smr[i] = SMR %>% 
      filter(year==2019,
             num_week==SMR$num_week[i],
             age==SMR$age[i],
             gender==SMR$gender[i]) %>% 
      pull(smr)
    
    }
    
    if(SMR$num_week[i]==53){
      
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i],
               num_week==1,
               age==SMR$age[i],
               gender==SMR$gender[i]) %>% 
        pull(smr)
      
    }
    
  }
  
}

write_csv(SMR,'SMR.csv')

# Expected Mortality
SMR = read_csv('SMR.csv')

death = death_interval %>% 
  group_by(year,num_week,age,gender) %>% 
  summarise(death = sum(death)) %>% 
  ungroup()

excess_death = SMR %>%
  inner_join(death,
             by = c('year','num_week','age','gender')) %>% 
  mutate(expect_mortality = death*lag_smr/smr,
         excess_death = death-expect_mortality)

write_csv(excess_death,'excess_death.csv')
  
# Excess Mortality Visualization
age_gender = death %>% 
  select(age,gender) %>% 
  distinct()

for(i in 1:nrow(age_gender)){
  
  p = excess_death %>% 
    filter(age==age_gender$age[i],
           gender==age_gender$gender[i]) %>% 
    mutate(bias = if_else(excess_death>0,'positive','negative')) %>% 
    filter(year %in% c(2020, 2021)) %>% 
    ggplot(aes(x = mid_day,y = excess_death,fill = bias)) +
    geom_col() +
    scale_fill_brewer(palette = 'Set1',direction = -1) +
    scale_x_date(date_breaks = '2 month', date_labels = '%Y-%m') +
    labs(x = 'Date',
         y = 'Excess Mortality',
         title = 'Excess Mortality in England & Wales',
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

# Yearly SMR
SMR_year = exposure_pop %>%
  left_join(death_interval,
            by = c('year',
                   'num_week',
                   'mid_day',
                   'age',
                   'gender')) %>% 
  mutate(rate = death/exposure) %>% 
  left_join(std_pop,
            by = c('age','gender')) %>% 
  mutate(std_death = EuropeanStandardPopulation*rate) %>% 
  group_by(year,num_week,mid_day) %>% 
  summarise(std_death = sum(std_death)) %>% 
  ungroup() %>% 
  mutate(smr_year = std_death/sum(std_pop$EuropeanStandardPopulation)) %>% 
  arrange(year,num_week) %>% 
  mutate(lag_smr = NA,
         quarterly_avg = NA,
         annual_avg = NA) %>% 
  filter(num_week<=52)

for(i in 1:nrow(SMR_year)){
  
  if(i-6>0 & i+6<nrow(SMR_year)){
    SMR_year$quarterly_avg[i] = mean(SMR_year$smr_year[(i-6):(i+6)])
  }
  
  if(i-26>0 & i+26<nrow(SMR_year)){
    SMR_year$annual_avg[i] = mean(SMR_year$smr_year[(i-26):(i+26)])
  }
  
  if(SMR_year$year[i]>min(SMR_year$year) & SMR_year$year[i]<=2019){
    SMR_year$lag_smr[i] = SMR_year %>% 
      filter(year==SMR_year$year[i]-1,
             num_week==SMR_year$num_week[i]) %>% 
      pull(smr_year)
  }
  
  if(SMR_year$year[i]>2019){
    SMR_year$lag_smr[i] = SMR_year %>% 
      filter(year==2019,
             num_week==SMR_year$num_week[i]) %>% 
      pull(smr_year)
  }
  
}

write_csv(SMR_year,'SMR_year.csv')

# Chart 4A: Quarterly and annual centred average SMRs – whole period
SMR_year %>% 
  select(mid_day,quarterly_avg,annual_avg) %>% 
  pivot_longer(cols = -mid_day) %>% 
  mutate(name = str_remove(name,'_avg'),
         name = str_to_title(name)) %>% 
  ggplot(aes(x = mid_day,y = value,color = name)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year',date_labels = '%Y') +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = NULL,
       y = 'Quarterly and annual centred average SMRs',
       color = NULL) +
  theme_light() +
  theme(legend.position = 'bottom')
ggsave(filename = str_c('plot/','Quarterly and annual centred average SMRs.jpg'),
       width = 7500,
       height = 2500,
       units = 'px',
       dpi = 600)

# Chart 4C: Quarterly centred average SMRs, by week number
ggplot(data = SMR_year,
       mapping = aes(x = num_week,y = quarterly_avg,color = as_factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,52,13)) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x = 'Week',
       y = 'Quarterly centred average SMRs',
       color = NULL) +
  theme_light() +
  theme(legend.position = 'bottom')
ggsave(filename = str_c('plot/','Quarterly centred average SMRs (by week number).jpg'),
       width = 5500,
       height = 3500,
       units = 'px',
       dpi = 600)


# Chart 4D: Cumulative standardised mortality rate (cSMR) compared to the xxxx-xxxx average


# Expected Mortality
SMR_year = read_csv('SMR_year.csv')

death_year = death_interval %>% 
  select(year,num_week,death) %>% 
  group_by(year,num_week) %>% 
  summarise(death = sum(death)) %>% 
  ungroup()

excess_death_year = SMR_year %>%
  inner_join(death_year,
             by = c('year','num_week')) %>% 
  mutate(expect_mortality = death*lag_smr/smr_year,
         excess_death = death-expect_mortality)

write_csv(excess_death_year,'excess_death_year.csv')

# Excess Mortality Visualization
excess_death_2020_2021 <- excess_death_year %>%
  filter(year %in% c(2020, 2021))

ggplot(data = excess_death_2020_2021, aes(x = num_week, y = excess_death, group = as_factor(year), color = as_factor(year))) +
  geom_line() +  
  scale_color_brewer(palette = 'Dark2') + 
  labs(x = 'Week',
       y = 'Excess Death',
       color = 'Year',  
       title = 'Excess Death in England & Wales') +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(filename = str_c('plot/','Excess Death in England & Wales.jpg'),
       width = 5500,
       height = 3500,
       units = 'px',
       dpi = 600)

SMR_year_gender = exposure_pop %>%
  left_join(death_interval,
            by = c('year',
                   'num_week',
                   'mid_day',
                   'age',
                   'gender')) %>%  
  mutate(rate = death/exposure) %>% 
  left_join(std_pop,
            by = c('age',
                   'gender')) %>%  
  mutate(std_death = EuropeanStandardPopulation * rate) %>% 
  group_by(year, num_week, mid_day, gender) %>%  
  summarise(std_death = sum(std_death), .groups = 'drop') %>%
  ungroup() %>%
  mutate(smr_year = std_death / sum(std_pop$EuropeanStandardPopulation)) %>% 
  arrange(year, num_week) %>%
  mutate(lag_smr = NA, quarterly_avg = NA, annual_avg = NA) %>% 
  filter(num_week <= 52)

for(i in 1:nrow(SMR_year_gender)){
  # Quarterly Average
  if(i-6 > 0 & i+6 < nrow(SMR_year_gender)){
    SMR_year_gender$quarterly_avg[i] = mean(SMR_year_gender$smr_year[(i-6):(i+6)])
  }
  
  # Annual Average
  if(i-26 > 0 & i+26 < nrow(SMR_year_gender)){
    SMR_year_gender$annual_avg[i] = mean(SMR_year_gender$smr_year[(i-26):(i+26)])
  }
  
  # Lag SMR
  if(SMR_year_gender$year[i] > min(SMR_year_gender$year) & SMR_year_gender$year[i] <= 2019){
    SMR_year_gender$lag_smr[i] = SMR_year_gender %>% 
      filter(year == SMR_year_gender$year[i] - 1,
             num_week == SMR_year_gender$num_week[i],
             gender == SMR_year_gender$gender[i]) %>%  
      pull(smr_year)
  }
  
  if(SMR_year_gender$year[i] > 2019){
    SMR_year_gender$lag_smr[i] = SMR_year_gender %>% 
      filter(year == 2019,
             num_week == SMR_year_gender$num_week[i],
             gender == SMR_year_gender$gender[i]) %>%
      pull(smr_year)
  }
}

gender_death_year = death_interval %>% 
  select(year,num_week,gender,death) %>% 
  group_by(year,num_week,gender) %>% 
  summarise(death = sum(death)) %>% 
  ungroup()

excess_death_gender_year = SMR_year_gender %>%
  inner_join(gender_death_year,
             by = c('year','num_week','gender')) %>% 
  mutate(expect_mortality = death*lag_smr/smr_year,
         excess_death = death-expect_mortality)



excess_death_filtered <- excess_death %>%
  filter(year %in% c(2020, 2021))

excess_death_filtered$bias <- ifelse(excess_death_filtered$excess_death > 0, 'Positive', 'Negative')


p <- ggplot(excess_death_filtered, aes(x = mid_day, y = excess_death, fill = bias)) +
  geom_col() +
  scale_fill_manual(values = c('Positive' = 'red', 'Negative' = 'blue')) + 
  facet_grid(rows = vars(age), cols = vars(gender), scales = "free_y") + 
  labs(title = 'Excess Death by Age and Gender',
       x = 'Date',
       y = 'Excess Death') +
  theme_light(base_size = 16) +  
  theme(
    legend.position = 'bottom',
    plot.background = element_blank(),  
    panel.background = element_blank(),  
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 16, color = "black"),  
    strip.text.y = element_text(size = 16, color = "black")   
  )
ggsave('Excess Death by Age and Gender.jpg', plot = p, width = 12, height = 15, dpi = 300)



excess_death_year <- subset(excess_death_year, year == 2020 | year == 2021)

# change mid day into date
excess_death_year$mid_day <- as.Date(excess_death_year$mid_day)

# plot
ggplot(excess_death_year, aes(x = mid_day)) +
  geom_line(aes(y = expect_mortality, colour = "Expected Deaths")) +
  geom_line(aes(y = death, colour = "Actual Deaths")) +
  scale_colour_manual(values = c("Expected Deaths" = "blue", "Actual Deaths" = "red")) +
  labs(x = "Time", y = "Number of Death", title = "Expected vs Actual Deaths (England &Wales)") +
  theme_minimal() +  # 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("expected_vs_actual_deaths_2020_2021.jpg", width = 12, height = 6, dpi = 300)

str(excess_death_year$excess_death)


excess_death_year$excess_death <- as.numeric(excess_death_year$excess_death)


excess_death_summary <- excess_death_year %>%
  group_by(year) %>%
  summarize(total_excess_death = sum(excess_death, na.rm = TRUE))


print(excess_death_summary)
