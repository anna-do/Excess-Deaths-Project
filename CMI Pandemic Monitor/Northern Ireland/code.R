# Load Pacakges
library(tidyverse)
library(readxl)
library(timeDate)
library(scales)

# Import Data
years = 2014:2022

## E(x,g,y)
mid_year_pop = read_xlsx('data/NI mid_year_pop.xlsx')
mid_year_pop = mid_year_pop %>% 
  mutate(age = factor(age,levels = c('0 to 14',
                                     '15 to 44',
                                     '45 to 64',
                                     '65 to 74',
                                     '75 to 84',
                                     '85+')))
  
## P(x,g)
std_pop = read_csv('data/european_standard_population_by_sex.csv')
std_pop = std_pop %>% 
  select(-Sex) %>% 
  mutate(AgeGroup = str_remove(AgeGroup,' years')) %>%
  mutate(
    age = case_when(
    AgeGroup %in% c('0-4','5-9','10-14') ~ '0 to 14',
    AgeGroup %in% c('15-19','20-24','25-29','30-34','35-39','40-44') ~ '15 to 44',
    AgeGroup %in% c('45-49','50-54','55-59','60-64') ~ '45 to 64',
    AgeGroup %in% c('65-69','70-74') ~ '65 to 74',
    AgeGroup %in% c('75-79','80-84') ~ '75 to 84',
    AgeGroup %in% c('85-89','90plus') ~ '85+')) %>% 
  group_by(age) %>% 
  summarise(EuropeanStandardPopulation = sum(EuropeanStandardPopulation)) %>% 
  ungroup() %>% 
  mutate(age = factor(age,
                      labels = levels(mid_year_pop$age)))

## D(x,g,i)
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

death_interval = death %>% 
  mutate(mid_day = last_day - 3) %>% 
  select(year,num_week,mid_day,last_day,age,death) %>%
  group_by(year,num_week,mid_day,age) %>% 
  summarise(death = sum((death))) %>% 
  ungroup() %>% 
  arrange(year,num_week,age)

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
                   'age')) %>% 
  mutate(rate = death/exposure) %>% 
  left_join(std_pop,
            by = c('age')) %>% 
  mutate(std_death = EuropeanStandardPopulation*rate) %>% 
  group_by(age,year,num_week,mid_day) %>% 
  summarise(std_death = sum(std_death)) %>% 
  ungroup() %>% 
  full_join(std_pop,
            by = c('age')) %>% 
  mutate(smr = std_death / EuropeanStandardPopulation) %>% 
  select(year,num_week,mid_day,age,smr) %>% 
  mutate(lag_smr = NA)

for(i in 1:nrow(SMR)){
  
  if(SMR$year[i]>min(SMR$year) & SMR$year[i]<=2019){
    
    if(SMR$num_week[i]<53){
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i]-1,
               num_week==SMR$num_week[i],
               age==SMR$age[i]) %>% 
        pull(smr)
    }
    
    if(SMR$num_week[i]==53){
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i],
               num_week==1,
               age==SMR$age[i]) %>% 
        pull(smr)
    }
    
  }
  
  if(SMR$year[i]>2019){
    
    if(SMR$num_week[i]<53){
    
    SMR$lag_smr[i] = SMR %>% 
      filter(year==2019,
             num_week==SMR$num_week[i],
             age==SMR$age[i]) %>% 
      pull(smr)
    
    }
    
    if(SMR$num_week[i]==53){
      
      SMR$lag_smr[i] = SMR %>% 
        filter(year==SMR$year[i],
               num_week==1,
               age==SMR$age[i]) %>% 
        pull(smr)
      
    }
    
  }
  
}

write_csv(SMR,'SMR.csv')

# Expected Mortality
SMR = read_csv('SMR.csv')

excess_death = SMR %>% 
  inner_join(death,
             by = c('year','num_week','age')) %>% 
  mutate(expect_mortality = death*lag_smr/smr,
         excess_death = death-expect_mortality)

write_csv(excess_death,'excess_death.csv')
  
# Excess Mortality Visualization
age_tab = death %>% 
  select(age) %>% 
  distinct()
  
for(i in 1:nrow(age_tab)){
  
  p = excess_death %>% 
    filter(age==age_tab$age[i]) %>% 
    mutate(bias = if_else(excess_death>0,'positive','negative')) %>% 
    filter(year==2020) %>% 
    ggplot(aes(x = mid_day,y = excess_death,fill = bias)) +
    geom_col() +
    scale_fill_brewer(palette = 'Set1',direction = -1) +
    scale_x_date(date_breaks = '1 month', date_labels = '%Y-%m') +
    labs(x = 'Date',
         y = 'Excess Mortality',
         title = 'Excess Mortality in England',
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

# Yearly SMR
SMR_year = exposure_pop %>%
  left_join(death_interval,
            by = c('year',
                   'num_week',
                   'mid_day',
                   'age')) %>% 
  mutate(rate = death/exposure) %>% 
  left_join(std_pop,
            by = c('age')) %>% 
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



# Expected Mortality
SMR_year = read_csv('SMR_year.csv')

death_year = death %>% 
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

excess_death <- excess_death %>%
  mutate(bias = if_else(excess_death > 0, 'positive', 'negative')) %>%
  filter(year %in% c(2020, 2021)) 


p <- ggplot(excess_death, aes(x = mid_day, y = excess_death, fill = bias)) +
  geom_col() +
  scale_fill_brewer(palette = 'Set1', direction = -1) +
  scale_x_date(date_breaks = '3 months', date_labels = '%Y-%m') +
  labs(x = 'Date',
       y = 'Excess Death',
       title = 'Excess Death in Northern Ireland by Age Group',
       subtitle = '2020 and 2021') +
  theme_minimal() +  # Use a minimal theme to remove the gray background
  theme(legend.position = 'none',
        strip.background = element_blank(),  # Remove the gray background from the facet labels
        strip.text.x = element_text(size = 14, colour = 'black'),  # Increase size and make text black for horizontal labels
        strip.text.y = element_text(size = 14, colour = 'black'),  # Increase size and make text black for vertical labels
        axis.text = element_text(size = 12, colour = 'black'),  # Change axis text color to black
        axis.title = element_text(size = 14, colour = 'black'),  # Increase size and change color of axis titles
        plot.title = element_text(size = 16, colour = 'black'),  # Increase size and change color of plot title
        plot.subtitle = element_text(size = 14, colour = 'black')) +  # Increase size and change color of plot subtitle
  facet_wrap(~age, ncol = 2, scales = 'free_y')


print(p)

ggsave(filename = 'excess_death_by_age_group.jpg', plot = p, width = 15, height = 15, units = 'in', dpi = 300)

excess_death_year <- subset(excess_death_year, year == 2020 | year == 2021)

# change mid day into date
excess_death_year$mid_day <- as.Date(excess_death_year$mid_day)

# plot
ggplot(excess_death_year, aes(x = mid_day)) +
  geom_line(aes(y = expect_mortality, colour = "Expected Deaths")) +
  geom_line(aes(y = death, colour = "Actual Deaths")) +
  scale_colour_manual(values = c("Expected Deaths" = "blue", "Actual Deaths" = "red")) +
  labs(x = "Time", y = "Number of Death", title = "Expected vs Actual Deaths (Northern Ireland)") +
  theme_minimal() +  
  theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("expected_vs_actual_deaths_2020_2021.jpg", width = 12, height = 6, dpi = 300)


ggplot(data = excess_death_year, aes(x = num_week, y = excess_death, group = as_factor(year), color = as_factor(year))) +
  geom_line() +  
  scale_color_brewer(palette = 'Dark2') + 
  labs(x = 'Week',
       y = 'Excess Death',
       color = 'Year',  
       title = 'Excess Death in Northern Ireland') +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(filename = str_c('plot/','Excess Death in Northern Ireland.jpg'),
       width = 5500,
       height = 3500,
       units = 'px',
       dpi = 600)


str(excess_death_year$excess_death)


excess_death_year$excess_death <- as.numeric(excess_death_year$excess_death)


excess_death_summary <- excess_death_year %>%
  group_by(year) %>%
  summarize(total_excess_death = sum(excess_death, na.rm = TRUE))


print(excess_death_summary)
