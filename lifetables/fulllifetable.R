# load packages
install.packages(c("MortalityTables", "tidyverse",
                   "readxl", "ggthemes", 
                   "fpp3", "readr","janitor"))



library(MortalityTables)
library(tidyverse)
library(readxl)
library(ggthemes)
library(fpp3)
library(readr)
library(janitor)

# Population Data
#### load region info, rename, put each place into region
uk_regions <- read_csv("https://raw.githubusercontent.com/xrander/uk_mortality_forecast/master/uk_regions%20-%20Sheet1.csv",
                       col_names = F) %>% 
  rename("geography" = X1,
         "region" = X2)
head(uk_regions)

# load population data 
pop <- read_csv("https://raw.githubusercontent.com/xrander/uk_mortality_forecast/master/pop_dat.csv")
head(pop)

# only select the needed data
pop_dat <-  pop %>% select(laname21, sex, age, population_2013:population_2020)

# join data to the left side so each geography shows the region the belong to
pop_data <- pop_dat %>%  
  left_join(uk_regions, join_by(laname21 == geography))

# assign the missing data to their region
pop_df <- pop_data %>% 
  mutate(region = case_when(laname21 == "Bournemouth, Christchurch and Poole" ~ "South West England",
                            laname21 == "High Peak" ~ "South West England",
                            .default = region),
         sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female"))

# tidy data
# group output by year, region and sex
cleaned_population_data <-  pop_df %>% 
  pivot_longer(cols = starts_with("pop"),
               names_to = "year",
               values_to = "population") %>% 
  mutate(year = str_remove_all(year, "population_"),
         year = as.integer(year)) %>%
  select(sex:population) %>% 
  group_by(year, region, sex,  age) %>% 
  summarize(pop = sum(population)) %>%
  pivot_wider(names_from = sex,
              values_from = pop)

head(cleaned_population_data)

##################################
# The output from this code will be used for forecasting
binned_data <- cleaned_population_data |>
  filter(region != "Northern Ireland") |>
  mutate(region = case_when(region == "Wales" ~ "Wales",
                            region == "Scotland" ~ "Scotland",
                            .default = "England")) |> 
  group_by(year, region) |> 
  summarize(female = sum(Female),
            male = sum(Male)) |>
  mutate(total_pop = female + male) |> 
  select(year:region, total_pop)

####################################

# Function to read all sheets from an Excel workbook and store them in the global environment
read_all_sheets_to_global <- function(filename, x) {
  # Get the names of all sheets in the workbook
  sheet_names <- excel_sheets(filename)
  sheet_names <- sheet_names[-c(1:4, 13:45)]
  # Read each sheet and assign it to a variable in the global environment
  lapply(sheet_names, function(sheet) {
    sheet_data <- read_excel(filename, sheet = sheet, skip = 5) %>%
      janitor::clean_names() %>% 
      select(1, 3, 10) %>% 
      rename("age" = age_1,
             "death_prob_male" = qx_3,
             "death_prob_female" = qx_10)
    
    assign(paste(as.character(x), sheet, sep = "_"), sheet_data, envir = .GlobalEnv)
  })
}

# Load the life table data

# laod England data
nlt_eng = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/xrander/uk_mortality_forecast/raw/master/nlt_england.xlsx"
download.file(dataURL, destfile=nlt_eng, mode='wb')

read_all_sheets_to_global(nlt_eng, "eng")

# rename_eng
eng_death_prob_13 <- `eng_2013-2015` %>% mutate(year = 2013)
eng_death_prob_14 <- `eng_2014-2016` %>% mutate(year = 2014)
eng_death_prob_15 <- `eng_2015-2017` %>% mutate(year = 2015)
eng_death_prob_16 <- `eng_2016-2018` %>% mutate(year = 2016)
eng_death_prob_17 <- `eng_2017-2019` %>% mutate(year = 2017)
eng_death_prob_18 <-`eng_2018-2020` %>% mutate(year = 2018)
eng_death_prob_19 <- `eng_2019-2021` %>% mutate(year = 2019)
eng_death_prob_20 <- `eng_2020-2022` %>% mutate(year = 2020)

# Remove old list
rm(list = c("eng_2020-2022", "eng_2019-2021","eng_2018-2020",
            "eng_2017-2019","eng_2016-2018", "eng_2015-2017",
            "eng_2014-2016", "eng_2013-2015"))


# England population data
# create mortality tables for each region and year
# filter out England from the cleaned_population_data and get the total male and female in the region
eng_pop <- cleaned_population_data %>% 
  filter(region %in% c("East Midlands", "East of England", "London",
                       "North East England", "North West England", "South East England",
                       "South West England", "West Midlands", "Yorkshire and the Humber")) %>% 
  mutate(region = "England") %>% 
  group_by(year, age, region) %>% 
  summarize(female = sum(Female),
            male = sum(Male))

head(eng_pop)

# Function to generate total death probability
# function convert the life table into a mortality table package data format type
mort_data_format_eng <- function(x, y){
  x %>%
    left_join(eng_pop %>%
                filter(year == y),
              join_by(year, age)) %>% 
    mutate(total_pop = female + male,
           male_prop = male/total_pop,
           female_prop = female/total_pop,
           death_prop = (death_prob_male * male_prop) + (death_prob_female * female_prop),
           total_death = round(death_prop * total_pop))
}

# apply function to all life table data of england
eng_death_prob_13 <- mort_data_format_eng(eng_death_prob_13, 2013)
eng_death_prob_14 <- mort_data_format_eng(eng_death_prob_14, 2014)
eng_death_prob_15 <- mort_data_format_eng(eng_death_prob_15, 2015)
eng_death_prob_16 <- mort_data_format_eng(eng_death_prob_16, 2016)
eng_death_prob_17 <- mort_data_format_eng(eng_death_prob_17, 2017)
eng_death_prob_18 <- mort_data_format_eng(eng_death_prob_18, 2018)
eng_death_prob_19 <- mort_data_format_eng(eng_death_prob_19, 2019)
eng_death_prob_20 <- mort_data_format_eng(eng_death_prob_20, 2020)


# same for the other regions
# Scotland
# Download from the web
nlt_scot = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/xrander/uk_mortality_forecast/raw/master/nlt_scotland.xlsx"
download.file(dataURL, destfile=nlt_scot, mode='wb')


read_all_sheets_to_global(nlt_scot, "scot")



# Scotland population data
scotland_pop <- cleaned_population_data %>% 
  filter(region == "Scotland") %>% 
  group_by(year, age, region) %>% 
  summarize(female = sum(Female),
            male = sum(Male))

# Rename scotland 
scot_death_prob_13 <- `scot_2013-2015` %>% mutate(year = 2013)
scot_death_prob_14 <- `scot_2014-2016` %>% mutate(year = 2014)
scot_death_prob_15 <- `scot_2015-2017` %>% mutate(year = 2015)
scot_death_prob_16 <- `scot_2016-2018` %>% mutate(year = 2016)
scot_death_prob_17 <- `scot_2017-2019` %>% mutate(year = 2017)
scot_death_prob_18 <-`scot_2018-2020` %>% mutate(year = 2018)
scot_death_prob_19 <- `scot_2019-2021` %>% mutate(year = 2019)
scot_death_prob_20 <- `scot_2020-2022` %>% mutate(year = 2020)

# Remove old list
rm(list = c("scot_2020-2022", "scot_2019-2021","scot_2018-2020",
            "scot_2017-2019","scot_2016-2018", "scot_2015-2017",
            "scot_2014-2016", "scot_2013-2015"))

# Function to generate total death probability tailored to scotland
mort_data_format_scot <- function(x, y){
  x %>%
    left_join(scotland_pop %>%
                filter(year == y),
              join_by(year, age)) %>% 
    mutate(total_pop = female + male,
           male_prop = male/total_pop,
           female_prop = female/total_pop,
           death_prop = (death_prob_male * male_prop) + (death_prob_female * female_prop),
           total_death = round(death_prop * total_pop))
}

scot_death_prob_13 <- mort_data_format_scot(scot_death_prob_13, 2013)
scot_death_prob_14 <- mort_data_format_scot(scot_death_prob_14, 2014)
scot_death_prob_15 <- mort_data_format_scot(scot_death_prob_15, 2015)
scot_death_prob_16 <- mort_data_format_scot(scot_death_prob_16, 2016)
scot_death_prob_17 <- mort_data_format_scot(scot_death_prob_17, 2017)
scot_death_prob_18 <- mort_data_format_scot(scot_death_prob_18, 2018)
scot_death_prob_19 <- mort_data_format_scot(scot_death_prob_19, 2019)
scot_death_prob_20 <- mort_data_format_scot(scot_death_prob_20, 2020)


# Wales
nlt_wales = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/xrander/uk_mortality_forecast/raw/master/nlt_wales.xlsx"
download.file(dataURL, destfile=nlt_wales, mode='wb')

read_all_sheets_to_global(nlt_wales, "wales")
# Wales pop data
wales_pop <- cleaned_population_data %>% 
  filter(region == "Wales") %>% 
  group_by(year, age, region) %>% 
  summarize(female = sum(Female),
            male = sum(Male))


# rename_wales
wales_death_prob_13 <- `wales_2013-2015` %>% mutate(year = 2013)
wales_death_prob_14 <- `wales_2014-2016` %>% mutate(year = 2014)
wales_death_prob_15 <- `wales_2015-2017` %>% mutate(year = 2015)
wales_death_prob_16 <- `wales_2016-2018` %>% mutate(year = 2016)
wales_death_prob_17 <- `wales_2017-2019` %>% mutate(year = 2017)
wales_death_prob_18 <-`wales_2018-2020` %>% mutate(year = 2018)
wales_death_prob_19 <- `wales_2019-2021` %>% mutate(year = 2019)
wales_death_prob_20 <- `wales_2020-2022` %>% mutate(year = 2020)


# Remove old list
rm(list = c("wales_2020-2022", "wales_2019-2021","wales_2018-2020",
            "wales_2017-2019","wales_2016-2018", "wales_2015-2017",
            "wales_2014-2016", "wales_2013-2015"))

wales_death_prob_13 <- wales_death_prob_13 %>% 
  left_join(wales_pop %>% 
              filter(year == 2013),
            join_by(year, age))


wales_death_prob_13 <-  wales_death_prob_13 %>% 
  mutate(total_pop = female + male,
         male_prop = male/total_pop,
         female_prop = female/total_pop,
         death_prop = (death_prob_male * male_prop) + (death_prob_female * female_prop),
         total_death = round(death_prop * total_pop))

# Format function to take in Wales population as the variable
mort_data_format <- function(x, y){
  x %>%
    left_join(wales_pop %>%
                filter(year == y),
              join_by(year, age)) %>% 
    mutate(total_pop = female + male,
           male_prop = male/total_pop,
           female_prop = female/total_pop,
           death_prop = (death_prob_male * male_prop) + (death_prob_female * female_prop),
           total_death = round(death_prop * total_pop))
}


# Generate total death probability for each year
wales_death_prob_14 <- mort_data_format(wales_death_prob_14, 2014)
wales_death_prob_15 <- mort_data_format(wales_death_prob_15, 2015)
wales_death_prob_16 <- mort_data_format(wales_death_prob_16, 2016)
wales_death_prob_17 <- mort_data_format(wales_death_prob_17, 2017)
wales_death_prob_18 <- mort_data_format(wales_death_prob_18, 2018)
wales_death_prob_19 <- mort_data_format(wales_death_prob_19, 2019)
wales_death_prob_20 <- mort_data_format(wales_death_prob_20, 2020)

biglifetable <- rbind(wales_death_prob_13,
                      wales_death_prob_14,
                      wales_death_prob_15,
                      wales_death_prob_16,
                      wales_death_prob_17,
                      wales_death_prob_18,
                      wales_death_prob_19,
                      wales_death_prob_20,
                      scot_death_prob_13,
                      scot_death_prob_14,
                      scot_death_prob_15,
                      scot_death_prob_16,
                      scot_death_prob_17,
                      scot_death_prob_18,
                      scot_death_prob_19,
                      scot_death_prob_20,
                      eng_death_prob_13,
                      eng_death_prob_14,
                      eng_death_prob_15,
                      eng_death_prob_16,
                      eng_death_prob_17,
                      eng_death_prob_18,
                      eng_death_prob_19,
                      eng_death_prob_20)

write.csv(biglifetable, "Combined life table.csv")
