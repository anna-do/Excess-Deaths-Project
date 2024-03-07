library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(gridExtra)
library(stringr)

# Method 1
# England and Wales Weekly Excess Deaths - 53 weeks convert to 52 weeks
prepare_data <- function(sheet_name) {
  
  data <- read_excel("Weekly_Deaths.xlsx", sheet = sheet_name, skip = 1)
  
  data <- data %>%
    separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
    mutate(
      Year = as.integer(Year),
      WeekNumber = as.integer(WeekNumber)
    )
  
  # Identify years with a 53rd week
  years_with_53_weeks <- unique(data$Year[data$WeekNumber == 53])
  
  for(year in years_with_53_weeks) {
    if(year + 1 %in% data$Year) {
      # Calculate the average of 53rd week and next year's 1st week
      week_53_idx <- which(data$Year == year & data$WeekNumber == 53)
      next_year_week_1_idx <- which(data$Year == year + 1 & data$WeekNumber == 1)
      
      if(length(week_53_idx) > 0 & length(next_year_week_1_idx) > 0) {
        avg_columns <- setdiff(names(data), c("Year", "WeekNumber", "Age_Group"))
        
        for(column in avg_columns) {
          avg_value <- mean(c(data[[column]][week_53_idx], data[[column]][next_year_week_1_idx]), na.rm = TRUE)
          week_52_idx <- which(data$Year == year & data$WeekNumber == 52)
          if(length(week_52_idx) > 0) {
            data[[column]][week_52_idx] <- avg_value
          }
        }
      }
    }
  }
  
  # Now, remove the 53rd week's data
  data <- data %>%
    filter(!(Year %in% years_with_53_weeks & WeekNumber == 53))
  
  age_group_cols <- setdiff(names(data), c("Year", "WeekNumber", "All ages"))
  data <- data %>%
    mutate(Total_Deaths = rowSums(select(., all_of(age_group_cols)), na.rm = TRUE))
  
  data <- data %>%
    select(Year, WeekNumber, `All ages`, Total_Deaths, everything())
  
  data_long <- data %>%
    pivot_longer(
      cols = -c(Year, WeekNumber, `All ages`, Total_Deaths),
      names_to = "Age_Group",
      values_to = "Deaths"
    ) %>%
    mutate(
      Age_Group = if_else(Age_Group == "Under_1", "<1", Age_Group)
    )
  
  return(data_long)
}

# Seperate the deaths data 
EngWal_death_female <- prepare_data("Female")
EngWal_death_male <- prepare_data("Male")
EngWal_death_all <- prepare_data("All")


# Define the function to calculate weekly excess mortality
calculate_weekly_excess_mortality <- function(year, week, age_group, deaths_data) {
  # Get observed mortality for the specified year, week, and age group
  observed_mortality <- deaths_data %>%
    filter(Year == year, WeekNumber == week, Age_Group == age_group) %>%
    summarise(Week_Deaths = sum(Deaths, na.rm = TRUE)) %>%
    pull(Week_Deaths)
  if (year < 2021) {
    previous_years <- (year - 5):(year - 1)
  } else if (year == 2021) {
    previous_years <- 2015:2019
  } else {
    previous_years <- setdiff((year - 5):(year - 1), 2020)
  }
  
  
  # Calculate expected mortality as the average deaths for the corresponding week over the past five years
  expected_mortality <- deaths_data %>%
    filter(Year %in% previous_years, WeekNumber == week, Age_Group == age_group) %>%
    summarise(Average_Deaths = mean(Deaths, na.rm = TRUE)) %>%
    pull(Average_Deaths)
  
  # Calculate weekly excess mortality
  weekly_excess_mortality <- observed_mortality - expected_mortality
  
  # Return a data frame with the calculated weekly excess mortality
  return(data.frame(Year = year, WeekNumber = week, Age_Group = age_group,  Observed_Mortality = observed_mortality, Expected_Mortality = expected_mortality, Excess_Mortality = weekly_excess_mortality))
}

EngWal_excess_mortality_data <- data.frame()

# Define the years and weeks of interest
years <- 2017:2021
weeks <- 1:52

categories <- list(Female = EngWal_death_female, Male = EngWal_death_male, All = EngWal_death_all)

# Loop through categories, years, weeks, and age groups to calculate excess mortality
for (category in names(categories)) {
  sheet_data <- categories[[category]]
  
  age_groups <- unique(sheet_data$Age_Group)
  
  for (year in years) {
    for (week in weeks) {
      for (age_group in age_groups) {
        weekly_excess_mortality <- calculate_weekly_excess_mortality(year, week, age_group, sheet_data)
        EngWal_excess_mortality_data <- rbind(EngWal_excess_mortality_data, cbind(Category = category, weekly_excess_mortality))
      }
    }
  }
}


print(EngWal_excess_mortality_data)


# The data for combined gender with specific Observed Mortality, Expected Mortality and Excess Mortality. 
# First, filter for the category "All"
EngWal_data_all <- EngWal_excess_mortality_data %>%
  filter(Category == "All")
# Group by 'Year' and 'WeekNumber' and then summarize the excess mortality
EngWal_excess_mortality_by_week <- EngWal_data_all %>%
  group_by(Year, WeekNumber) %>%
  summarize(Observed_Mortality = sum(Observed_Mortality), Expected_Mortality = sum(Expected_Mortality), Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')

print(EngWal_excess_mortality_by_week)

EngWal_excess_deaths_plot <- ggplot(EngWal_excess_mortality_by_week, aes(x = WeekNumber, y = Excess_Mortality, group = Year, color = as.factor(Year))) +
  geom_line() + # Line plot for each year
  labs(x = "Week Number", y = "Total Excess Mortality", color = "Year") +
  theme_minimal() + # Minimal theme
  theme(legend.position = "bottom") # Move legend to bottom
# Print the plot
print(EngWal_excess_deaths_plot)


# The weekly deaths for Male
# First, filter for the category "Male"
EngWal_data_male <- EngWal_excess_mortality_data %>%
  filter(Category == "Male")
# Group by 'Year' and 'WeekNumber' and then summarize the excess mortality
EngWal_excess_mortality_by_week_male <- EngWal_data_male %>%
  group_by(Year, WeekNumber) %>%
  summarize(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')

print(EngWal_excess_mortality_by_week_male)

EngWal_excess_deaths_male_plot <- ggplot(EngWal_excess_mortality_by_week_male, aes(x = WeekNumber, y = Total_Excess_Mortality, group = Year, color = as.factor(Year))) +
  geom_line() + # Line plot for each year
  labs(x = "Week Number", y = "Total Excess Mortality", color = "Year") +
  theme_minimal() + 
  theme(legend.position = "bottom") 

print(EngWal_excess_deaths_male_plot)



# Weekly Deaths for Female
# First, filter for the category "Female"
EngWal_data_female <- EngWal_excess_mortality_data %>%
  filter(Category == "Female")
# Group by 'Year' and 'WeekNumber' and then summarize the excess mortality
EngWal_excess_mortality_by_week_female <- EngWal_data_female %>%
  group_by(Year, WeekNumber) %>%
  summarize(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')

print(EngWal_excess_mortality_by_week_female)

EngWal_excess_deaths_female_plot <- ggplot(EngWal_excess_mortality_by_week_female, aes(x = WeekNumber, y = Total_Excess_Mortality, group = Year, color = as.factor(Year))) +
  geom_line() + # Line plot for each year
  labs(x = "Week Number", y = "Total Excess Mortality", color = "Year") +
  theme_minimal() + 
  theme(legend.position = "bottom") 

print(EngWal_excess_deaths_female_plot)



# Calculate the total annual excess mortality for each category
EngWal_annual_excess_mortality <- EngWal_excess_mortality_data %>%
  group_by(Year, Category) %>%
  summarise(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")
# Plot the trend of total annual excess mortality over the years for each category
ggplot(EngWal_annual_excess_mortality, aes(x = Year, y = Total_Excess_Mortality, group = Category, color = Category)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in England and Wales",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Choose a specific year to compare the weekly mortality trend by sex
specific_year <- 2020
EngWal_weekly_excess_mortality_specific_year <- EngWal_excess_mortality_data %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber, Category) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

# Plot the weekly excess mortality trends for different sexes in the chosen year
ggplot(EngWal_weekly_excess_mortality_specific_year, aes(x = WeekNumber, y = Weekly_Excess_Mortality, group = Category, color = Category)) +
  geom_line() +
  geom_point() +
  labs(title = paste("England and Wales Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())



# Summarize the data by Age_Group and Category to get the total excess mortality
EngWal_total_excess_mortality <- EngWal_excess_mortality_data %>%
  group_by(Age_Group, Category) %>%
  summarise(Total_Excess = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

ggplot(EngWal_total_excess_mortality, aes(x = Age_Group, y = Total_Excess, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Total Excess Mortality by Age Group and Category (2017-2021)",
       x = "Age Group",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "All" = "grey50"))

for (year in 2017:2021) {
  # Filter data for the current year
  yearly_data <- EngWal_excess_mortality_data %>%
    filter(Year == year) %>%
    group_by(Age_Group, Category) %>%
    summarise(Total_Excess = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")
  
  current_year <- ggplot(yearly_data, aes(x = Age_Group, y = Total_Excess, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    labs(title = paste("England and Wales Total Excess Mortality by Age Group and Gender in", year),
         x = "Age Group",
         y = "Total Excess Mortality") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "All" = "grey50"))
  
  print(current_year)
}








## Scotland Weekly Excess Deaths Calculation
Scot_Deaths_all <- read_excel("Weekly_Deaths_Scotland.xlsx", sheet = 'All', skip = 1)
# Separate "Year & week number" into "Year" and "WeekNumber"
Scot_Deaths_all <- Scot_Deaths_all %>%
  separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
  mutate(
    Year = as.integer(Year),
    WeekNumber = as.integer(WeekNumber)
  )

Scot_Deaths <- Scot_Deaths_all %>%
  arrange(Year, WeekNumber)
# Find years with 53 weeks
Scot_years_with_53_weeks <- unique(Scot_Deaths$Year[Scot_Deaths$WeekNumber == 53])
# Loop through years with 53 weeks to adjust the 52nd week data
for (year in Scot_years_with_53_weeks) {
  # Check if the first week of the next year is present
  if (nrow(filter(Scot_Deaths, Year == (year + 1), WeekNumber == 1)) > 0) {
    # Calculate the average deaths of the 53rd week and the first week of the next year
    week_53_deaths <- filter(Scot_Deaths, Year == year, WeekNumber == 53) %>% pull(`All ages`)
    week_1_next_year_deaths <- filter(Scot_Deaths, Year == (year + 1), WeekNumber == 1) %>% pull(`All ages`)
    average_deaths <- mean(c(week_53_deaths, week_1_next_year_deaths), na.rm = TRUE)
    # Assign this average to the 52nd week of the current year
    Scot_Deaths <- Scot_Deaths %>%
      mutate(`All ages` = ifelse(Year == year & WeekNumber == 52, average_deaths, `All ages`))
  } else {
    # If the first week of the next year is not available, use the 53rd week's data for the 52nd week
    week_53_deaths <- filter(Scot_Deaths, Year == year, WeekNumber == 53) %>% pull(`All ages`)
    Scot_Deaths <- Scot_Deaths %>%
      mutate(`All ages` = ifelse(Year == year & WeekNumber == 52, week_53_deaths, `All ages`))
  }
  # Remove the 53rd week's data
  Scot_Deaths <- Scot_Deaths %>%
    filter(!(Year == year & WeekNumber == 53))
}

# Initialize a data frame to store the results
Scot_excess_mortality_results <- data.frame()
# Loop through the years of interest
for (year_of_interest in 2017:2021) {
  if (year_of_interest <= 2019) {
    previous_years <- (year_of_interest - 5):(year_of_interest - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  # Loop through all weeks of the year
  for (week_of_interest in 1:52) {
    # Get observed mortality for the specified year and week
    observed_mortality <- Scot_Deaths %>%
      filter(Year == year_of_interest, WeekNumber == week_of_interest) %>%
      summarise(Week_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
      pull(Week_Deaths)
    # Calculate expected mortality as the average deaths for the corresponding week over the previous five years
    expected_mortality <- Scot_Deaths %>%
      filter(Year %in% previous_years, WeekNumber == week_of_interest) %>%
      summarise(Average_Deaths = mean(`All ages`, na.rm = TRUE)) %>%
      pull(Average_Deaths)
    # Calculate weekly excess mortality
    weekly_excess_mortality <- observed_mortality - expected_mortality
    # Bind the results to the data frame
    Scot_excess_mortality_results <- rbind(Scot_excess_mortality_results, data.frame(
      Year = year_of_interest,
      WeekNumber = week_of_interest,
      Observed_Mortality = observed_mortality,
      Expected_Mortality = expected_mortality,
      Excess_Mortality = weekly_excess_mortality
    ))
  }
}

ggplot(data = Scot_excess_mortality_results, aes(x = WeekNumber, y = Excess_Mortality, color = factor(Year))) +
  geom_line() +
  labs(title = "Total Excess Mortality",
       x = "Week Number",
       y = "Total Excess Mortality",
       color = "Year") +
  theme_minimal()

Scot_annual_excess_mortality <- Scot_excess_mortality_results %>%
  group_by(Year) %>%
  summarise(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")


ggplot(Scot_annual_excess_mortality, aes(x = Year, y = Total_Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in Scotland",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())

Scot_weekly_excess_mortality_specific_year <- Scot_excess_mortality_results %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

# Plot the weekly excess mortality trends for different sexes in the chosen year
ggplot(Scot_weekly_excess_mortality_specific_year, aes(x = WeekNumber, y = Weekly_Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Scotland Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Cumulative Deaths in Scotland
# Calculate the cumulative excess mortality for each year
Scot_Cumulative_Deaths <- Scot_excess_mortality_results %>%
  group_by(Year) %>%
  arrange(Year, WeekNumber) %>%
  mutate(Cumulative_Excess_Mortality = cumsum(Excess_Mortality)) %>%
  ungroup()

# Plot the cumulative excess mortality by week number for each year
ggplot(data = Scot_Cumulative_Deaths, aes(x = WeekNumber, y = Cumulative_Excess_Mortality, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Cumulative Excess Mortality by Week Number in Scotland",
       x = "Week Number",
       y = "Cumulative Excess Mortality",
       color = "Year") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") 


# Scotland Yearly Excess Deaths ( Using the cumulative of weekly excess deaths 'cumsum' to do the calculation)
Scot_yearly_totals <- Scot_Deaths_all %>%
  group_by(Year) %>%
  summarise(Yearly_Deaths = sum(`All ages`, na.rm = TRUE))
# Initialize a data frame to hold the 5-year averages
five_year_averages <- data.frame(Year = integer(), Average_Deaths = numeric())
# Populate the five_year_averages with the appropriate years and their corresponding averages
for (year in 2017:2021) {
  if (year <= 2019) {
    # Calculate the average of the total yearly deaths from the previous five years
    average_deaths <- Scot_yearly_totals %>%
      filter(Year >= (year - 5) & Year < year) %>%
      summarise(Average_Deaths = mean(Yearly_Deaths)) %>%
      pull(Average_Deaths)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    average_deaths <- Scot_yearly_totals %>%
      filter(Year >= 2015 & Year <= 2019) %>%
      summarise(Average_Deaths = mean(Yearly_Deaths)) %>%
      pull(Average_Deaths)
  }
  five_year_averages <- rbind(five_year_averages, data.frame(Year = year, Average_Deaths = average_deaths))
}

# Calculate the excess deaths for the years 2017 to 2021
Scot_yearly_totals <- Scot_yearly_totals %>%
  left_join(five_year_averages, by = "Year") %>%
  mutate(Excess_Deaths = ifelse(Year >= 2017, Yearly_Deaths - Average_Deaths, NA))

Scot_yearly_totals %>%
  filter(Year >= 2017 & Year <= 2021)


Scot_yearly_excess <- Scot_yearly_totals %>%
  filter(Year >= 2017 & Year <= 2021)









## NI Weekly Excess Deaths
NI_Deaths_all <- read_excel("Weekly_Deaths_NI1.xlsx", sheet = 'All', skip = 1)

NI_Deaths_all <- NI_Deaths_all %>%
  select(`Year & week number`, `All ages`)

NI_Deaths_all <- NI_Deaths_all %>%
  separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
  mutate(
    Year = as.integer(Year),
    WeekNumber = as.integer(WeekNumber)
  )

# Find years with 53 weeks
NI_years_with_53_weeks <- unique(NI_Deaths_all$Year[NI_Deaths_all$WeekNumber == 53])

# Loop through years with 53 weeks to adjust the 52nd week data
for (year in NI_years_with_53_weeks) {
  # Check if the first week of the next year is present
  if (nrow(filter(NI_Deaths_all, Year == (year + 1), WeekNumber == 1)) > 0) {
    # Calculate the average deaths of the 53rd week and the first week of the next year
    week_53_deaths <- filter(NI_Deaths_all, Year == year, WeekNumber == 53) %>% pull(`All ages`)
    week_1_next_year_deaths <- filter(NI_Deaths_all, Year == (year + 1), WeekNumber == 1) %>% pull(`All ages`)
    average_deaths <- mean(c(week_53_deaths, week_1_next_year_deaths), na.rm = TRUE)
    
    # Assign this average to the 52nd week of the current year
    NI_Deaths <- NI_Deaths_all %>%
      mutate(`All ages` = ifelse(Year == year & WeekNumber == 52, average_deaths, `All ages`))
  } else {
    # If the first week of the next year is not available, use the 53rd week's data for the 52nd week
    week_53_deaths <- filter(NI_Deaths, Year == year, WeekNumber == 53) %>% pull(`All ages`)
    NI_Deaths <- NI_Deaths %>%
      mutate(`All ages` = ifelse(Year == year & WeekNumber == 52, week_53_deaths, `All ages`))
  }
  # Remove the 53rd week's data
  NI_Deaths <- NI_Deaths %>%
    filter(!(Year == year & WeekNumber == 53))
}


# Initialize a data frame to store the results
NI_excess_mortality_results <- data.frame()
# Loop through the years of interest
for (year_of_interest in 2017:2021) {
  # Define the range of previous five years, with special handling for 2020 and 2021
  if (year_of_interest <= 2019) {
    previous_years <- (year_of_interest - 5):(year_of_interest - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  
  # Loop through all weeks of the year
  for (week_of_interest in 1:52) {
    # Get observed mortality for the specified year and week
    observed_mortality <- NI_Deaths %>%
      filter(Year == year_of_interest, WeekNumber == week_of_interest) %>%
      summarise(Week_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
      pull(Week_Deaths)
    # Calculate expected mortality as the average deaths for the corresponding week over the previous five years
    expected_mortality <- NI_Deaths %>%
      filter(Year %in% previous_years, WeekNumber == week_of_interest) %>%
      summarise(Average_Deaths = mean(`All ages`, na.rm = TRUE)) %>%
      pull(Average_Deaths)
    # Calculate weekly excess mortality
    weekly_excess_mortality <- observed_mortality - expected_mortality
    # Bind the results to the data frame
    NI_excess_mortality_results <- rbind(NI_excess_mortality_results, data.frame(
      Year = year_of_interest,
      WeekNumber = week_of_interest,
      Observed_Mortality = observed_mortality,
      Expected_Mortality = expected_mortality,
      Excess_Mortality = weekly_excess_mortality
    ))
  }
}

NI_weekly_excess_mortality_specific_year <- NI_excess_mortality_results %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

# Plot the weekly excess mortality trends for different sexes in the chosen year
ggplot(NI_weekly_excess_mortality_specific_year, aes(x = WeekNumber, y = Weekly_Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Northern Ireland Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())






# Northern Ireland Yearly
NI_yearly_totals <- NI_Deaths_all %>%
  group_by(Year) %>%
  summarise(Yearly_Deaths = sum(`All ages`, na.rm = TRUE))

# Initialize a data frame to hold the 5-year averages
NI_five_year_averages <- data.frame(Year = integer(), Average_Deaths = numeric())


for (year in 2017:2021) {
  if (year <= 2019) {
    average_deaths <- NI_yearly_totals %>%
      filter(Year >= (year - 5) & Year < year) %>%
      summarise(Average_Deaths = mean(Yearly_Deaths)) %>%
      pull(Average_Deaths)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    average_deaths <- NI_yearly_totals %>%
      filter(Year >= 2015 & Year <= 2019) %>%
      summarise(Average_Deaths = mean(Yearly_Deaths)) %>%
      pull(Average_Deaths)
  }
  NI_five_year_averages <- rbind(NI_five_year_averages, data.frame(Year = year, Average_Deaths = average_deaths))
}

# Calculate the excess deaths for the years 2017 to 2021
NI_yearly_totals <- NI_yearly_totals %>%
  left_join(NI_five_year_averages, by = "Year") %>%
  mutate(Excess_Deaths = ifelse(Year >= 2017, Yearly_Deaths - Average_Deaths, NA))


NI_filtered_data <- NI_yearly_totals %>% 
  filter(Year >= 2017 & Year <= 2021)

ggplot(NI_filtered_data, aes(x = Year, y = Excess_Deaths)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in NI",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())






# Comparison between three regions
# Combine datasets
combined_excess_mortality <- bind_rows(
  EngWal_data_all %>% mutate(Region = "EngWal"),
  Scot_excess_mortality_results %>% mutate(Region = "Scotland"),
  NI_excess_mortality_results %>% mutate(Region = "Northern Ireland")
)

# Calculate the total annual excess mortality by region and year
annual_excess_mortality_by_region <- combined_excess_mortality %>%
  group_by(Year, Region) %>%
  summarise(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')

# Plot the trend of total annual excess mortality by region
ggplot(annual_excess_mortality_by_region, aes(x = Year, y = Total_Excess_Mortality, color = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality by Region",
       x = "Year",
       y = "Total Excess Mortality",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Filter EngWal data for the "All" category and sum across all age groups
EngWal_weekly_total <- EngWal_data_all %>%
  filter(Category == "All") %>%
  group_by(Year, WeekNumber) %>%
  summarise(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')

# Rename the 'Total_Excess_Mortality' column to 'Excess_Mortality' for the EngWal data
EngWal_weekly_total <- EngWal_weekly_total %>%
  rename(Excess_Mortality = Total_Excess_Mortality) %>%
  mutate(Region = "EngWal")

# Now, combine the datasets
combined_excess_mortality <- bind_rows(
  EngWal_weekly_total,
  Scot_excess_mortality_results %>% select(Year, WeekNumber, Excess_Mortality) %>% mutate(Region = "Scotland"),
  NI_excess_mortality_results %>% select(Year, WeekNumber, Excess_Mortality) %>% mutate(Region = "Northern Ireland")
)


ggplot(combined_excess_mortality, aes(x = WeekNumber, y = Excess_Mortality, color = Region, group = interaction(Year, Region))) +
  geom_line() +
  geom_point() +
  facet_wrap(~Year, scales = 'free_y') +
  labs(title = "Weekly Excess Mortality by Region",
       x = "Week Number",
       y = "Excess Mortality",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Comparison about year 2020
# Filter the combined dataset for the year 2020
combined_excess_mortality_2020 <- combined_excess_mortality %>%
  filter(Year == 2020)

# Plot the trend of weekly excess mortality by region for 2020 with smoothing using GAM
ggplot(combined_excess_mortality_2020, aes(x = WeekNumber, y = Excess_Mortality, color = Region)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE) + # GAM smoothing
  labs(title = "Weekly Excess Mortality by Region for 2020",
       x = "Week Number",
       y = "Excess Mortality",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")




# Add the "Region" column to each dataset
EngWal_results <- EngWal_excess_mortality_by_week %>% mutate(Region = 'EngWal')
Scot_results <- Scot_excess_mortality_results %>% mutate(Region = 'Scotland')
NI_results <- NI_excess_mortality_results %>% mutate(Region = 'Northern Ireland')

# Combine the datasets into one
combined_excess_mortality_results <- bind_rows(
  EngWal_results,
  Scot_results,
  NI_results
)

# Filter the data for each region and for the year 2020
engwal_data <- combined_excess_mortality_results %>%
  filter(Year == 2020 & Region == "EngWal")

scot_data <- combined_excess_mortality_results %>%
  filter(Year == 2020 & Region == "Scotland")

ni_data <- combined_excess_mortality_results %>%
  filter(Year == 2020 & Region == "Northern Ireland")


plot_excess_mortality <- function(data, region_name, region_color) {
  ggplot(data, aes(x = WeekNumber, y = Excess_Mortality, fill = factor(Excess_Mortality > 0))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = region_name, 
                      values = c("FALSE" = "grey", "TRUE" = region_color),
                      labels = c("Negative Excess", "Positive Excess")) +
    labs(title = region_name,
         x = "Week number",
         y = "Excess deaths") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) 
}

engwal_plot <- plot_excess_mortality(engwal_data, "England and Wales", "#ff9999")
scot_plot <- plot_excess_mortality(scot_data, "Scotland", "#99ccff")
ni_plot <- plot_excess_mortality(ni_data, "Northern Ireland", "#99ff99")

# Combine the plots
excess_deaths_combined_plot <- grid.arrange(engwal_plot, scot_plot, ni_plot, ncol = 3)


# Filter for year 2020 and calculate cumulative excess mortality
Combined_Cumulative_Deaths_2020 <- combined_excess_mortality_results %>%
  filter(Year == 2020) %>%
  arrange(Region, WeekNumber) %>%
  group_by(Region) %>%
  mutate(Cumulative_Excess_Mortality = cumsum(Excess_Mortality)) %>%
  ungroup()

# Plot the cumulative excess mortality by week number for each region
ggplot(data = Combined_Cumulative_Deaths_2020, aes(x = WeekNumber, y = Cumulative_Excess_Mortality, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Cumulative Excess Mortality by Week Number for 2020",
       x = "Week Number",
       y = "Cumulative Excess Mortality") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) 











########################################Method 2########################################
# England and Wales - Method 2 - Calculating Weekly Excess Deaths but convert 52 weeks to 53 weeks
prepare_data_2 <- function(sheet_name) {
  data <- read_excel("Weekly_Deaths.xlsx", sheet = sheet_name, skip = 1)
  data <- data %>%
    separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
    mutate(
      Year = as.integer(Year),
      WeekNumber = as.integer(WeekNumber)
    )
  age_group_cols <- setdiff(names(data), c("Year", "WeekNumber", "All ages"))
  data_long <- data %>%
    pivot_longer(
      cols = age_group_cols,
      names_to = "Age_Group",
      values_to = "Deaths"
    ) %>%
    mutate(
      Age_Group = str_replace(Age_Group, "Under_1", "<1")
    )
  return(data_long)
}


add_53rd_week <- function(data) {
  # Initialize an empty data frame to store the new 53rd week rows
  new_data <- data[0,]
  # Identify years in the dataset and filter out the last year since it cannot have a 53rd week added from the next year
  all_years <- unique(data$Year)
  years_to_process <- all_years[all_years < max(all_years)]
  
  for(year in years_to_process) {
    if(!53 %in% data$WeekNumber[data$Year == year] && (year + 1) %in% all_years) {
      # Get the 52nd week data of the current year
      week_52_data <- data[data$Year == year & data$WeekNumber == 52,]
      # Get the 1st week data of the next year
      week_1_next_year_data <- data[data$Year == year + 1 & data$WeekNumber == 1,]
      age_groups <- unique(c(week_52_data$Age_Group, week_1_next_year_data$Age_Group))
      for(age_group in age_groups) {
        # Filter data by age group
        week_52_age_data <- week_52_data[week_52_data$Age_Group == age_group,]
        week_1_next_year_age_data <- week_1_next_year_data[week_1_next_year_data$Age_Group == age_group,]
        
        # Average the deaths for the age group if both weeks have data for it
        if(nrow(week_52_age_data) > 0 & nrow(week_1_next_year_age_data) > 0) {
          avg_deaths <- mean(c(week_52_age_data$Deaths, week_1_next_year_age_data$Deaths), na.rm = TRUE)
          
          # Create a new row for the 53rd week
          new_row <- week_52_age_data[1, ]
          new_row$WeekNumber <- 53
          new_row$Deaths <- avg_deaths
          
          # Append this new row to the new_data
          new_data <- rbind(new_data, new_row)
        }
      }
    }
  }
  
  # Append the new 53rd week data to the original dataset and sort
  if(nrow(new_data) > 0) {
    data <- rbind(data, new_data)
    data <- data[order(data$Year, data$WeekNumber, data$Age_Group), ]
  }
  
  return(data)
}


EngWal_death_female_2 <- prepare_data_2("Female")
EngWal_death_male_2 <- prepare_data_2("Male")
EngWal_death_all_2 <- prepare_data_2("All")

EngWal_dfemale <- add_53rd_week(EngWal_death_female_2)
EngWal_dmale <- add_53rd_week(EngWal_death_male_2)
EngWal_dall <- add_53rd_week(EngWal_death_all_2)

calculate_excess_deaths_2 <- function(year, week, age_group, deaths_data) {
  if (year < 2021) {
    previous_years <- (year - 5):(year - 1)
  } else if (year == 2021) {
    previous_years <- 2015:2019
  } else {
    previous_years <- setdiff((year - 5):(year - 1), 2020)
  }
  
  # Calculate the average weekly deaths for previous years
  weekly_expected_mortality <- deaths_data %>%
    filter(Year %in% previous_years, WeekNumber == week, Age_Group == age_group) %>%
    summarise(Average_Deaths = mean(Deaths, na.rm = TRUE)) %>%
    pull(Average_Deaths)
  
  # Calculate the observed weekly deaths for the specified year
  weekly_observed_mortality <- deaths_data %>%
    filter(Year == year, WeekNumber == week, Age_Group == age_group) %>%
    summarise(Week_Deaths = sum(Deaths, na.rm = TRUE)) %>%
    pull(Week_Deaths)
  
  # Calculate excess deaths
  excess_deaths <- weekly_observed_mortality - weekly_expected_mortality
  
  # Return a data frame with the calculated weekly excess mortality
  return(data.frame(Year = year, WeekNumber = week, Age_Group = age_group,  Observed_Mortality = weekly_observed_mortality, Expected_Mortality = weekly_expected_mortality, Excess_Mortality = excess_deaths))
}


# Initialize an empty data frame to store excess mortality data
EngWal_excess_mortality_data_2 <- data.frame()
years_2 <- 2017:2021
weeks_2 <- 1:53
categories <- list(Female = EngWal_dfemale, Male = EngWal_dmale, All = EngWal_dall)

# Loop through categories, years, weeks, and age groups to calculate excess mortality
for (category in names(categories)) {
  sheet_data <- categories[[category]]
  age_groups <- unique(sheet_data$Age_Group)
  for (year in years_2) {
    for (week in weeks_2) {
      for (age_group in age_groups) {
        # Calculate weekly excess mortality
        weekly_excess_mortality <- calculate_excess_deaths_2(year, week, age_group, sheet_data)
        # Add the result to the data frame along with the category
        EngWal_excess_mortality_data_2 <- rbind(EngWal_excess_mortality_data_2, cbind(Category = category, weekly_excess_mortality))
      }
    }
  }
}



EngWal_annual_excess_mortality_2 <- EngWal_excess_mortality_data_2 %>%
  group_by(Year, Category) %>%
  summarise(Total_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")
# Plot the trend of total annual excess mortality over the years for each category
ggplot(EngWal_annual_excess_mortality_2, aes(x = Year, y = Total_Excess_Mortality, group = Category, color = Category)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in England and Wales",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())


specific_year <- 2020
EngWal_weekly_excess_mortality_specific_year_2 <- EngWal_excess_mortality_data_2 %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber, Category) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

# Plot the weekly excess mortality trends for different sexes in the chosen year
ggplot(EngWal_weekly_excess_mortality_specific_year_2, aes(x = WeekNumber, y = Weekly_Excess_Mortality, group = Category, color = Category)) +
  geom_line() +
  geom_point() +
  labs(title = paste("England and Wales Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())




EngWal_total_excess_mortality_2 <- EngWal_excess_mortality_data_2 %>%
  group_by(Age_Group, Category) %>%
  summarise(Total_Excess = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

ggplot(EngWal_total_excess_mortality_2, aes(x = Age_Group, y = Total_Excess, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(title = "Total Excess Mortality by Age Group and Category (2017-2021)",
       x = "Age Group",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "All" = "grey50"))


for (year in 2017:2021) {
  # Filter data for the current year
  yearly_data_2 <- EngWal_excess_mortality_data_2 %>%
    filter(Year == year) %>%
    group_by(Age_Group, Category) %>%
    summarise(Total_Excess = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")

  
  current_year_2 <- ggplot(yearly_data_2, aes(x = Age_Group, y = Total_Excess, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    labs(title = paste("England and Wales Total Excess Mortality by Age Group and Gender in", year),
         x = "Age Group",
         y = "Total Excess Mortality") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "All" = "grey50"))
  
  print(current_year_2)
}



# Weekly Aggregate
# First, filter for the category "All"
EngWal_data_all_2 <- EngWal_excess_mortality_data_2 %>%
  filter(Category == "All")
# Group by 'Year' and 'WeekNumber' and then summarize the excess mortality
EngWal_excess_mortality_all_2 <- EngWal_data_all_2 %>%
  group_by(Year, WeekNumber) %>%
  summarize(Observed_Mortality = sum(Observed_Mortality), Expected_Mortality = sum(Expected_Mortality), Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = 'drop')
EngWal_excess_mortality_long_2 <- EngWal_excess_mortality_data_2 %>%
  group_by(Year, Age_Group, WeekNumber) %>%
  summarise(Weekly_Excess_Deaths = sum(Excess_Mortality, na.rm = TRUE)) %>%
  ungroup()


five_yr_weekly_plots <- list()
for(week in unique(EngWal_excess_mortality_long_2$WeekNumber)) {
  # Filter the data for the specific week
  weekly_data <- EngWal_excess_mortality_long_2 %>% filter(WeekNumber == week)
  
  # Create the plot for the current week
  five_yr_weekly_plots[[week]] <- ggplot(weekly_data, aes(x = Age_Group, y = Weekly_Excess_Deaths, group = Year, color = as.factor(Year))) +
    geom_line() +
    geom_point() +
    labs(title = paste("Weekly Excess Mortality by Age Group for Week", week),
         x = "Age Group",
         y = "Weekly Excess Deaths") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_brewer(palette = "Set1") +
    guides(color = guide_legend(title = "Year"))
}





# EngWal Yearly - we don't sum all week deaths,use original data, not converted data
calculate_expected_annual_deaths_2 <- function(year, data) {
  if (year < 2021) {
    previous_years <- (year - 5):(year - 1)
  } else if (year == 2021) {
    previous_years <- 2015:2019
  } else {
    # Exclude 2020 from the set of previous years if the year is after 2021
    previous_years <- setdiff((year - 5):(year - 1), 2020)
  }
  
  # Filter the data for the past five years
  past_years_data <- data %>%
    filter(Year %in% previous_years)
  
  # Calculate the sum of 'All ages' for each week over the past five years
  weekly_totals <- past_years_data %>%
    group_by(Year, WeekNumber) %>%
    summarise(All_Ages = first(`All ages`))
  print(weekly_totals)
  
  # Calculate the yearly total by summing the weekly totals
  yearly_totals <- weekly_totals %>%
    group_by(Year) %>%
    summarise(Yearly_Total_All_Ages = sum(All_Ages, na.rm = TRUE)) %>%
    ungroup()
  print(yearly_totals)
  
  # Calculate the average of these yearly totals to get the expected deaths for the specified year
  expected_deaths <- yearly_totals %>%
    summarise(Expected_Annual_Deaths = mean(Yearly_Total_All_Ages, na.rm = TRUE))
  
  # Add Year column to the expected_deaths data frame
  expected_deaths$Year <- year
  
  return(expected_deaths)
}

categories_2 <- list(Female = EngWal_death_female_2, Male = EngWal_death_male_2, All = EngWal_death_all_2)


expected_annual_deaths_results_2 <- data.frame()

# Loop through each category and year to calculate expected annual deaths
for (category_name in names(categories_2)) {
  for (year in years_2) {
    expected_deaths_data <- calculate_expected_annual_deaths_2(year, categories_2[[category_name]])
    
    expected_deaths_data$Category <- category_name
    
    expected_annual_deaths_results_2 <- rbind(expected_annual_deaths_results_2, expected_deaths_data)
  }
}


# Ignore Age-group
calculate_observed_annual_deaths_2 <- function(year, data) {
  # Filter the data for the specified year
  year_data <- data %>%
    filter(Year == year)
  
  # Calculate the sum of 'All ages' for each week in the specified year
  weekly_totals <- year_data %>%
    group_by(Year, WeekNumber) %>%
    summarise(Weekly_All_Ages = first(`All ages`), .groups = 'drop') %>%
    ungroup()
  
  # Calculate the yearly total by summing the weekly totals
  yearly_total <- weekly_totals %>%
    summarise(Yearly_Total_All_Ages = sum(Weekly_All_Ages, na.rm = TRUE), .groups = 'drop')
  
  # Add Year column to the yearly_total data frame
  yearly_total$Year <- year
  
  return(yearly_total)
}


observed_annual_deaths_results_2 <- data.frame()

for (category_name in names(categories_2)) {
  for (year in years) {
    # Apply the function to calculate observed annual deaths
    observed_deaths_data_2 <- calculate_observed_annual_deaths_2(year, categories_2[[category_name]])
    
    # Add category information to the results
    observed_deaths_data_2$Category <- category_name
    
    # Append the results to the data frame
    observed_annual_deaths_results_2 <- rbind(observed_annual_deaths_results_2, observed_deaths_data_2)
  }
}


# Excess deaths
# Merge observed and expected annual deaths results based on category and year
merged_data <- merge(observed_annual_deaths_results_2, expected_annual_deaths_results_2, by = c("Category", "Year"))

# Calculate excess deaths
merged_data$Excess_Deaths <- merged_data$Yearly_Total_All_Ages - merged_data$Expected_Annual_Deaths

excess_deaths_results_EngWal_2 <- merged_data[, c("Category", "Year", "Yearly_Total_All_Ages","Expected_Annual_Deaths","Excess_Deaths")]

EngWal_excess_mortality_results_yearly_2 <- excess_deaths_results_EngWal_2 %>%
  filter(Category == "All")




# Scotland Weekly Excess Deaths Calculation ( No gender difference in dataset)
add_53rd_week_SNI <- function(data) {
  # Initialize an empty data frame to store the new 53rd week rows
  new_data <- data[0,]
  
  # Identify years in the dataset and filter out the last year since it cannot have a 53rd week added from the next year
  all_years <- unique(data$Year)
  years_to_process <- all_years[all_years < max(all_years)]
  
  for(year in years_to_process) {
    # Check if this year does not have a 53rd week
    if(!53 %in% data$WeekNumber[data$Year == year]) {
      # Check if the next year is in the dataset
      if((year + 1) %in% all_years) {
        # Get the 52nd week data of the current year
        week_52_data <- data[data$Year == year & data$WeekNumber == 52,]
        # Get the 1st week data of the next year
        week_1_next_year_data <- data[data$Year == year + 1 & data$WeekNumber == 1,]
        
        # Average the values for the 52nd and 1st weeks if both weeks have data
        if(nrow(week_52_data) > 0 & nrow(week_1_next_year_data) > 0) {
          avg_value <- mean(c(week_52_data[["All ages"]], week_1_next_year_data[["All ages"]]), na.rm = TRUE)
          
          # Create a new row for the 53rd week
          new_row <- week_52_data[1, ]
          new_row$WeekNumber <- 53
          new_row[["All ages"]] <- avg_value
          
          # Append this new row to the new_data
          new_data <- rbind(new_data, new_row)
        }
      }
    }
  }
  
  # Combine the original data with the new 53rd week rows
  combined_data <- rbind(data, new_data)
  
  # Return the combined data sorted by year and week number
  combined_data <- combined_data[order(combined_data$Year, combined_data$WeekNumber),]
  return(combined_data)
}

Scot_Deaths_all_2 <- read_excel("Weekly_Deaths_Scotland.xlsx", sheet = 'All', skip = 1)

Scot_Deaths_all_2 <- Scot_Deaths_all_2 %>%
  separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
  mutate(
    Year = as.integer(Year),
    WeekNumber = as.integer(WeekNumber)
  )

Scot_dall <-add_53rd_week_SNI(Scot_Deaths_all)


Scot_excess_mortality_results_weekly_2 <- data.frame()

for (year_2 in 2017:2021) {
  if (year_2 <= 2019) {
    previous_years <- (year_2 - 5):(year_2 - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  
  # Loop through all weeks of the year
  for (week_2 in 1:53) {
    # Get observed mortality for the specified year and week
    observed_mortality <- Scot_dall %>%
      filter(Year == year_2, WeekNumber == week_2) %>%
      summarise(Week_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
      pull(Week_Deaths)
    
    # Calculate expected mortality as the average deaths for the corresponding week over the previous five years
    expected_mortality <- Scot_dall %>%
      filter(Year %in% previous_years, WeekNumber == week_2) %>%
      summarise(Average_Deaths = mean(`All ages`, na.rm = TRUE)) %>%
      pull(Average_Deaths)
    
    # Calculate weekly excess mortality
    weekly_excess_mortality <- observed_mortality - expected_mortality
    
    # Bind the results to the data frame
    Scot_excess_mortality_results_weekly_2 <- rbind(Scot_excess_mortality_results_weekly_2, data.frame(
      Year = year_2,
      WeekNumber = week_2,
      Observed_Mortality = observed_mortality,
      Expected_Mortality = expected_mortality,
      Excess_Mortality = weekly_excess_mortality
    ))
  }
}

print(Scot_excess_mortality_results_weekly_2)



# Scotland Yearly - we will not use cumulative, we use sum of week 1 to week 52 for normal years
# Sum week 1 to week 53 for year 2015, 2020

Scot_excess_mortality_results_yearly_2 <- data.frame()
for (year_2 in 2017:2021) {
  # Define the range of previous five years, with special handling for 2020 and 2021
  if (year_2 <= 2019) {
    previous_years <- (year_2 - 5):(year_2 - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  
  observed_mortality <- Scot_dall %>%
    filter(Year == year_2) %>%
    summarise(Yearly_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
    pull(Yearly_Deaths)
  
  expected_mortality <- Scot_dall %>%
    filter(Year %in% previous_years) %>%
    group_by(Year) %>%
    summarise(Average_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
    summarise(Expected_Deaths = mean(Average_Deaths, na.rm = TRUE)) %>%
    pull(Expected_Deaths)
  
  # Calculate yearly excess mortality
  yearly_excess_mortality <- observed_mortality - expected_mortality
  
  Scot_excess_mortality_results_yearly_2 <- rbind(Scot_excess_mortality_results_yearly_2, data.frame(
    Year = year_2,
    Observed_Mortality = observed_mortality,
    Expected_Mortality = expected_mortality,
    Excess_Mortality = yearly_excess_mortality
  ))
}

print(Scot_excess_mortality_results_yearly_2)


# Scotland Plot yearly
ggplot(Scot_excess_mortality_results_yearly_2, aes(x = Year, y = Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in Scotland",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Year 2020 Scotland plot
Scot_weekly_excess_mortality_specific_year_2 <- Scot_excess_mortality_results_weekly_2 %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")
# Plot the weekly excess mortality trends for different sexes in the chosen year
ggplot(Scot_weekly_excess_mortality_specific_year_2, aes(x = WeekNumber, y = Weekly_Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Scotland Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Cumulative Deaths in Scotland
# Calculate the cumulative excess mortality for each year
Scot_Cumulative_Deaths_2 <- Scot_excess_mortality_results_weekly_2 %>%
  group_by(Year) %>%
  arrange(Year, WeekNumber) %>%
  mutate(Cumulative_Excess_Mortality = cumsum(Excess_Mortality)) %>%
  ungroup()

# Plot the cumulative excess mortality by week number for each year
ggplot(data = Scot_Cumulative_Deaths_2, aes(x = WeekNumber, y = Cumulative_Excess_Mortality, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Cumulative Excess Mortality by Week Number in Scotland",
       x = "Week Number",
       y = "Cumulative Excess Mortality",
       color = "Year") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") 









# Northern Ireland Weekly

NI_Deaths_all_2 <- read_excel("Weekly_Deaths_NI1.xlsx", sheet = 'All', skip = 1)

NI_Deaths_all_2 <- NI_Deaths_all_2 %>%
  select(`Year & week number`, `All ages`)

NI_Deaths_all_2 <- NI_Deaths_all_2 %>%
  separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
  mutate(
    Year = as.integer(Year),
    WeekNumber = as.integer(WeekNumber)
  )

NI_dall <-add_53rd_week_SNI(NI_Deaths_all_2)

NI_excess_mortality_results_weekly_2 <- data.frame()


for (year_2 in 2017:2021) {
  # Define the range of previous five years, with special handling for 2020 and 2021
  if (year_2 <= 2019) {
    previous_years <- (year_2 - 5):(year_2 - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  
  # Loop through all weeks of the year
  for (week_2 in 1:53) {
    # Get observed mortality for the specified year and week
    observed_mortality <- NI_dall %>%
      filter(Year == year_2, WeekNumber == week_2) %>%
      summarise(Week_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
      pull(Week_Deaths)
    
    # Calculate expected mortality as the average deaths for the corresponding week over the previous five years
    expected_mortality <- NI_dall %>%
      filter(Year %in% previous_years, WeekNumber == week_2) %>%
      summarise(Average_Deaths = mean(`All ages`, na.rm = TRUE)) %>%
      pull(Average_Deaths)
    
    # Calculate weekly excess mortality
    weekly_excess_mortality <- observed_mortality - expected_mortality
    
    # Bind the results to the data frame
    NI_excess_mortality_results_weekly_2 <- rbind(NI_excess_mortality_results_weekly_2, data.frame(
      Year = year_2,
      WeekNumber = week_2,
      Observed_Mortality = observed_mortality,
      Expected_Mortality = expected_mortality,
      Excess_Mortality = weekly_excess_mortality
    ))
  }
}


print(NI_excess_mortality_results_weekly_2)






# Norther Ireland Yearly
NI_excess_mortality_results_yearly_2 <- data.frame()
for (year_2 in 2017:2021) {
  # Define the range of previous five years, with special handling for 2020 and 2021
  if (year_2 <= 2019) {
    previous_years <- (year_2 - 5):(year_2 - 1)
  } else {
    # For 2020 and 2021, we use 2015-2019 due to the COVID-19 pandemic affecting 2020 data
    previous_years <- 2015:2019
  }
  
  # Get observed mortality for the specified year
  observed_mortality <- NI_dall %>%
    filter(Year == year_2) %>%
    summarise(Yearly_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
    pull(Yearly_Deaths)
  
  # Calculate the expected mortality as the average of previous years' deaths
  expected_mortality <- NI_dall %>%
    filter(Year %in% previous_years) %>%
    group_by(Year) %>%
    summarise(Average_Deaths = sum(`All ages`, na.rm = TRUE)) %>%
    summarise(Expected_Deaths = mean(Average_Deaths, na.rm = TRUE)) %>%
    pull(Expected_Deaths)
  
  # Calculate yearly excess mortality
  yearly_excess_mortality <- observed_mortality - expected_mortality
  
  # Bind the results to the data frame
  NI_excess_mortality_results_yearly_2 <- rbind(NI_excess_mortality_results_yearly_2, data.frame(
    Year = year_2,
    Observed_Mortality = observed_mortality,
    Expected_Mortality = expected_mortality,
    Excess_Mortality = yearly_excess_mortality
  ))
}


print(NI_excess_mortality_results_yearly_2)

# Plot for NI yearly
ggplot(NI_excess_mortality_results_yearly_2, aes(x = Year, y = Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Annual Excess Mortality Over Years in Northern Ireland",
       x = "Year",
       y = "Total Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())

NI_weekly_excess_mortality_specific_year_2 <- NI_excess_mortality_results_weekly_2 %>%
  filter(Year == specific_year) %>%
  group_by(WeekNumber) %>%
  summarise(Weekly_Excess_Mortality = sum(Excess_Mortality, na.rm = TRUE), .groups = "drop")


ggplot(NI_weekly_excess_mortality_specific_year_2, aes(x = WeekNumber, y = Weekly_Excess_Mortality)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Northern Ireland Weekly Excess Mortality Trends in", specific_year),
       x = "Week Number",
       y = "Weekly Excess Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Comparison

EngWal_results_2 <- EngWal_excess_mortality_results_yearly_2 %>%
  rename(
    Observed_Mortality = Yearly_Total_All_Ages,
    Expected_Mortality = Expected_Annual_Deaths,
    Excess_Mortality = Excess_Deaths
  ) %>%
  select(-Category) %>%
  mutate(Region = 'EngWal')

# Add the "Region" column to the Scot and NI datasets
Scot_results_2 <- Scot_excess_mortality_results_yearly_2 %>%
  mutate(Region = 'Scotland')

NI_results_2 <- NI_excess_mortality_results_yearly_2 %>%
  mutate(Region = 'Northern Ireland')

# Combine the datasets into one
combined_yearly_results_2 <- bind_rows(
  EngWal_results_2,
  Scot_results_2,
  NI_results_2
)

print(combined_yearly_results_2)



# Generate a plot with trends for different regions
plot_yearly_2 <- ggplot(combined_yearly_results_2, aes(x = Year, y = Excess_Mortality, group = Region, color = Region)) +
  geom_line() +  # Plot lines for each region
  geom_point() +  # Add points to the lines
  theme_minimal() +
  labs(
    title = "Excess Mortality by Region and Year",
    x = "Year",
    y = "Excess Mortality"
  ) +
  theme(legend.position = "bottom") 


print(plot_yearly_2)



# Weekly Combined

EngWal_weekly_results_2 <- EngWal_excess_mortality_all_2 %>%
  mutate(Region = 'EngWal')

# Add the "Region" column to the Scot and NI datasets
Scot_weekly_results_2 <- Scot_excess_mortality_results_weekly_2 %>%
  mutate(Region = 'Scotland')

NI_weekly_results_2 <- NI_excess_mortality_results_weekly_2 %>%
  mutate(Region = 'Northern Ireland')

# Combine the datasets into one
combined_weekly_results_2 <- bind_rows(
  EngWal_weekly_results_2,
  Scot_weekly_results_2,
  NI_weekly_results_2
)


print(combined_weekly_results_2)


# Plot weekly in different regions
plot_weekly_2 <- ggplot(combined_weekly_results_2, aes(x = WeekNumber, y = Excess_Mortality, group = interaction(Year, Region), color = Region)) +
  geom_line() +  
  facet_grid(Region ~ Year, scales = "free_y") +  # Create a separate plot for each region and year combination
  theme_minimal() +
  labs(
    title = "Weekly Excess Deaths by Region and Year",
    x = "Week Number",
    y = "Excess Deaths"
  ) +
  theme(legend.position = "bottom")  


print(plot_weekly_2)




# Weekly plot with 4-week periods for 2020
weekly_2020_data <- combined_weekly_results_2 %>%
  filter(Year == 2020) %>%
  mutate(Period = ceiling(WeekNumber / 4)) %>%
  group_by(Region, Period) %>%
  summarise(
    Observed = mean(Observed_Mortality),
    Expected = mean(Expected_Mortality),
    .groups = "drop" 
  )

# Calculate the Period variable
combined_weekly_result_period <- combined_weekly_results_2 %>%
  mutate(Period = ceiling(WeekNumber / 4))

# Calculate the range for each Period for the years 2016-2019
weekly_range_data <- combined_weekly_result_period %>%
  filter(Year %in% 2016:2019) %>%
  group_by(Region, Period) %>%
  summarise(
    Range_Lower = min(Observed_Mortality),
    Range_Upper = max(Observed_Mortality),
    .groups = "drop"
  )


weekly_plot_2020 <- ggplot(weekly_2020_data, aes(x = Period)) +
  geom_ribbon(data = weekly_range_data, aes(x = Period, ymin = Range_Lower, ymax = Range_Upper, fill = "Range"), alpha = 0.3) +
  geom_line(aes(y = Observed, color = "Observed"), linewidth = 1) +
  geom_line(aes(y = Expected, color = "Expected"), linewidth = 1, linetype = "dashed") +
  facet_wrap(~Region, scales = "free_y") +
  scale_color_manual(values = c("Observed" = "black", "Expected" = "red")) +
  scale_fill_manual(values = c("Range" = "grey")) +
  labs(
    title = "Weekly Excess Mortality for 2020 by 4-Week Periods",
    x = "4-Week Period",
    y = "Mortality"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
print(weekly_plot_2020)






# Filter the data for each region and for the year 2020
engwal_data_2 <- combined_weekly_results_2 %>%
  filter(Year == 2020 & Region == "EngWal")

scot_data_2 <- combined_weekly_results_2 %>%
  filter(Year == 2020 & Region == "Scotland")

ni_data_2 <- combined_weekly_results_2 %>%
  filter(Year == 2020 & Region == "Northern Ireland")


plot_excess_mortality_2 <- function(data, region_name, region_color) {
  ggplot(data, aes(x = WeekNumber, y = Excess_Mortality, fill = factor(Excess_Mortality > 0))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = region_name, 
                      values = c("FALSE" = "grey", "TRUE" = region_color),
                      labels = c("Negative Excess", "Positive Excess")) +
    labs(title = region_name,
         x = "Week number",
         y = "Excess deaths") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) 
}

engwal_plot_2 <- plot_excess_mortality_2(engwal_data_2, "England and Wales", "#ff9999")
scot_plot_2 <- plot_excess_mortality_2(scot_data_2, "Scotland", "#99ccff")
ni_plot_2 <- plot_excess_mortality_2(ni_data_2, "Northern Ireland", "#99ff99")

# Combine the plots
excess_deaths_combined_plot_2 <- grid.arrange(engwal_plot_2, scot_plot_2, ni_plot_2, ncol = 3)







# Comparison about year 2020
# Filter the combined dataset for the year 2020
combined_excess_mortality_2020_2 <- combined_weekly_results_2 %>%
  filter(Year == 2020)
# Plot the trend of weekly excess mortality by region for 2020 with smoothing using GAM
ggplot(combined_excess_mortality_2020_2, aes(x = WeekNumber, y = Excess_Mortality, color = Region)) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se = FALSE) + # GAM smoothing
  labs(title = "Weekly Excess Mortality by Region for 2020",
       x = "Week Number",
       y = "Excess Mortality",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")





# Filter for year 2020 and calculate cumulative excess mortality
Combined_Cumulative_Deaths_2020_2 <- combined_weekly_results_2 %>%
  filter(Year == 2020) %>%
  arrange(Region, WeekNumber) %>%
  group_by(Region) %>%
  mutate(Cumulative_Excess_Mortality = cumsum(Excess_Mortality)) %>%
  ungroup()

# Plot the cumulative excess mortality by week number for each region
ggplot(data = Combined_Cumulative_Deaths_2020_2, aes(x = WeekNumber, y = Cumulative_Excess_Mortality, group = Region, color = Region)) +
  geom_line() +
  labs(title = "Cumulative Excess Mortality by Week Number for 2020",
       x = "Week Number",
       y = "Cumulative Excess Mortality") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) 



# Output Comparison Weekly
# Method 1 dataset
combined_excess_mortality_results
# Method 2 dataset
combined_weekly_results_2

comparison_data_weekly <- merge(combined_excess_mortality_results, combined_weekly_results_2,
                         by = c("Year", "WeekNumber", "Region"),
                         suffixes = c("_Method1", "_Method2"))

comparison_data_2020_weekly <- comparison_data_weekly %>%
  filter(Year == 2020) %>%
  mutate(Difference = Excess_Mortality_Method1 - Excess_Mortality_Method2)

comparison_table_2020_weekly <- comparison_data_2020_weekly %>%
  select(Year, WeekNumber, Region, Excess_Mortality_Method1, Excess_Mortality_Method2, Difference)


print(comparison_table_2020_weekly)


# Output Comparison Yearly
# Method 1 dataset
annual_excess_mortality_by_region
# Method 2 dataset
combined_yearly_results_2

# Merge the datasets on 'Year' and 'Region'
yearly_comparison_data <- merge(annual_excess_mortality_by_region, combined_yearly_results_2,
                                by = c("Year", "Region"))


yearly_comparison_data <- yearly_comparison_data %>%
  rename(Excess_Mortality_Method1 = Total_Excess_Mortality,
         Excess_Mortality_Method2 = Excess_Mortality)

yearly_comparison_data <- yearly_comparison_data %>%
  mutate(Difference = Excess_Mortality_Method1 - Excess_Mortality_Method2)

yearly_comparison_table <- yearly_comparison_data %>%
  select(Year, Region, Excess_Mortality_Method1, Excess_Mortality_Method2, Difference)

print(yearly_comparison_table)




