library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(purrr)

# England and Wales
# Load the population data
EngWal_pop <- read_excel("Population_Yearly_Combine.xlsx")
Standard_pop <- read_excel("Standard_Pop.xlsx")

# Convert 53 weeks to 52 weeks 
process_sheet_data <- function(sheet_name) {
  EngWal_death <- read_excel("Weekly_Deaths.xlsx", sheet = sheet_name, skip = 1)
  
  # Separate "Year & week number" into "Year" and "WeekNumber"
  EngWal_death <- EngWal_death %>%
    separate(`Year & week number`, into = c("Year", "WeekNumber"), sep = "-", convert = TRUE) %>%
    mutate(
      Year = as.integer(Year),
      WeekNumber = as.integer(WeekNumber)
    )
  # ---- Calculate the average for weeks 52 and 53 in years with 53 weeks ----
  age_group_cols <- setdiff(names(EngWal_death), c("Year", "WeekNumber", "All ages"))
  
  # ---- Identify the years with 53 weeks ----
  years_with_53_weeks <- EngWal_death %>%
    count(Year, WeekNumber) %>%
    filter(WeekNumber == 53) %>%
    pull(Year)
  
  ## For each year with 53 weeks, average the data for week 52 and week 1 of the following year
  for (year in years_with_53_weeks) {
    week_52_data <- EngWal_death %>% 
      filter(Year == year & WeekNumber == 52) %>%
      select(all_of(age_group_cols))
    
    week_1_next_year_data <- EngWal_death %>%
      filter(Year == year + 1 & WeekNumber == 1) %>%
      select(all_of(age_group_cols))
    
    # If there is no week 1 data for the next year, skip the interpolation
    if (nrow(week_1_next_year_data) == 0) {
      next
    }
    
    # Calculate the average for each age group column
    averaged_data <- map2_dfr(week_52_data, week_1_next_year_data, ~ ifelse(is.na(.x) | is.na(.y), NA, (.x + .y) / 2))
    
    # Update the week 52 data with the averaged values
    EngWal_death <- EngWal_death %>%
      mutate(across(all_of(age_group_cols), ~ if_else(Year == year & WeekNumber == 52, averaged_data[[cur_column()]], .)))
  }
  
  # Remove the rows for week 53 in the years with 53 weeks
  EngWal_death <- EngWal_death %>%
    filter(!(Year %in% years_with_53_weeks & WeekNumber == 53))
  
  # Add a new column for the total deaths by summing across the age group columns
  EngWal_death <- EngWal_death %>%
    mutate(Total_Deaths = rowSums(select(., all_of(age_group_cols)), na.rm = TRUE))
  
  # Reorder columns to move Total_Deaths next to All ages
  EngWal_death <- EngWal_death %>%
    select(Year, WeekNumber, `All ages`, Total_Deaths, everything())
  
  return(EngWal_death)
}

deaths_all <- process_sheet_data("All")
deaths_female <- process_sheet_data("Female")
deaths_male <- process_sheet_data("Male")


EngWal_pop <- EngWal_pop %>%
  filter(Location == "EngWal") %>%
  pivot_longer(
    cols = -c(Year, Location, Gender), 
    names_to = "Age_Group",
    values_to = "TotPopulation"
  ) %>%
  mutate(
    Gender = str_replace(Gender, "Combine", "All"), 
    Year = as.numeric(Year) # Make sure Year is numeric
  )

EngWal_pop_female <- EngWal_pop %>%
  filter(Gender == "Female")

EngWal_pop_male <- EngWal_pop %>%
  filter(Gender == "Male")

EngWal_pop_all <- EngWal_pop %>%
  filter(Gender == "All")

process_deaths_data <- function(deaths_data, gender) {
  deaths_data %>%
    mutate(Year = as.integer(Year), WeekNumber = as.integer(WeekNumber)) %>%
    # Ensure numeric columns are converted to character before pivoting
    mutate(across(where(is.numeric), as.character)) %>%
    select(-`All ages`, -`Total_Deaths`) %>% # 
    # Pivot longer, excluding 'All ages' and 'Total_Deaths' from pivoting
    pivot_longer(
      cols = -c(Year, WeekNumber),
      names_to = "Age_Group",
      values_to = "Deaths"
    ) %>%
    mutate(
      Deaths = as.numeric(Deaths),
      Gender = gender
    )
}

deaths_all_long <- process_deaths_data(deaths_all, "All")
deaths_male_long <- process_deaths_data(deaths_male, "Male")
deaths_female_long <- process_deaths_data(deaths_female, "Female")


interpolate_population <- function(population_data) {
  interpolated_results <- data.frame()
  
  # Get the list of years and age groups
  years <- unique(population_data$Year)
  age_groups <- unique(population_data$Age_Group)
  genders <- unique(population_data$Gender)
  
  # Loop over each age group and year
  for(age_group in age_groups) {
    for(year in years) {
      # Skip the first and last year since we can't interpolate without data from the year before and after
      if (year == min(years) || year == max(years)) {
        next
      }
      
      # Get the population for the previous, current, and next year
      pop_prev = population_data$TotPopulation[population_data$Year == year-1 & population_data$Age_Group == age_group]
      pop_curr = population_data$TotPopulation[population_data$Year == year & population_data$Age_Group == age_group]
      pop_next = population_data$TotPopulation[population_data$Year == year+1 & population_data$Age_Group == age_group]
      
      # Interpolate the population for each week
      for(week in 1:52) {
        if(week <= 26) {
          # For weeks in the first half of the year, interpolate between the previous and current year
          interpolated_pop = ((26 - week) / 26) * pop_prev + (week / 26) * pop_curr
        } else {
          # For weeks in the second half of the year, interpolate between the current and next year
          interpolated_pop = ((52 - week) / 26) * pop_curr + ((week - 26) / 26) * pop_next
        }
        
        # Add the interpolated data to the results data frame
        interpolated_results <- rbind(interpolated_results, data.frame(Year = year, Age_Group = age_group, Gender = genders, WeekNumber = week, TotPopulation = interpolated_pop))
      }
    }
  }
  
  return(interpolated_results)
}

# Apply the interpolation function to the population data
interpolated_pop_female <- interpolate_population(EngWal_pop_female)
interpolated_pop_male <- interpolate_population(EngWal_pop_male)
interpolated_pop_all <- interpolate_population(EngWal_pop_all)


calculate_ASMR <- function(week, year, gender, deaths_data, interpolated_pop_data, standard_pop) {
  
  # Filter the deaths data for the specified week, year, and gender
  deaths_filtered <- deaths_data %>%
    filter(WeekNumber == week, Year == year, Gender == gender)
  
  # Filter the interpolated population data for the specified week, year, and gender
  interpolated_pop_filtered <- interpolated_pop_data %>%
    filter(WeekNumber == week, Year == year, Gender == gender)
  
  # Sum of ESP for all age groups
  esp_sum <- sum(standard_pop$Standard_Population)
  
  # Initialize the sum of the products of death rate and ESP for each age group
  product_sum <- 0
  
  # Loop through each age group in the standard population
  for (age in standard_pop$Age_Group) {
    # Filter deaths and interpolated population for the current age group
    deaths_age <- deaths_filtered %>%
      filter(Age_Group == age)
    
    interpolated_age <- interpolated_pop_filtered %>%
      filter(Age_Group == age)
    
    # Get the ESP for the current age group
    esp_age <- standard_pop %>%
      filter(Age_Group == age) %>%
      pull(Standard_Population)
    
    # Calculate the product of death rate and ESP for the current age group
    if (nrow(deaths_age) > 0 && nrow(interpolated_age) > 0 && length(esp_age) > 0) {
      product_sum <- product_sum + (deaths_age$Deaths / interpolated_age$TotPopulation) * esp_age
    }
  }
  
  # Calculate the final ASMR
  asmr <- (100000 / esp_sum) * product_sum
  return(asmr)
}


# Weekly ASMR Table in different years
# Initialize matrices for each gender with NA values
asmr_matrix <- matrix(NA, nrow = 52, ncol = 10,
                      dimnames = list(paste("Week", 1:52), 2012:2021))

asmr_matrix_male <- matrix(NA, nrow = 52, ncol = 10,
                           dimnames = list(paste("Week", 1:52), 2012:2021))

asmr_matrix_female <- matrix(NA, nrow = 52, ncol = 10,
                             dimnames = list(paste("Week", 1:52), 2012:2021))

# Loop through each year and week, and calculate the ASMR for male and female
for (year in 2012:2021) {
  for (week in 1:52) {
    
    # Calculate the ASMR for all
    interpolated_pop_all_year <- filter(interpolated_pop_all, Year == year)
    deaths_all_year <- filter(deaths_all_long, Year == year)
    asmr_value <- calculate_ASMR(week, year, "All", deaths_all_year, interpolated_pop_all_year, Standard_pop)
    asmr_matrix[week, as.character(year)] <- asmr_value
    
    # Calculate the ASMR for male
    interpolated_pop_male_year <- filter(interpolated_pop_male, Year == year)
    deaths_male_year <- filter(deaths_male_long, Year == year)
    asmr_value_male <- calculate_ASMR(week, year, "Male", deaths_male_year, interpolated_pop_male_year, Standard_pop)
    asmr_matrix_male[week, as.character(year)] <- asmr_value_male
    
    # Calculate the ASMR for female
    interpolated_pop_female_year <- filter(interpolated_pop_female, Year == year)
    deaths_female_year <- filter(deaths_female_long, Year == year)
    asmr_value_female <- calculate_ASMR(week, year, "Female", deaths_female_year, interpolated_pop_female_year, Standard_pop)
    asmr_matrix_female[week, as.character(year)] <- asmr_value_female
  }
}

asmr_df <- as.data.frame(asmr_matrix)
asmr_df_male <- as.data.frame(asmr_matrix_male)
asmr_df_female <- as.data.frame(asmr_matrix_female)


# Create a WeekNumber column from the row names
asmr_df$WeekNumber <- as.integer(gsub("Week ", "", rownames(asmr_df)))
asmr_df_male$WeekNumber <- as.integer(gsub("Week ", "", rownames(asmr_df_male)))
asmr_df_female$WeekNumber <- as.integer(gsub("Week ", "", rownames(asmr_df_female)))


# rASMR
# Convert wide data frames to long format
long_asmr_all <- pivot_longer(asmr_df, cols = -WeekNumber, names_to = "Year", values_to = "ASMR")
long_asmr_male <- pivot_longer(asmr_df_male, cols = -WeekNumber, names_to = "Year", values_to = "ASMR")
long_asmr_female <- pivot_longer(asmr_df_female, cols = -WeekNumber, names_to = "Year", values_to = "ASMR")


# Function to calculate the relative ASMR (rASMR)
calculate_rASMR <- function(asmr_data, start_year, end_year) {
  # Loop through the years and calculate rASMR
  rasmr_df <- data.frame()
  
  for (current_year in start_year:end_year) {
    # Determine the reference years based on the condition
    if (current_year == 2021) {
      reference_years <- 2015:2019
    } else {
      reference_years <- (current_year - 5):(current_year - 1)
    }
    
    # Calculate weekly mean ASMR for reference years
    weekly_mean_asmr <- asmr_data %>%
      filter(Year %in% reference_years) %>%
      group_by(WeekNumber) %>%
      summarise(Mean_ASMR = mean(ASMR, na.rm = TRUE), .groups = 'drop')
    
    # Filter the ASMR data for the current year
    yearly_data <- asmr_data %>% filter(Year == current_year)
    
    # Join the mean ASMR with the yearly data
    yearly_data <- left_join(yearly_data, weekly_mean_asmr, by = "WeekNumber")
    
    # Calculate rASMR
    yearly_data <- yearly_data %>%
      mutate(rASMR = (ASMR - Mean_ASMR) / Mean_ASMR)
    
    # Combine with the overall rASMR data frame
    rasmr_df <- rbind(rasmr_df, yearly_data)
  }
  
  return(rasmr_df)
}


# Apply the function to calculate rASMR for the years 2017 to 2021
rASMR_all <- calculate_rASMR(long_asmr_all, 2017, 2021)
rASMR_female <- calculate_rASMR(long_asmr_female, 2017, 2021)
rASMR_male <- calculate_rASMR(long_asmr_male, 2017, 2021)

# Add a 'Gender' column to each dataset
rASMR_all$Gender <- 'All'
rASMR_female$Gender <- 'Female'
rASMR_male$Gender <- 'Male'

# Bind the datasets together (Gender)
rASMR_combined <- rbind(rASMR_all, rASMR_female, rASMR_male)


# Filter the data for each year and create plots
rASMR_plots <- list()
for(year in 2017:2021) {
  # Filter the data for the specific year
  yearly_data <- rASMR_combined %>% filter(Year == year)
  
  # Create the plot for the year with the legend for Gender
  rASMR_plots[[as.character(year)]] <- ggplot(yearly_data, aes(x = WeekNumber, y = rASMR, color = Gender, group = Gender)) +
    geom_line() + # Using a line plot
    geom_point() + # Add points to the line
    labs(title = paste("rASMR for Year", year),
         x = "Week",
         y = "rASMR") +
    scale_color_manual(values = c("All" = "blue", "Female" = "red", "Male" = "green")) + 
    theme_minimal() # Using a minimal theme for a nice look
}



# Cumulative rASMR， rcASMR 
# The rcASMR is equal to (cumulative ASMR - the average of past five years' CASMR) / the average of cASMR for past five years. 
# But for year 2021, when calculating the average, it will still use the cASMR for past 2015 to 2019; 
# others will stay same, such as for year 2020, it will use the the average of past 2015 to 2019. 


# cASMR
# Convert the matrix to a data frame and make sure week information is included
asmr_df1 <- as.data.frame(asmr_df)
week_numbers <- 1:nrow(asmr_df1)
df_with_weeks <- cbind(Week = week_numbers,asmr_df1)

# Convert the data frame to long format
asmr_long <- pivot_longer(
  df_with_weeks,
  cols = -Week,
  names_to = "Year",
  values_to = "ASMR"
) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year)),
    Week = as.numeric(Week)
  )

# Calculate cumulative ASMR for each year
cASMR_all <- asmr_long %>%
  filter(Year >= 2012 & Year <= 2021) %>% # Use '&' for logical 'and', and add missing pipe operator '%>%'
  group_by(Year) %>%
  arrange(Year, Week) %>%
  mutate(Cumulative_ASMR = cumsum(ASMR)) %>%
  ungroup()


# Plot the cumulative ASMR for each year
ggplot(cASMR_all, aes(x = Week, y = Cumulative_ASMR, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Cumulative ASMR by Week and Year",
       x = "Week",
       y = "Cumulative ASMR",
       color = "Year") +
  theme_minimal()




# rcASMR
# Calculate the average of the past five years' cASMR for each week
average_past_five_years <- function(year, cumulative_asmr_df) {
  if (year <= 2019) {
    previous_years <- (year - 5):(year - 1)
  } else {
    # For 2020 and 2021, use 2015-2019 due to the specifics of the calculation
    previous_years <- 2015:2019
  }
  cumulative_asmr_df %>%
    filter(Year %in% previous_years) %>%
    group_by(Week) %>%
    summarize(Average_Cumulative_ASMR = mean(Cumulative_ASMR, na.rm = TRUE)) %>%
    ungroup()
}


calculate_weekly_cumulative_rASMR <- function(cumulative_asmr_df, start_year, end_year) {
  map_dfr(start_year:end_year, function(year) {
    average_cASMR_df <- average_past_five_years(year, cumulative_asmr_df)
    
    yearly_data <- cumulative_asmr_df %>%
      filter(Year == year) %>%
      left_join(average_cASMR_df, by = "Week") %>%
      mutate(
        rASMR = (Cumulative_ASMR - Average_Cumulative_ASMR) / Average_Cumulative_ASMR,
        cumulative_rASMR = cumsum(rASMR)
      ) %>%
      select(Year, Week, rASMR, cumulative_rASMR)
    
    return(yearly_data)
  })
}

# Apply the function to calculate weekly cumulative rASMR for the years 2017 to 2021
rcASMR_all <- calculate_weekly_cumulative_rASMR(cASMR_all, 2017, 2021)

# Print the resulting data frames
print(rcASMR_all)


# Calculate the weekly three-year average baseline
EngWal_three_year_weekly_avg <- rcASMR_all %>%
  filter(Year >= 2018 & Year <= 2020) %>%
  group_by(Week) %>%
  summarise(Mean_ASMR_3yr_avg = mean(rASMR, na.rm = TRUE)) %>%
  ungroup() # Remove grouping

rASMR_EngWal_baseline <- ggplot(rcASMR_all, aes(x = Week, y = rASMR, group = as.factor(Year), color = as.factor(Year))) +
  geom_line() +
  geom_line(data = EngWal_three_year_weekly_avg, aes(y = Mean_ASMR_3yr_avg, group = 1), color = "black", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("2018" = "green", "2019" = "blue", "2020" = "red"), 
                     labels = c("2018", "2019", "2020"),
                     name = "Year") +
  scale_linetype_manual(values = c("dashed"), labels = c("Three-Year Average rASMR in England and Wales")) +
  labs(title = "Relative ASMR from 2018 to 2020 with Weekly Three-Year Average Baseline",
       x = "Week Number",
       y = "Relative ASMR") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(rASMR_EngWal_baseline)

# Convert 'Year' to a factor to ensure it's treated as a discrete variable in the plot
rcASMR_all$Year <- as.factor(rcASMR_all$Year)

ggplot(rcASMR_all, aes(x = Week, y = cumulative_rASMR, group = Year, color = Year)) +
  geom_line() + # Use geom_line for line plots
  labs(title = "Weekly Cumulative rASMR Trend from 2017 to 2021",
       x = "Week Number",
       y = "Cumulative rASMR",
       color = "Year") +
  theme_minimal() + # A minimal theme for a cleaner look
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability


############################ Northern Ireland No Gender Difference #########################
NI_pop <- read_excel("Population_Yearly_Combine.xlsx")
NIStandard_pop <- read_excel("Standard_Pop.xlsx")
NI_pop <- NI_pop %>% filter(Location == "NI")

NI_death <- read_excel("Weekly_Deaths_NI.xlsx", sheet = "Table_1", skip = 4)

NI_death$Year <- as.integer(NI_death$Year)

# Identify the columns that represent age groups
NI_age_group_cols <- setdiff(names(NI_death), c("Year", "WeekNumber", "All ages"))

# Identify the years with 53 weeks
NI_years_with_53_weeks <- NI_death %>%
  filter(WeekNumber == 53) %>%
  .$Year

# For each year with 53 weeks, average the data for week 52 and week 1 of the following year
for (year in NI_years_with_53_weeks) {
  # Ensure the year is numeric
  year <- as.numeric(year)
  
  # Select the data for week 52 of the current year
  week_52_data <- NI_death %>%
    filter(Year == year & WeekNumber == 52) %>%
    select(all_of(NI_age_group_cols))
  
  # Select the data for week 1 of the following year
  week_1_next_year_data <- NI_death %>%
    filter(Year == (year + 1) & WeekNumber == 1) %>%
    select(all_of(NI_age_group_cols))
  
  # Skip if there is no week 1 data for the next year
  if (nrow(week_1_next_year_data) == 0) {
    next
  }

  # Calculate the average for each age group column
  averaged_data <- (week_52_data + week_1_next_year_data) / 2

  # Update the week 52 data with the averaged values
  NI_death <- NI_death %>%
    mutate(across(all_of(NI_age_group_cols), ~ if_else(Year == year & WeekNumber == 52, averaged_data[[cur_column()]], .)))
}

# Remove the rows for week 53 in the years with 53 weeks
NI_death <- NI_death %>%
  filter(!(Year %in% NI_years_with_53_weeks & WeekNumber == 53))

# Add a new column for the total deaths by summing across the age group columns
NI_death <- NI_death %>%
  mutate(Total_Deaths = rowSums(select(., all_of(NI_age_group_cols)), na.rm = TRUE))

# Reorder columns to move Total_Deaths next to All ages
NI_death <- NI_death %>%
  select(Year, WeekNumber, `All ages`, Total_Deaths, everything())


NI_pop <- NI_pop %>%
  pivot_longer(
    cols = -c(Year, Location, Gender), 
    names_to = "Age_Group",
    values_to = "TotPopulation"
  ) %>%
  mutate(
    Gender = str_replace(Gender, "Combine", "All"), 
    Year = as.numeric(Year) # Make sure Year is numeric
  )

NI_pop_all <- NI_pop %>%
  filter(Gender == "All")


rASMR_deaths_data <- function(deaths_data, gender) {
  deaths_data %>%
    mutate(Year = as.integer(Year), WeekNumber = as.integer(WeekNumber)) %>%
    # Exclude 'Total_Deaths' from being pivoted and ensure numeric columns are converted to character before pivoting
    mutate(across(where(is.numeric), as.character)) %>%
    select(-`All ages`, -`Total_Deaths`) %>% # 
    # Pivot longer, excluding 'All ages' and 'Total_Deaths' from pivoting
    pivot_longer(
      cols = -c(Year, WeekNumber),
      names_to = "Age_Group",
      values_to = "Deaths"
    ) %>%
    
    mutate(
      Deaths = as.numeric(Deaths),
      Gender = gender
    )
}


deaths_all_NI <- rASMR_deaths_data(NI_death, "All")

NI_interpolate_population <- function(population_data) {
  # Create an empty data frame to store interpolated results
  interpolated_results <- data.frame()
  
  # Get the list of years and age groups
  years <- unique(population_data$Year)
  age_groups <- unique(population_data$Age_Group)
  genders <- unique(population_data$Gender)
  
  # Loop over each age group and year
  for(age_group in age_groups) {
    for(year in years) {
      if (year == min(years) || year == max(years)) {
        next
      }
      
      # Get the population for the previous, current, and next year
      pop_prev = population_data$TotPopulation[population_data$Year == year-1 & population_data$Age_Group == age_group]
      pop_curr = population_data$TotPopulation[population_data$Year == year & population_data$Age_Group == age_group]
      pop_next = population_data$TotPopulation[population_data$Year == year+1 & population_data$Age_Group == age_group]
      
      # Interpolate the population for each week
      for(week in 1:52) {
        if(week <= 26) {
          # For weeks in the first half of the year, interpolate between the previous and current year
          interpolated_pop = ((26 - week) / 26) * pop_prev + (week / 26) * pop_curr
        } else {
          # For weeks in the second half of the year, interpolate between the current and next year
          interpolated_pop = ((52 - week) / 26) * pop_curr + ((week - 26) / 26) * pop_next
        }
        
        # Add the interpolated data to the results data frame
        interpolated_results <- rbind(interpolated_results, data.frame(Year = year, Age_Group = age_group, Gender = genders, WeekNumber = week, TotPopulation = interpolated_pop))
      }
    }
  }
  
  return(interpolated_results)
}

# Apply the interpolation function to the population data
NI_interpolated_pop_all <- NI_interpolate_population(NI_pop_all)


NI_calculate_ASMR <- function(week, year, deaths_data, interpolated_pop_data, standard_pop) {
  
  # Filter the deaths data for the specified week, year, and gender
  deaths_filtered <- deaths_data %>%
    filter(WeekNumber == week, Year == year)
  
  # Filter the interpolated population data for the specified week, year, and gender
  interpolated_pop_filtered <- interpolated_pop_data %>%
    filter(WeekNumber == week, Year == year)
  
  # Sum of ESP for all age groups
  esp_sum <- sum(standard_pop$Standard_Population)
  
  # Initialize the sum of the products of death rate and ESP for each age group
  product_sum <- 0
  
  # Loop through each age group in the standard population
  for (age in standard_pop$Age_Group) {
    # Filter deaths and interpolated population for the current age group
    deaths_age <- deaths_filtered %>%
      filter(Age_Group == age)
    
    interpolated_age <- interpolated_pop_filtered %>%
      filter(Age_Group == age)
    
    # Get the ESP for the current age group
    esp_age <- standard_pop %>%
      filter(Age_Group == age) %>%
      pull(Standard_Population)
    
    # Calculate the product of death rate and ESP for the current age group
    if (nrow(deaths_age) > 0 && nrow(interpolated_age) > 0 && length(esp_age) > 0) {
      product_sum <- product_sum + (deaths_age$Deaths / interpolated_age$TotPopulation) * esp_age
    }
  }
  
  # Calculate the final ASMR
  asmr <- (100000 / esp_sum) * product_sum
  return(asmr)
}



# Initialize matrices for each gender with NA values
NI_asmr_matrix <- matrix(NA, nrow = 52, ncol = 10,
                         dimnames = list(paste("Week", 1:52), 2012:2021))

# Loop through each year and week, and calculate the ASMR for male and female
for (year in 2012:2021) {
  for (week in 1:52) {
    
    # Calculate the ASMR for all
    NI_interpolated_pop_all_year <- filter(NI_interpolated_pop_all, Year == year)
    NI_deaths_all_year <- filter(deaths_all_NI, Year == year)
    NI_asmr_value <- NI_calculate_ASMR(week, year, deaths_all_NI, NI_interpolated_pop_all, NIStandard_pop)
    NI_asmr_matrix[week, as.character(year)] <- NI_asmr_value
  }
}

asmr_NI <- as.data.frame(NI_asmr_matrix)



NI_week_numbers <- 1:nrow(asmr_NI)
NI_df_with_weeks <- cbind(Week = NI_week_numbers,asmr_NI)

# Convert the data frame to long format
NI_asmr_long <- pivot_longer(
  NI_df_with_weeks,
  cols = -Week,
  names_to = "Year",
  values_to = "ASMR"
) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year)),
    Week = as.numeric(Week)
  ) %>%
  filter(Year >= 2013 & Year <= 2020) # Select only the data for years 2013 to 2020

NI_asmr_long

# Calculate cumulative ASMR for each year
NI_cASMR_all <- NI_asmr_long %>%
  group_by(Year) %>%
  arrange(Year, Week) %>%
  mutate(Cumulative_ASMR = cumsum(ASMR)) %>%
  ungroup()

NI_cASMR_all

# Plot the cumulative ASMR for each year
ggplot(NI_cASMR_all, aes(x = Week, y = Cumulative_ASMR, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Cumulative ASMR by Week and Year",
       x = "Week",
       y = "Cumulative ASMR",
       color = "Year") +
  theme_minimal()





# rASMR
asmr_NI$WeekNumber <- as.integer(gsub("Week ", "", rownames(asmr_NI)))

# Convert wide data frames to long format
NI_long_asmr_all <- pivot_longer(asmr_NI, cols = -WeekNumber, names_to = "Year", values_to = "ASMR")
NI_long_asmr_all <- NI_long_asmr_all %>%
  filter(Year >= 2013 & Year <= 2020)

# Function to calculate the relative ASMR (rASMR)
calculate_rASMR <- function(asmr_data, start_year, end_year) {
  # Loop through the years and calculate rASMR
  rasmr_df <- data.frame()
  
  for (current_year in start_year:end_year) {
    # Determine the reference years based on the condition
    reference_years <- (current_year - 5):(current_year - 1)
    
    
    # Calculate weekly mean ASMR for reference years
    weekly_mean_asmr <- asmr_data %>%
      filter(Year %in% reference_years) %>%
      group_by(WeekNumber) %>%
      summarise(Mean_ASMR = mean(ASMR, na.rm = TRUE), .groups = 'drop')
    
    # Filter the ASMR data for the current year
    yearly_data <- asmr_data %>% filter(Year == current_year)
    
    # Join the mean ASMR with the yearly data
    yearly_data <- left_join(yearly_data, weekly_mean_asmr, by = "WeekNumber")
    
    # Calculate rASMR
    yearly_data <- yearly_data %>%
      mutate(rASMR = (ASMR - Mean_ASMR) / Mean_ASMR)
    
    # Combine with the overall rASMR data frame
    rasmr_df <- rbind(rasmr_df, yearly_data)
  }
  
  return(rasmr_df)
}


# Apply the function to calculate rASMR for the years 2018 to 2020
NI_rASMR_all <- calculate_rASMR(NI_long_asmr_all, 2018, 2020)

print(NI_rASMR_all)




# Calculate the weekly three-year average baseline
three_year_weekly_avg <- NI_rASMR_all %>%
  filter(Year >= 2018 & Year <= 2020) %>%
  group_by(WeekNumber) %>%
  summarise(Mean_ASMR_3yr_avg = mean(rASMR, na.rm = TRUE)) %>%
  ungroup() # Remove grouping

# Plot the data
ggplot(NI_rASMR_all, aes(x = WeekNumber, y = rASMR, group = Year, color = Year)) +
  geom_line() +
  # Use geom_line instead of geom_hline for the baseline to include it in the legend
  geom_line(data = three_year_weekly_avg, aes(y = Mean_ASMR_3yr_avg, group = 1), color = "black", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("2018" = "green", "2019" = "blue", "2020" = "Red")) +
  scale_linetype_manual(values = c("dashed"), labels = c("Three-Year Average rASMR")) +
  labs(title = "Relative ASMR from 2018 to 2020 with Weekly Three-Year Average Baseline",
       x = "Week Number",
       y = "Relative ASMR") +
  theme_minimal() +
  theme(legend.position = "bottom")






# rcASMR
NI_average_years <- function(year, cumulative_asmr_df) {
  previous_years <- (year - 5):(year - 1)
  cumulative_asmr_df %>%
    filter(Year %in% previous_years) %>%
    group_by(Week) %>%
    summarize(Average_Cumulative_ASMR = mean(Cumulative_ASMR, na.rm = TRUE)) %>%
    ungroup()
}

# Function to calculate weekly cumulative rASMR for a given year range
NI_calculate_weekly_cumulative_rASMR <- function(cumulative_asmr_df, start_year, end_year) {
  map_dfr(start_year:end_year, function(year) {
    average_cASMR_df <- NI_average_years(year, cumulative_asmr_df)
    
    yearly_data <- cumulative_asmr_df %>%
      filter(Year == year) %>%
      left_join(average_cASMR_df, by = "Week") %>%
      mutate(
        rASMR = (Cumulative_ASMR - Average_Cumulative_ASMR) / Average_Cumulative_ASMR,
        cumulative_rASMR = cumsum(rASMR)
      ) %>%
      select(Year, Week, rASMR, cumulative_rASMR)
    
    return(yearly_data)
  })
}


NI_rcASMR_all <- NI_calculate_weekly_cumulative_rASMR(NI_cASMR_all, 2020, 2020)

print(NI_rcASMR_all)


ggplot(NI_rcASMR_all, aes(x = Week, y = rASMR)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Trend of Relative ASMR Over Weeks in 2020",
       x = "Week Number",
       y = "Relative ASMR") +
  theme_minimal() + # Minimal theme
  theme(plot.title = element_text(hjust = 0.5))


# Comparison 
# Filter the EngWal data for the year 2020
EngWal_rASMR_2020 <- rASMR_all %>%
  filter(Year == 2020) %>%
  mutate(Region = "EngWal") %>%
  rename(Week = WeekNumber) %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Year, Week, rASMR, Region)

# Ensure the NI data has a Region column for consistency
NI_rASMR_all_2020 <- NI_rcASMR_all %>%
  mutate(Region = "NI",
         Year = as.numeric(Year)) %>%
  select(Year, Week, rASMR, Region)

# Combine the two datasets
combined_region_2020 <- bind_rows(EngWal_rASMR_2020, NI_rASMR_all_2020)

rASMR_comparison_plot <- ggplot(combined_region_2020, aes(x = Week, y = rASMR, color = Region)) +
  geom_line() + 
  labs(title = "Comparison of rASMR Trends in 2020: NI vs. EngWal",
       x = "Week Number",
       y = "Relative ASMR",
       color = "Region") +
  scale_color_manual(values = c("NI" = "blue", "EngWal" = "red")) + 
  theme_minimal() + # Minimal theme
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") 

print(rASMR_comparison_plot)









