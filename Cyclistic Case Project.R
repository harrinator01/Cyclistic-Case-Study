# Cyclistic Trip Data Analysis
# Name: Harrison Chambers
# Date: January 2022

## Before running code:
# Data retrieved from website:
# https://d3c33hcgiwev3.cloudfront.net/aacF81H_TsWnBfNR_x7FIg_36299b28fa0c4a5aba836111daad12f1_DAC8-Case-Study-1.pdf?Expires=1640390400&Signature=TAk4RsGIFHpi4YGSJKJSWZAp7~QoPf3BqtcAeV-PHRcSLF7S7pHWF8snqljecUOuFCS1exKIno5TzYlDP~QOBxYyxJyXUdgV0lYawLTZ-8h4OS6sCy9jPwcRc7DzY96hu2RzVaGwcW6bEWaJaNx~Wte8uMAzDV4ktLC5ktmT~k8_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A
# took data from months 2020-12 to 2021-11 for a complete year of data
# above link will not work - see data in attached folder

# Install and load packages:
install.packages("tidyverse")
library(tidyverse)
library(skimr)
library(lubridate)
library(scales)

# Loading data
# Make sure to set the working directory to where the csv files are, and that there aren't any other csv files in the directory
cycle_csv_files = list.files(path = ".", pattern = "csv$") # Create a list of the csv files
cycle_data = map_dfr(cycle_csv_files, read_csv) # Map the csv files into one data frame

## Data Cleaning
cycle_data <- mutate(cycle_data, ride_length = as.numeric(difftime(ended_at, started_at, units = c("auto")))) # add a column for trip length 
skim_without_charts(cycle_data) # skim to inspect the data

## Here I noticed a few things:
# ride_length should never be negative. Without more context it's unclear why. These results will be discarded. This casts doubt on my opinion of the data quality.
# all instances of ride_id are unique; this is good.
# a significant (1.9 - 2.8%) portion of station name and station id data is missing. This is likely due to the usage of some dockless bicycles.
# station name and station id data vary in the number of unique data points. Unclear why, and also casts doubt on data quality.
# one significant outlier in the end longitude data to be discarded; ride_id = 9F438AD0AB380E3F. Location outside of service range.
## Filter out bad data as described above:
cycle_data_clean <- cycle_data %>%
  filter(ride_length > 0) %>%
  filter(ride_id != "9F438AD0AB380E3F")

cycle_data_clean <- mutate(cycle_data_clean, weekday = wday(started_at, label = TRUE, abbr = TRUE, week_start = getOption("lubridate.week.start", 7))) # Add a variable for day of week based on "started_at"
cycle_data_clean$member_casual[cycle_data_clean$member_casual == "member"] <- "annual" # Rename elements of "casual_member"; improves clarity and will simplify visualizations

cycle_data_clean$rideable_type[cycle_data_clean$rideable_type == "classic_bike"] <- "Classic bike" 
cycle_data_clean$rideable_type[cycle_data_clean$rideable_type == "docked_bike"] <- "Docked bike"
cycle_data_clean$rideable_type[cycle_data_clean$rideable_type == "electric_bike"] <- "Electric bike" # Rename variables in "rideable_type"

cycle_data_clean <- mutate(cycle_data_clean, month = month(started_at, label = TRUE, abbr = TRUE)) # Add a variable for month, where January = 1 and December = 12, based on "started_at"
cycle_data_clean <- mutate(cycle_data_clean, hour = hour(started_at)) # Add a variable for hour of the day based on "started_at"
cycle_data_clean <- mutate(cycle_data_clean, ride_length_min = ride_length / 60) # Add a variable for trip duration in minutes

## Data Preparation
# Creating summary tables to simplify complex analyses
ride_length_by_day <- cycle_data_clean %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, weekday) # Comparing ride_length against membership status and day of week respectively
ride_length_by_month <- cycle_data_clean %>% 
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, month) # Comparing ride_length against membership status and month of year respectively
ride_length_by_hour <- cycle_data_clean %>% 
  group_by(member_casual, hour) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, hour) # Comparing ride_length against membership status and hour of day respectively
ride_length_by_membership <- cycle_data_clean %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length_min)) # Comparing ride_length against membership status

## Analysis
# This section is rather brief since most of the analysis is being done in the visualizations

ride_length_by_membership # provides summary numbers for average trip duration & # of trips for annual and casual members

## Visualizations
# Plotting and saving visualizations comparing total bike usage and average trip duration for different members

# Average Trip Duration by Hour of day
ggplot(data = ride_length_by_hour) + 
  geom_col(mapping = aes(x = hour, y = average_duration, fill = member_casual), position = "dodge") + 
  labs(x = "Hour of Day", y = "Average Trip Duration, minutes", title = "Average Trip Duration by Hour of Day, Annual and Casual Members") +
  guides(fill = guide_legend(title = "Membership Type")) +
  scale_x_continuous(breaks = seq(0, 23, by = 3))
ggsave("duration_hour.png", width = 700, height = 700, units = "px", dpi = 90)

# Number of Yearly Trips by Hour of Day
ggplot(data = ride_length_by_hour) + 
  geom_col(mapping = aes(x = hour, y = number_of_rides, fill = member_casual), position = "dodge") + 
  labs(x = "Hour of Day", y = "Number of Rides", title = "Number of Yearly Rides by Hour of Day, Annual and Casual Members") + 
  scale_y_continuous(labels = comma) +
  guides(fill = guide_legend(title = "Membership Type")) +
  scale_x_continuous(breaks = seq(0, 23, by = 3))
ggsave("quantity_hour.png", width = 700, height = 700, units = "px", dpi = 90)

# Number of Yearly Trips by Day of Week
ggplot(data = cycle_data_clean) + 
  geom_bar(mapping = aes(x = weekday, fill = member_casual), position = "dodge") + 
  labs(x = "Day of Week", y = "Number of Rides", title = "Number of Rides Yearly by Day of Week") + 
  scale_y_continuous(labels = comma) + 
  guides(fill = guide_legend(title = "Membership Type"))
ggsave("quantity_week.png", width = 700, height = 700, units = "px", dpi = 90)

# Number of Monthly Trips
ggplot(data = ride_length_by_month) + 
  geom_col(mapping = aes(x = month, y = number_of_rides, fill = member_casual), position = "dodge") + 
  scale_y_continuous(labels = comma) + 
  labs(x = "Month", y = "Number of Rides", title = "Number of Rides by Month, Annual and Casual Members") +
  guides(fill = guide_legend(title = "Membership Type"))
ggsave("quantity_month.png", width = 700, height = 700, units = "px", dpi = 90)

# Average Trip Duration by Month
ggplot(data = ride_length_by_month) + 
  geom_col(mapping = aes(x = month, y = average_duration, fill = member_casual), position = "dodge") + 
  labs(x = "Month", y = "Average Ride Time, minutes", title = "Average Ride Time by Month, Casual and Annual Members") +
  guides(fill = guide_legend(title = "Membership Type"))
ggsave("duration_month.png", width = 700, height = 700, units = "px", dpi = 90)

# Number of Trips by Bicycle Type
ggplot(data = cycle_data_clean) + 
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual), position = "dodge") + 
  labs(x = "Type of Bike Ride", y = "Number of Rides", title = "Number of Yearly Rides by Bike Type, Annual and Casual Members") +
  guides(fill = guide_legend(title = "Membership Type"))
ggsave("quantity_biketype.png", width = 700, height = 700, units = "px", dpi = 90)

## Findings
# Annual and Casual members use Cyclystic bicycles in different ways. Chiefly:
# - annual members are more likely to use classic bikes and less likely to use docked bikes
# - annual members have weekly and hourly usage patterns that suggest they use the bikes to commute more than casual members
# - annual members take significantly shorter trips, averaging 12 minutes to the casual member's 30-minute ride

# In general, it appears that annual members are more likely to use the bikes for 'utility' trips.
# That would be commuting, short trips to a friend's place, to a shop, appointment, etc.
# In contrast, casual members are likely using the bikes for exercise and enjoyment.

# One way to leverage this information is to advertise the utility of bikes for short trips to casual members.
# Bicycles need not be solely a leisure activity. They can be used for general mobility and have advantages over walking, transit, and automobiles.
# A further study leveraging the latitudinal and longitudinal data may reveal routes most often used for longer trips by casual members.
# This could help to target advertising based on geolocation and time through digital billboard space or by the use of online digital media.