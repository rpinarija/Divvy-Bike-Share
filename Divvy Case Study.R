setwd("C:/Users/pinar/OneDrive/Documents/Google Data Analytics Certificate/Course 8 - Google Analytics Capstone - Complete a Case Study/Case Study")
library(readr)
cyclistic_2022_01 <- read_csv("202201-divvy-tripdata.csv")
cyclistic_2022_02 <- read_csv("202202-divvy-tripdata.csv")
cyclistic_2022_03 <- read_csv("202203-divvy-tripdata.csv")
cyclistic_2022_04 <- read_csv("202204-divvy-tripdata.csv")
cyclistic_2022_05 <- read_csv("202205-divvy-tripdata.csv")
cyclistic_2022_06 <- read_csv("202206-divvy-tripdata.csv")
cyclistic_2022_07 <- read_csv("202207-divvy-tripdata.csv")
cyclistic_2022_08 <- read_csv("202208-divvy-tripdata.csv")
cyclistic_2022_09 <- read_csv("202209-divvy-publictripdata.csv")
cyclistic_2022_10 <- read_csv("202210-divvy-tripdata.csv")
cyclistic_2022_11 <- read_csv("202211-divvy-tripdata.csv")
cyclistic_2022_12 <- read_csv("202212-divvy-tripdata.csv")
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

# Column data types check
compare_df_cols(cyclistic_2022_01, cyclistic_2022_02, cyclistic_2022_03,  cyclistic_2022_04, cyclistic_2022_05, cyclistic_2022_06, cyclistic_2022_07, cyclistic_2022_08, cyclistic_2022_09, cyclistic_2022_10, cyclistic_2022_11, cyclistic_2022_12)

# Merge datasets
cyclistic_2022 <- rbind(cyclistic_2022_01, cyclistic_2022_02, cyclistic_2022_03,  cyclistic_2022_04, cyclistic_2022_05, cyclistic_2022_06, cyclistic_2022_07, cyclistic_2022_08, cyclistic_2022_09, cyclistic_2022_10, cyclistic_2022_11, cyclistic_2022_12)


### Adding additional columns ###
# Ride Length
cyclistic_2022$ride_length <- as.numeric(difftime(cyclistic_2022$ended_at, cyclistic_2022$started_at, units = "mins"))

# Day of the week
cyclistic_2022$day_of_week <- wday(cyclistic_2022$started_at, label = TRUE, week_start = 1)

# Month of the year
cyclistic_2022$month <- month(cyclistic_2022$started_at, label = TRUE)


## Summary Statistics ##
skim_without_charts(cyclistic_2022)
# Based on the skim_without_charts we can notice a few things right away. The start and end station name and 
# id's have NA values, end longitude and latitude have minimum values of 0 as well as outliers.

cyclistic_2022 %>% 
  filter(is.na(end_lat)) %>% 
  count(start_station_name, start_station_id, end_station_name, end_station_id, end_lng)

# Print csv of grouped station names and id's to check for spelling errors.
name_check <- cyclistic_2022 %>% 
  group_by(start_station_id, start_station_name) %>% 
  count(start_station_name)
write.csv(name_check, "cyclistic_name_check.csv")

# Remove NA's, rides under a minute and over 24 hours, parameterize longitude and latitude, 
# and remove white space from station names.
cyclistic_2022 <- cyclistic_2022 %>% 
  filter(start_lat > 41.5 & start_lat < 42.3,
         start_lng > -88.1 & start_lng < -87.5,
         end_lat > 41.5 & end_lat < 42.3,
         end_lng > -88.1 & end_lng < -87.5,
         ride_length >= 1 & ride_length <= 1440) %>% 
  mutate(start_station_name = str_trim(start_station_name),
         end_station_name = str_trim(end_station_name),
         start_station_name = str_replace(start_station_name, "\\([:alpha:]*\\)", ""),
         end_station_name = str_replace(end_station_name, "\\([:alpha:]*\\)", ""),
         start_station_name = str_replace(start_station_name,"\\*",""),
         end_station_name = str_replace(end_station_name,"\\*",""),
         start_station_name = str_replace(start_station_name, "&amp;", ""),
         end_station_name = str_replace(end_station_name, "&amp;", ""))

# Additional summary statistics check 
skim_without_charts(cyclistic_2022)

# Check to see how the number are dispersed for key variables
table(cyclistic_2022$rideable_type)
table(cyclistic_2022$member_casual)
table(cyclistic_2022$day_of_week)
table(cyclistic_2022$month)

# Start station NA's amongst member/casual and rideable type
cyclistic_2022 %>% 
  filter(is.na(start_station_name)) %>% 
  count(member_casual, rideable_type)

# End station NA's amongst member/casual and rideable type
cyclistic_2022 %>% 
  filter(is.na(end_station_name)) %>% 
  count(member_casual, rideable_type)

# Both start and end station NA's amongst amongst member/casual and rideable type
cyclistic_2022 %>% 
  filter(is.na(start_station_name),
         is.na(end_station_name)) %>% 
  group_by(start_station_name, end_station_name) %>% 
  count(member_casual, rideable_type)

### Descriptive statstics ### 
# Member vs casual riders
cyclistic_2022 %>% 
  group_by(member_casual) %>% 
  summarise(ride_length_frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Descriptive statstics by rideable type
cyclistic_2022 %>% 
  group_by(rideable_type) %>% 
  summarise(ride_length_frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Descriptive statstics of Ride length by month for members
cyclistic_2022 %>% 
  filter(member_casual == "member") %>% 
  group_by(month) %>% 
  summarise(Frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Descriptive statstics of Ride length by month for casuals
cyclistic_2022 %>% 
  filter(member_casual == "casual") %>% 
  group_by(month) %>% 
  summarise(Frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Descriptive statstics of Ride length by rideable type for members
cyclistic_2022 %>% 
  filter(member_casual == "member") %>% 
  group_by(rideable_type) %>% 
  summarise(Frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Descriptive statstics of Ride length by rideable type for casuals
cyclistic_2022 %>% 
  filter(member_casual == "casual") %>% 
  group_by(rideable_type) %>% 
  summarise(ride_length_frequency = n(),
            Average = mean(ride_length),
            Median = median(ride_length),
            Standard_Deviation = sd(ride_length))

# Popular trips
cyclistic_2022 %>% 
  group_by(member_casual, start_station_name, end_station_name) %>% 
  summarise(Frequency = n()) %>% 
  arrange(desc(Frequency)) %>% 
  print(n = 40)

# Popular start stations for members
cyclistic_2022 %>% 
  filter(member_casual == "member",
         !is.na(start_station_name)) %>% 
  group_by(start_station_name) %>% 
  summarise(Frequency = n()) %>% 
  arrange(desc(Frequency)) %>% 
  print(n = 30)

# Popular start stations for casuals
cyclistic_2022 %>% 
  filter(member_casual == "casual",
         !is.na(start_station_name)) %>% 
  group_by(start_station_name) %>% 
  summarise(Frequency = n()) %>% 
  arrange(desc(Frequency)) %>% 
  print(n = 30)

# Popular end stations for members
cyclistic_2022 %>% 
  filter(member_casual == "member",
         !is.na(end_station_name)) %>% 
  group_by(end_station_name) %>% 
  summarise(Frequency = n()) %>% 
  arrange(desc(Frequency)) %>% 
  print(n = 30)

# Popular end stations for casuals
cyclistic_2022 %>% 
  filter(member_casual == "casual",
         !is.na(end_station_name)) %>% 
  group_by(end_station_name) %>% 
  summarise(Frequency = n()) %>% 
  arrange(desc(Frequency)) %>% 
  print(n = 30)

### Plots ###

# Ride frequency by day of the week between members and casual riders
cyclistic_2022 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(Frequency = n()) %>%
  ggplot(aes(x = day_of_week,y = Frequency, fill = member_casual)) +
  geom_col(position = "dodge")

# Ride frequency by day of the week and month between members and casual riders
cyclistic_2022 %>% 
  group_by(member_casual, day_of_week, month) %>% 
  summarise(Frequency = n()) %>%
  ggplot(aes(x = day_of_week, y = Frequency, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=1)) +
  facet_wrap(~month)

# Ride length by month between members and casual riders
ggplot(cyclistic_2022, aes(x = ride_length, y = ..count.., fill = member_casual)) +
  geom_density(alpha = 0.7) +
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120), limits = c(0,120)) +
  facet_wrap(~month)

# Proportion of rideable type between members and casual riders
ggplot(cyclistic_2022, aes(x = member_casual, fill = rideable_type)) +
  geom_bar(position = "fill")

# Ride Frequency by month between members and casual riders
cyclistic_2022 %>% 
  group_by(member_casual, month) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(x = month, y = Frequency, fill = member_casual)) +
  geom_col(position = "dodge")

# Ride Frequency by hour of the day between members and casual riders
cyclistic_2022 %>% 
  mutate(Hour = hour(started_at)) %>% 
  group_by(member_casual, Hour) %>% 
  summarise(Frequency = n()) %>%
  ggplot(aes(x = Hour, y = Frequency, color = member_casual)) +
  geom_line()

# Ride Frequency by hour of the day and month between members and casual riders
cyclistic_2022 %>% 
  mutate(Hour = hour(started_at)) %>% 
  group_by(member_casual, Hour, month) %>% 
  summarise(Frequency = n()) %>%
  ggplot(aes(x = Hour, y = Frequency, color = member_casual)) +
  geom_line() +
  facet_wrap(~month)

# Ride frequency by month and rideable type (excluding docked bikes)
cyclistic_2022 %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(rideable_type, month) %>%
  summarise(Frequency = n()) %>%
  ggplot(aes(x = month, y = Frequency, fill = rideable_type)) +
  geom_col(position = "dodge")

# Ride frequency by month and rideable type for members
cyclistic_2022 %>% 
  filter(member_casual == "member") %>% 
  group_by(rideable_type, month) %>%
  summarise(Frequency = n()) %>%
  ggplot(aes(x = month, y = Frequency, fill = rideable_type)) +
  geom_col(position = "dodge")

# Ride frequency by month and rideable type for casuals (excluding docked bikes)
cyclistic_2022 %>% 
  filter(member_casual == "casual", 
         rideable_type != "docked_bike") %>% 
  group_by(rideable_type, month) %>%
  summarise(Frequency = n()) %>%
  ggplot(aes(x = month, y = Frequency, fill = rideable_type)) +
  geom_col(position = "dodge")

