install.packages("tidyverse")
install.packages("geosphere")
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(dplyr) #helps clean data
library(tidyr) #helps clean data
library(geosphere)
library("dplyr")

# Read the trip data from May 2020 to April 2021 (12 months)
tripdata_2020_05 <- read.csv("202005-divvy-tripdata.csv")
tripdata_2020_06 <- read.csv("202006-divvy-tripdata.csv")
tripdata_2020_07 <- read.csv("202007-divvy-tripdata.csv")
tripdata_2020_08 <- read.csv("202008-divvy-tripdata.csv")
tripdata_2020_09 <- read.csv("202009-divvy-tripdata.csv")
tripdata_2020_10 <- read.csv("202010-divvy-tripdata.csv")
tripdata_2020_11 <- read.csv("202011-divvy-tripdata.csv")
tripdata_2020_12 <- read.csv("202012-divvy-tripdata.csv")
tripdata_2021_01 <- read.csv("202101-divvy-tripdata.csv")
tripdata_2021_02 <- read.csv("202102-divvy-tripdata.csv")
tripdata_2021_03 <- read.csv("202103-divvy-tripdata.csv")
tripdata_2021_04 <- read.csv("202104-divvy-tripdata.csv")

#DATA CHECK 
colnames(tripdata_2020_05)
colnames(tripdata_2020_06)
colnames(tripdata_2020_07)
colnames(tripdata_2020_08)
colnames(tripdata_2020_09)
colnames(tripdata_2020_10)
colnames(tripdata_2020_11)
colnames(tripdata_2020_12)
colnames(tripdata_2021_01)
colnames(tripdata_2021_02)
colnames(tripdata_2021_03)
colnames(tripdata_2021_04)
# Confirmed none of column name should be changed
#DATA CHECK 
str(tripdata_2020_05)
str(tripdata_2020_06)
str(tripdata_2020_07)
str(tripdata_2020_08)
str(tripdata_2020_09)
str(tripdata_2020_10)
str(tripdata_2020_11)
str(tripdata_2020_12)
str(tripdata_2021_01)
str(tripdata_2021_02)
str(tripdata_2021_03)
str(tripdata_2021_04)
# Confirmed

#Covert some data types (from Double to Character) to merge
tripdata_2020_05 <-  mutate(tripdata_2020_05, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_06 <-  mutate(tripdata_2020_06, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_07 <-  mutate(tripdata_2020_07, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_08 <-  mutate(tripdata_2020_08, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_09 <-  mutate(tripdata_2020_09, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_10 <-  mutate(tripdata_2020_10, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_2020_11 <-  mutate(tripdata_2020_11, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

#Combine all the data sets
all_trips <- bind_rows(tripdata_2020_05, 
                       tripdata_2020_06, 
                       tripdata_2020_07, 
                       tripdata_2020_08, 
                       tripdata_2020_09, 
                       tripdata_2020_10, 
                       tripdata_2020_11, 
                       tripdata_2020_12, 
                       tripdata_2021_01, 
                       tripdata_2021_02, 
                       tripdata_2021_03,
                       tripdata_2021_04)
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.

# Add columns that list the date, month, day, and year of each ride as we might need to aggregate ride data for each month, day, or year.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Confirmed additional columns have been added
colnames(all_trips)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Convert "ride_length" from Double to numeric so we can run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
#Create a new data frame without records that have ride length <= zero minute OR > 1440 minutes
all_trips_v2 <- all_trips[!(all_trips$ride_length <= 0 | all_trips$ride_length > 1440),]


#Check the new data frame
dim(all_trips_v2) 
View(all_trips_v2)
summary(all_trips_v2)
#Confirmed the total number of rows is 2,664,700 (down from 3,850,936 - decreased by 30% - significant but necessary removals.
#1,241 NAs Remains in end_lat and end_long

#Drop all NAs
all_trips_v2 <- na.omit(all_trips_v2)
dim(all_trips_v2) 
summary(all_trips_v2)
# Confirmed all the NA is gone.


#Remove duplicated ID (confirmed different ride_id is assigned for every rides even if same rider uses this service):
all_trips_v3 <- all_trips_v2[!duplicated(all_trips_v2$ride_id),]
dim(all_trips_v3) 
# Confirmed 2,563,234 remained (Original data has 3,850,936 >>> Valid)


#Find out the distance for each ride:
all_trips_v3$ride_distance <- distGeo(matrix(c(all_trips_v3$start_lng, all_trips_v3$start_lat), ncol = 2), matrix(c(all_trips_v3$end_lng, all_trips_v3$end_lat), ncol = 2))
View(all_trips_v3)
summary(all_trips_v3)


#Assign the correct order to each day of the week
all_trips_v3$day_of_week <- 
  ordered(all_trips_v3$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))


all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(day_of_week)

#Assign the correct order to each month of the year
all_trips_v3$month <-
  ordered(all_trips_v3$month, levels = c('05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03', '04'))


all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(month)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + 
            all_trips_v3$day_of_week, FUN=mean)

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop') %>%
  arrange(month)

all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(day_of_week)

all_trips_v3 %>%
  group_by(member_casual, month) %>%
  summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
  arrange(month)

all_trips_v3 %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n() , .groups = 'drop')

all_trips_v3 %>%
  group_by(member_casual) %>%
  filter(ride_distance < 1) %>%
  summarize(number_of_rides = n() , .groups = 'drop')


all_trips_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity")
